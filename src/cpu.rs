use crate::{Io, Mem};
use std::fmt;

pub struct Cpu {
    pub pc: u16,
    cycle: u32,
    mem: Box<dyn Io>,
    regs: Regs,
    ime: bool,
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "PC {:04X}", self.pc)?;
        writeln!(f, "Instr {:02X}", self.mem.read_u8(self.pc))?;
        writeln!(f, "{:?}", self.regs)?;
        writeln!(f, "Cycles: {:?}", self.cycle)
    }
}
impl Cpu {
    pub fn new(mem: Box<dyn Io>) -> Self {
        Self {
            pc: 0,
            mem,
            cycle: 0,
            regs: Regs::new(),
            ime: false,
        }
    }
    pub fn step(&mut self) {
        use Reg16::*;
        use Reg8::*;
        let last_pc = self.pc;

        /* utility macros */
        macro_rules! update_flags {
            ($z:expr, $n:expr, $h:expr, $c:expr) => {
                self.regs
                    .write(F, ($z << 7) | ($n << 6) | ($h << 5) | ($c << 4));
            };
            ($mask:expr, $z:expr, $n:expr, $h:expr, $c:expr) => {
                self.regs.write(
                    F,
                    ($z << 7)
                        | ($n << 6)
                        | ($h << 5)
                        | ($c << 4)
                        | (self.regs.read(F) & (!($mask << 4))),
                );
            };
        }
        /* instruction macros */
        macro_rules! ld {
            ($dst:expr, n) => {{
                //8 bit immediate
                self.regs.write($dst, self.mem.read_u8(self.pc + 1));
                self.pc += 2;
                self.cycle += 2;
            }};
            ($dst:expr, nn) => {{
                //16 bit immediate
                self.regs.write($dst, self.mem.read_u16(self.pc + 1));
                self.pc += 3;
                self.cycle += 3;
            }};
            ($dst:expr, %$src:expr) => {{
                //reg to reg
                self.regs.write($dst, self.regs.read($src));
                self.pc += 1;
                self.cycle += 2;
            }};
            ($dst:expr, %$src:expr, n) => {{
                //HL <- SP+n
                self.regs.write(
                    $dst,
                    self.regs.read($src) + self.mem.read_u8(self.pc + 1) as u16,
                );
                self.pc += 2;
                self.cycle += 3;
            }};
            ($dst:expr, $src:expr) => {{
                //reg to reg
                self.regs.write($dst, self.regs.read($src));
                self.pc += 1;
                self.cycle += 1;
            }};
            ($dst:expr, @nn) => {{
                //(nn) to reg
                self.regs
                    .write($dst, self.mem.read_u8(self.mem.read_u16(self.pc + 1)));
                self.pc += 3;
                self.cycle += 4;
            }};
            ($dst:expr, @$src:expr) => {{
                //(HL) to reg
                self.regs
                    .write($dst, self.mem.read_u8(self.regs.read($src)));
                self.pc += 1;
                self.cycle += 2;
            }};
            (@$dst:expr, n) => {{
                //n to (HL)
                self.mem
                    .write_u8(self.regs.read($dst), self.mem.read_u8(self.pc + 1));
                self.pc += 2;
                self.cycle += 3;
            }};
            (@nn, %$src:expr) => {{
                //16 bit reg to (nn)
                self.mem
                    .write_u16(self.mem.read_u16(self.pc + 1), self.regs.read($src));
                self.pc += 3;
                self.cycle += 5;
            }};
            (@nn, $src:expr) => {{
                //reg to (nn)
                self.mem
                    .write_u8(self.mem.read_u16(self.pc + 1), self.regs.read($src));
                self.pc += 3;
                self.cycle += 4;
            }};
            (@$dst:expr, $src:expr) => {{
                //reg to (HL)
                self.mem
                    .write_u8(self.regs.read($dst), self.regs.read($src));
                self.pc += 1;
                self.cycle += 2;
            }};
            (@$dst:expr, $src:expr, $val:expr) => {{
                //reg to (dst) reg+= val
                self.mem
                    .write_u8(self.regs.read($dst), self.regs.read($src));
                self.regs
                    .write($dst, self.regs.read($dst).wrapping_add($val));
                self.pc += 1;
                self.cycle += 2;
            }};
            ($dst:expr, @$src:expr, $val:expr) => {{
                //(reg) to dst reg, reg+= val (ldi A,(HL))
                self.regs
                    .write($dst, self.mem.read_u8(self.regs.read($src)));
                self.regs
                    .write($src, self.regs.read($src).wrapping_add($val));
                self.pc += 1;
                self.cycle += 2;
            }};
            (>n, $src:expr) => {{
                //@(n+0xff00) = A
                self.mem.write_u8(
                    self.mem.read_u8(self.pc + 1) as u16 + 0xFF00,
                    self.regs.read($src),
                );
                self.pc += 2;
                self.cycle += 3;
            }};
            (>$dst:expr, $src:expr) => {{
                //@(reg+0xff00) = A
                self.mem
                    .write_u8(self.regs.read($dst) as u16 + 0xFF00, self.regs.read($src));
                self.pc += 1;
                self.cycle += 2;
            }};
            ($dst:expr, >n) => {{
                //A=@(n+0xff00)
                self.regs.write(
                    $dst,
                    self.mem
                        .read_u8(self.mem.read_u8(self.pc + 1) as u16 + 0xFF00),
                );
                self.pc += 2;
                self.cycle += 3;
            }};
            ($dst:expr, >$src:expr) => {{
                //A=@(reg+0xff00)
                self.regs
                    .write($dst, self.mem.read_u8(self.regs.read($src) as u16 + 0xFF00));
                self.pc += 1;
                self.cycle += 2;
            }};
        }
        macro_rules! _jr {
            ($f:expr) => {
                if $f {
                    let val = self.mem.read_u8(self.pc + 1) as i8;
                    self.pc = self.pc.wrapping_add(val as u16).wrapping_add(2);
                    self.cycle += 3;
                } else {
                    self.pc += 2;
                    self.cycle += 2;
                }
            };
        }
        macro_rules! NZ {
            () => {
                (self.regs.read(F) & 0x80) == 0
            };
        }
        macro_rules! NC {
            () => {
                (self.regs.read(F) & 0x10) == 0
            };
        }
        macro_rules! jr {
            (NZ) => {
                _jr!(NZ!())
            };
            (Z) => {
                _jr!(!NZ!())
            };
            (NC) => {
                _jr!(NC!())
            };
            (C) => {
                _jr!(!NC!())
            };
            () => {
                _jr!(true)
            };
        }
        macro_rules! _jp {
            ($f:expr) => {
                if $f {
                    let val = self.mem.read_u16(self.pc + 1);
                    self.pc = val;
                    self.cycle += 4;
                } else {
                    self.pc += 3;
                    self.cycle += 3;
                }
            };
        }
        macro_rules! jp {
            (NZ) => {
                _jp!(NZ!())
            };
            (Z) => {
                _jp!(!NZ!())
            };
            (NC) => {
                _jp!(NC!())
            };
            (C) => {
                _jp!(!NC!())
            };
            () => {
                _jp!(true)
            };
            ($reg:expr) => {{
                self.cycle += 1;
                self.pc = self.regs.read($reg);
            }};
        }
        macro_rules! _call {
            ($f:expr) => {
                if $f {
                    let dst = self.mem.read_u16(self.pc + 1);
                    let sp = self.regs.read(SP).wrapping_add(0xFFFE); //-2
                    self.regs.write(SP, sp);
                    self.mem.write_u16(sp, self.pc + 3);
                    self.pc = dst;
                    self.cycle += 6; //not sure about this
                } else {
                    self.pc += 3;
                    self.cycle += 3;
                }
            };
        }
        macro_rules! call {
            (NZ) => {
                _call!(NZ!())
            };
            (Z) => {
                _call!(!NZ!())
            };
            (NC) => {
                _call!(NC!())
            };
            (C) => {
                _call!(!NC!())
            };
            () => {
                _call!(true)
            };
        }
        macro_rules! _ret {
            ($f:expr, $cyc:expr) => {{
                if $f {
                    let sp = self.regs.read(SP);
                    self.pc = self.mem.read_u16(sp);
                    self.regs.write(SP, sp.wrapping_add(2));
                    self.cycle += $cyc;
                } else {
                    self.pc += 1;
                    self.cycle += 2;
                }
            }};
        }
        macro_rules! ret {
            (NZ) => {
                _ret!(NZ!(), 5)
            };
            (Z) => {
                _ret!(!NZ!(), 5)
            };
            (NC) => {
                _ret!(NC!(), 5)
            };
            (C) => {
                _ret!(!NC!(), 5)
            };
            () => {
                _ret!(true, 4)
            };
        }
        macro_rules! push {
            ($reg:expr) => {{
                let val = self.regs.read($reg);
                let sp = self.regs.read(SP).wrapping_add(0xFFFE); //-2
                self.regs.write(SP, sp);
                self.mem.write_u16(sp, val);
                self.pc += 1;
                self.cycle += 4;
            }};
        }
        macro_rules! pop {
            ($reg:expr) => {{
                let sp = self.regs.read(SP);
                let val = self.mem.read_u16(sp);
                self.regs.write(SP, sp.wrapping_add(2));
                self.regs.write($reg, val);
                self.pc += 1;
                self.cycle += 3;
            }};
        }
        macro_rules! math_op {
            ($name:ident, $lval:ident, $rval:ident, $body:stmt) => {
                macro_rules! $name {
                                    (n) => {{
                                        let $lval = self.regs.read(A);
                                        let $rval = self.mem.read_u8(self.pc +1);
                                        $body
                                        self.pc += 2;
                                        self.cycle += 2;
                                    }};
                                    (@$src:expr) => {{
                                        let $lval = self.regs.read(A);
                                        let $rval = self.mem.read_u8(self.regs.read($src));
                                        $body
                                        self.pc += 1;
                                        self.cycle += 2;
                                    }};
                                    ($src:expr) => {{
                                        let $lval = self.regs.read(A);
                                        let $rval = self.regs.read($src);
                                        $body
                                        self.pc += 1;
                                        self.cycle += 1;
                                    }};
                                }
            };
        }
        macro_rules! math_op_unary {
            ($name:ident, $lval:ident, $body:stmt) => {
                macro_rules! $name {
                                    (@$src:expr) => {{
                                        let mut $lval = self.mem.read_u8(self.regs.read($src));
                                        $body
                                        self.mem.write_u8(self.regs.read($src), $lval);
                                        self.pc += 1;
                                        self.cycle += 3;
                                    }};
                                    ($src:expr) => {{
                                        let mut $lval = self.regs.read($src);
                                        $body
                                        self.regs.write($src, $lval);
                                        self.pc += 1;
                                        self.cycle += 1;
                                    }};
                                }
            };
        }
        math_op!(add, a, b, {
            {
                let res = a.wrapping_add(b);
                update_flags!(
                    (res == 0) as u8,
                    0,
                    ((a & 0xf + b & 0xf) & 0x10) >> 4,
                    (((a as i32 + b as i32) & 0x100) >> 8) as u8
                );
                self.regs.write(A, res);
            }
        });
        math_op!(adc, a, b, {
            {
                let carry = (self.regs.read(F) >> 4) & 1;
                let res = a.wrapping_add(b).wrapping_add(carry);
                update_flags!(
                    (res == 0) as u8,
                    0,
                    ((a & 0xf + b & 0xf + carry) & 0x10) >> 4,
                    (((a as i32 + b as i32 + carry as i32) & 0x100) >> 8) as u8
                );
                self.regs.write(A, res);
            }
        });
        math_op!(sub, a, b, {
            {
                let res = a.wrapping_sub(b);
                update_flags!(
                    (res == 0) as u8,
                    1,
                    ((((a as i32) & 0xf - (b as i32) & 0xf) & 0x10) >> 4) as u8,
                    (((a as i32 - b as i32) & 0x100) >> 8) as u8
                );
                self.regs.write(A, res);
            }
        });
        math_op!(cp, a, b, {
            {
                let res = a.wrapping_sub(b);
                update_flags!(
                    (res == 0) as u8,
                    1,
                    ((((a as i32) & 0xf - (b as i32) & 0xf) & 0x10) >> 4) as u8,
                    (((a as i32 - b as i32) & 0x100) >> 8) as u8
                );
            }
        });
        math_op!(sbc, a, b, {
            {
                let carry = (self.regs.read(F) >> 4) & 1;
                let res = a.wrapping_sub(b.wrapping_add(carry));
                update_flags!(
                    (res == 0) as u8,
                    1,
                    ((a & 0xf - b & 0xf - carry) & 0x10) >> 4,
                    (((a as i32 - b as i32 - carry as i32) & 0x100) >> 8) as u8
                );
                self.regs.write(A, res);
            }
        });
        math_op!(xor, a, b, {
            let res = a ^ b;
            update_flags!((res == 0) as u8, 0, 0, 0);
            self.regs.write(A, res);
        });
        math_op!(and, a, b, {
            let res = a & b;
            update_flags!((res == 0) as u8, 0, 1, 0);
            self.regs.write(A, res);
        });
        math_op!(or, a, b, {
            let res = a | b;
            update_flags!((res == 0) as u8, 0, 0, 0);
            self.regs.write(A, res);
        });
        macro_rules! bit {
            (@$reg:expr, $bit:expr) => {{
                let res = self.mem.read_u8(self.regs.read($reg));
                update_flags!(0b1110, if res & (1 << $bit) != 0 { 1 } else { 0 }, 0, 1, 0);
                self.cycle += 1;
                self.pc += 3; //TODO: not sure if this is correct
            }};
            ($reg:expr, $bit:expr) => {{
                let res = self.regs.read($reg);
                update_flags!(0b1110, if res & (1 << $bit) != 0 { 1 } else { 0 }, 0, 1, 0);
                self.cycle += 1;
                self.pc += 1;
            }};
        }
        macro_rules! set {
            (@$reg:expr, $bit:expr) => {{
                let res = self.mem.read_u8(self.regs.read($reg));
                let res = res | (1 << $bit);
                self.mem.write_u8(self.regs.read($reg), res);
                self.cycle += 1;
                self.pc += 3;
            }};
            ($reg:expr, $bit:expr) => {{
                let res = self.regs.read($reg);
                let res = res | (1 << $bit);
                self.regs.write($reg, res);
                self.cycle += 1;
                self.pc += 1;
            }};
        }
        macro_rules! res {
            (@$reg:expr, $bit:expr) => {{
                let res = self.mem.read_u8(self.regs.read($reg));
                let res = res & !(1 << $bit);
                self.mem.write_u8(self.regs.read($reg), res);
                self.cycle += 1;
                self.pc += 3;
            }};
            ($reg:expr, $bit:expr) => {{
                let res = self.regs.read($reg);
                let res = res & !(1 << $bit);
                self.regs.write($reg, res);
                self.cycle += 1;
                self.pc += 1;
            }};
        }

        macro_rules! inc {
            ($reg:expr) => {{
                let orig = self.regs.read($reg);
                let res = orig.wrapping_add(1);
                update_flags!(0b1110, (res == 0) as u8, 0, (orig & 0xF == 0xF) as u8, 0);
                self.regs.write($reg, res);
                self.cycle += 1;
                self.pc += 1;
            }};
            (@$reg:expr) => {{
                let dst = self.regs.read($reg);
                let orig = self.mem.read_u8(dst);
                let res = orig.wrapping_add(1);
                update_flags!(0b1110, (res == 0) as u8, 0, (orig & 0xF == 0xF) as u8, 0);
                self.mem.write_u8(dst, res);
                self.cycle += 3;
                self.pc += 1;
            }};
            (%$reg:expr) => {{
                let orig = self.regs.read($reg);
                self.regs.write($reg, orig.wrapping_add(1));
                self.cycle += 2;
                self.pc += 1;
            }};
        }
        macro_rules! dec {
            ($reg:expr) => {{
                let orig = self.regs.read($reg);
                let res = orig.wrapping_add(0xFF);
                update_flags!(0b1110, (res == 0) as u8, 1, (orig & 0xF0 == 0) as u8, 0);
                self.regs.write($reg, res);
                self.cycle += 1;
                self.pc += 1;
            }};
            (@$reg:expr) => {{
                let dst = self.regs.read($reg);
                let orig = self.mem.read_u8(dst);
                let res = orig.wrapping_add(0xFF);
                update_flags!(0b1110, (res == 0) as u8, 1, (orig & 0xF0 == 0) as u8, 0);
                self.mem.write_u8(dst, res);
                self.cycle += 3;
                self.pc += 1;
            }};
            (%$reg:expr) => {{
                let orig = self.regs.read($reg);
                self.regs.write($reg, orig.wrapping_add(0xFFFF));
                self.cycle += 2;
                self.pc += 1;
            }};
        }
        macro_rules! add16 {
            ($dst:expr, n) => {{
                let a = self.regs.read($dst) as i16;
                let b = self.mem.read_u8(self.pc + 1) as i8 as i16;
                let res = a.wrapping_add(b);
                if b >= 0 {
                    update_flags!(
                        0,
                        0,
                        (((a & 0xff + b & 0xff) & 0x100) >> 8) as u8,
                        (((a as i32 + b as i32) & 0x10000) >> 16) as u8
                    );
                } else {
                    update_flags!(
                        0,
                        0,
                        ((((a & 0xff) as i32 - (b & 0xff) as i32) & 0x100) >> 8) as u8,
                        (((a as i32 - b as i32) & 0x10000) >> 16) as u8
                    );
                }
                self.regs.write($dst, res as u16);
                self.cycle += 4;
                self.pc += 2;
            }};
            ($dst:expr, $src:expr) => {{
                let a = self.regs.read($dst);
                let b = self.regs.read($src);
                let res = a.wrapping_add(b);
                update_flags!(
                    0b0111,
                    0,
                    0,
                    (((a & 0xff + b & 0xff) & 0x100) >> 8) as u8,
                    (((a as i32 + b as i32) & 0x10000) >> 16) as u8
                );
                self.regs.write($dst, res);
                self.cycle += 2;
                self.pc += 1;
            }};
        }
        macro_rules! daa {
            () => {{
                let orig = self.regs.read(A);
                let flags = self.regs.read(F);
                let mut val = orig;
                let n = (flags & 0x40) != 0;
                let mut c = (flags & 0x10) != 0;
                let h = (flags & 0x20) != 0;
                if !n {
                    if c || val > 0x99 {
                        val = val.wrapping_add(0x60);
                        c = true;
                    }
                    if h || (val & 0xF) > 9 {
                        val = val.wrapping_add(1)
                    }
                } else {
                    if c {
                        val = val.wrapping_add(0xA0); //-0x60
                    }
                    if h {
                        val = val.wrapping_add(0xFA); //-0x6
                    }
                }
                update_flags!(0b1011, (val == 0) as u8, 0, 0, c as u8);
                self.regs.write(A, val);
                self.pc += 1;
                self.cycle += 1;
            }};
        }
        macro_rules! cpl {
            () => {{
                self.regs.write(A, !self.regs.read(A));
                update_flags!(0b0110, 0, 1, 1, 0);
                self.pc += 1;
                self.cycle += 1;
            }};
        }
        macro_rules! ccf {
            () => {{
                self.regs.write(F, (self.regs.read(F) ^ 0x10) & 0b10010000);
                self.pc += 1;
                self.cycle += 1;
            }};
        }
        macro_rules! scf {
            () => {{
                update_flags!(0b0111, 0, 0, 0, 1);
                self.pc += 1;
                self.cycle += 1;
            }};
        }
        macro_rules! nop {
            () => {{
                self.pc += 1;
                self.cycle += 1;
            }};
        }
        macro_rules! halt {
            () => {
                panic!("Halt not implemented");
            };
        }
        macro_rules! stop {
            () => {
                panic!("Stop not implemented");
            };
        }
        macro_rules! di {
            () => {{
                self.ime = false;
                self.pc += 1;
                self.cycle += 2;
            }};
        }
        macro_rules! ei {
            () => {
                panic!("ei not implemented");
            };
        }
        macro_rules! rst {
            ($dst:expr) => {{
                self.cycle += 8;
                self.pc = $dst;
            }};
        }
        macro_rules! reti {
            () => {
                panic!("Reti not implemented");
            };
        }

        math_op_unary!(rl, val, {
            let c = (self.regs.read(F) & 0x10) >> 4;
            let c_new = ((val & 0x80) != 0) as u8;
            val = (val << 1) | c;
            update_flags!((val == 0) as u8, 0, 0, c_new);
        });
        math_op_unary!(rlc, val, {
            let c_new = ((val & 0x80) != 0) as u8;
            val = (val << 1) | c_new;
            update_flags!((val == 0) as u8, 0, 0, c_new);
        });
        math_op_unary!(rr, val, {
            let c = (self.regs.read(F) & 0x10) >> 4;
            let c_new = ((val & 1) != 0) as u8;
            val = (val >> 1) | (c << 7);
            update_flags!((val == 0) as u8, 0, 0, c_new);
        });
        math_op_unary!(rrc, val, {
            let c_new = ((val & 1) != 0) as u8;
            val = (val >> 1) | (c_new << 7);
            update_flags!((val == 0) as u8, 0, 0, c_new);
        });
        math_op_unary!(sla, val, {
            let c_new = ((val & 0x80) != 0) as u8;
            val = val << 1;
            update_flags!((val == 0) as u8, 0, 0, c_new);
        });
        math_op_unary!(sra, val, {
            let c_new = ((val & 1) != 0) as u8;
            val = (val as i8 >> 1) as u8;
            update_flags!((val == 0) as u8, 0, 0, c_new);
        });
        math_op_unary!(srl, val, {
            let c_new = ((val & 0x1) != 0) as u8;
            val = val >> 1;
            update_flags!((val == 0) as u8, 0, 0, c_new);
        });
        math_op_unary!(swap, val, {
            val = (val & 0xF << 4) | (val >> 4);
            update_flags!((val == 0) as u8, 0, 0, 0);
        });

        let instr = self.mem.read_u8(self.pc);
        //> means 0xFF + val, @ means memory, % means 16 bit constant
        match instr {
            0x06 => ld!(B, n),
            0x0E => ld!(C, n),
            0x16 => ld!(D, n),
            0x1E => ld!(E, n),
            0x26 => ld!(H, n),
            0x2E => ld!(L, n),
            0x52 => ld!(D, D),
            0x53 => ld!(D, E),
            0x54 => ld!(D, H),
            0x55 => ld!(D, L),
            0x56 => ld!(D,@HL),
            0x58 => ld!(E, B),
            0x59 => ld!(E, C),
            0x5A => ld!(E, D),
            0x5B => ld!(E, E),
            0x5C => ld!(E, H),
            0x5D => ld!(E, L),
            0x5E => ld!(E,@HL),
            0x60 => ld!(H, B),
            0x61 => ld!(H, C),
            0x62 => ld!(H, D),
            0x63 => ld!(H, E),
            0x64 => ld!(H, H),
            0x65 => ld!(H, L),
            0x66 => ld!(H,@HL),
            0x68 => ld!(L, B),
            0x69 => ld!(L, C),
            0x6A => ld!(L, D),
            0x6B => ld!(L, E),
            0x6C => ld!(L, H),
            0x6D => ld!(L, L),
            0x6E => ld!(L,@HL),
            0x70 => ld!(@HL,B),
            0x71 => ld!(@HL,C),
            0x72 => ld!(@HL,D),
            0x73 => ld!(@HL,E),
            0x74 => ld!(@HL,H),
            0x75 => ld!(@HL,L),
            0x36 => ld!(@HL,n),
            0x7F => ld!(A, A),
            0x78 => ld!(A, B),
            0x79 => ld!(A, C),
            0x7A => ld!(A, D),
            0x7B => ld!(A, E),
            0x7C => ld!(A, H),
            0x7D => ld!(A, L),
            0x7E => ld!(A,@HL),
            0x40 => ld!(B, B),
            0x41 => ld!(B, C),
            0x42 => ld!(B, D),
            0x43 => ld!(B, E),
            0x44 => ld!(B, H),
            0x45 => ld!(B, L),
            0x46 => ld!(B,@HL),
            0x48 => ld!(C, B),
            0x49 => ld!(C, C),
            0x4A => ld!(C, D),
            0x4B => ld!(C, E),
            0x4C => ld!(C, H),
            0x4D => ld!(C, L),
            0x4E => ld!(C,@HL),
            0x50 => ld!(D, B),
            0x51 => ld!(D, C),

            //0x7F => ld!(A,A), //available above!
            //0x78 => ld!(A,B),
            //0x79 => ld!(A,C),
            //0x7A => ld!(A,D),
            //0x7B => ld!(A,E),
            //0x7C => ld!(A,H),
            //0x7D => ld!(A,L),
            0x0A => ld!(A,@BC),
            0x1A => ld!(A,@DE),
            //0x7E => ld!(A,@HL),
            0xFA => ld!(A,@nn),
            0x3E => ld!(A, n),

            0x47 => ld!(B, A),
            0x4F => ld!(C, A),
            0x57 => ld!(D, A),
            0x5F => ld!(E, A),
            0x67 => ld!(H, A),
            0x6F => ld!(L, A),
            0x02 => ld!(@BC,A),
            0x12 => ld!(@DE,A),
            0x77 => ld!(@HL,A),
            0xEA => ld!(@nn,A),

            0xF2 => ld!(A, >C),
            0xE2 => ld!(>C, A),
            0x3A => ld!(A, @HL, 0xFFFF), //-1 as u16
            0x32 => ld!(@HL, A, 0xFFFF),
            0x2A => ld!(A, @HL, 1),
            0x22 => ld!(@HL, A, 1),
            0xE0 => ld!(>n, A),
            0xF0 => ld!(A, >n),
            0x01 => ld!(BC, nn),
            0x11 => ld!(DE, nn),
            0x21 => ld!(HL, nn),
            0x31 => ld!(SP, nn),
            0xF9 => ld!(SP, %HL),
            0xF8 => ld!(HL, %SP, n),
            0x08 => ld!(@nn, %SP),
            0xF5 => push!(AF),
            0xC5 => push!(BC),
            0xD5 => push!(DE),
            0xE5 => push!(HL),

            0xF1 => pop!(AF),
            0xC1 => pop!(BC),
            0xD1 => pop!(DE),
            0xE1 => pop!(HL),

            0x87 => add!(A),
            0x80 => add!(B),
            0x81 => add!(C),
            0x82 => add!(D),
            0x83 => add!(E),
            0x84 => add!(H),
            0x85 => add!(L),
            0x86 => add!(@HL),
            0xC6 => add!(n),
            0x8F => adc!(A),
            0x88 => adc!(B),
            0x89 => adc!(C),
            0x8A => adc!(D),
            0x8B => adc!(E),
            0x8C => adc!(H),
            0x8D => adc!(L),
            0x8E => adc!(@HL),
            0xCE => adc!(n),

            0x97 => sub!(A),
            0x90 => sub!(B),
            0x91 => sub!(C),
            0x92 => sub!(D),
            0x93 => sub!(E),
            0x94 => sub!(H),
            0x95 => sub!(L),
            0x96 => sub!(@HL),
            0xD6 => sub!(n),
            0x9F => sbc!(A),
            0x98 => sbc!(B),
            0x99 => sbc!(C),
            0x9A => sbc!(D),
            0x9B => sbc!(E),
            0x9C => sbc!(H),
            0x9D => sbc!(L),
            0x9E => sbc!(@HL),
            0xDE => sbc!(n),

            0xA7 => and!(A),
            0xA0 => and!(B),
            0xA1 => and!(C),
            0xA2 => and!(D),
            0xA3 => and!(E),
            0xA4 => and!(H),
            0xA5 => and!(L),
            0xA6 => and!(@HL),
            0xE6 => and!(n),

            0xB7 => or!(A),
            0xB0 => or!(B),
            0xB1 => or!(C),
            0xB2 => or!(D),
            0xB3 => or!(E),
            0xB4 => or!(H),
            0xB5 => or!(L),
            0xB6 => or!(@HL),
            0xF6 => or!(n),
            0xAF => xor!(A),
            0xA8 => xor!(B),
            0xA9 => xor!(C),
            0xAA => xor!(D),
            0xAB => xor!(E),
            0xAC => xor!(H),
            0xAD => xor!(L),
            0xAE => xor!(@HL),
            0xEE => xor!(n),
            0xBF => cp!(A),
            0xB8 => cp!(B),
            0xB9 => cp!(C),
            0xBA => cp!(D),
            0xBB => cp!(E),
            0xBC => cp!(H),
            0xBD => cp!(L),
            0xBE => cp!(@HL),
            0xFE => cp!(n),

            0x3C => inc!(A),
            0x04 => inc!(B),
            0x0C => inc!(C),
            0x14 => inc!(D),
            0x1C => inc!(E),
            0x24 => inc!(H),
            0x2C => inc!(L),
            0x34 => inc!(@HL),

            0x3D => dec!(A),
            0x05 => dec!(B),
            0x0D => dec!(C),
            0x15 => dec!(D),
            0x1D => dec!(E),
            0x25 => dec!(H),
            0x2D => dec!(L),
            0x35 => dec!(@HL),

            0x09 => add16!(HL, BC),
            0x19 => add16!(HL, DE),
            0x29 => add16!(HL, HL),
            0x39 => add16!(HL, SP),

            0xE8 => add16!(SP, n),

            0x03 => inc!(%BC),
            0x13 => inc!(%DE),
            0x23 => inc!(%HL),
            0x33 => inc!(%SP),
            0x0B => dec!(%BC),
            0x1B => dec!(%DE),
            0x2B => dec!(%HL),
            0x3B => dec!(%SP),

            0x27 => daa!(),

            0x2F => cpl!(),
            0x3F => ccf!(),
            0x37 => scf!(),
            0x00 => nop!(),

            0x76 => halt!(),
            0x10 => stop!(),

            0xF3 => di!(),
            0xFB => ei!(),

            0x07 => rlc!(A),
            0x17 => rl!(A),
            0x0F => rrc!(A),
            0x1F => rr!(A),

            0xC3 => jp!(),
            0xC2 => jp!(NZ),
            0xCA => jp!(Z),
            0xD2 => jp!(NC),
            0xDA => jp!(C),
            0xE9 => jp!(HL),
            0x18 => jr!(),
            0x20 => jr!(NZ),
            0x28 => jr!(Z),
            0x30 => jr!(NC),
            0x38 => jr!(C),

            0xCD => call!(),
            0xC4 => call!(NZ),
            0xCC => call!(Z),
            0xD4 => call!(NC),
            0xDC => call!(C),

            0xC9 => ret!(),
            0xC0 => ret!(NZ),
            0xC8 => ret!(Z),
            0xD0 => ret!(NC),
            0xD8 => ret!(C),

            0xC7 => rst!(0x00),
            0xCF => rst!(0x08),
            0xD7 => rst!(0x10),
            0xDF => rst!(0x18),
            0xE7 => rst!(0x20),
            0xEF => rst!(0x28),
            0xF7 => rst!(0x30),
            0xFF => rst!(0x38),

            0xD9 => reti!(),
            /*0xAF => {
                self.regs
                    .write(Reg8::A, self.regs.read(Reg8::A) ^ self.regs.read(Reg8::A));
                self.pc += 1;
            }*/
            0xCB => {
                let instr = self.mem.read_u8(self.pc + 1);
                self.pc += 1;
                self.cycle += 1;
                match instr {
                    0x37 => swap!(A),
                    0x30 => swap!(B),
                    0x31 => swap!(C),
                    0x32 => swap!(D),
                    0x33 => swap!(E),
                    0x34 => swap!(H),
                    0x35 => swap!(L),
                    0x36 => swap!(@HL),
                    0x07 => rlc!(A),
                    0x00 => rlc!(B),
                    0x01 => rlc!(C),
                    0x02 => rlc!(D),
                    0x03 => rlc!(E),
                    0x04 => rlc!(H),
                    0x05 => rlc!(L),
                    0x06 => rlc!(@HL),
                    0x17 => rl!(A),
                    0x10 => rl!(B),
                    0x11 => rl!(C),
                    0x12 => rl!(D),
                    0x13 => rl!(E),
                    0x14 => rl!(H),
                    0x15 => rl!(L),
                    0x16 => rl!(@HL),
                    0x0F => rrc!(A),
                    0x08 => rrc!(B),
                    0x09 => rrc!(C),
                    0x0A => rrc!(D),
                    0x0B => rrc!(E),
                    0x0C => rrc!(H),
                    0x0D => rrc!(L),
                    0x0E => rrc!(@HL),
                    0x1F => rr!(A),
                    0x18 => rr!(B),
                    0x19 => rr!(C),
                    0x1A => rr!(D),
                    0x1B => rr!(E),
                    0x1C => rr!(H),
                    0x1D => rr!(L),
                    0x1E => rr!(@HL),
                    0x27 => sla!(A),
                    0x20 => sla!(B),
                    0x21 => sla!(C),
                    0x22 => sla!(D),
                    0x23 => sla!(E),
                    0x24 => sla!(H),
                    0x25 => sla!(L),
                    0x26 => sla!(@HL),
                    0x2F => sra!(A),
                    0x28 => sra!(B),
                    0x29 => sra!(C),
                    0x2A => sra!(D),
                    0x2B => sra!(E),
                    0x2C => sra!(H),
                    0x2D => sra!(L),
                    0x2E => sra!(@HL),
                    0x3F => srl!(A),
                    0x38 => srl!(B),
                    0x39 => srl!(C),
                    0x3A => srl!(D),
                    0x3B => srl!(E),
                    0x3C => srl!(H),
                    0x3D => srl!(L),
                    0x3E => srl!(@HL),

                    0x40 => bit!(B, 0),
                    0x41 => bit!(C, 0),
                    0x42 => bit!(D, 0),
                    0x43 => bit!(E, 0),
                    0x44 => bit!(H, 0),
                    0x45 => bit!(L, 0),
                    0x46 => bit!(@HL, 0),
                    0x47 => bit!(A, 0),
                    0x48 => bit!(B, 1),
                    0x49 => bit!(C, 1),
                    0x4A => bit!(D, 1),
                    0x4B => bit!(E, 1),
                    0x4C => bit!(H, 1),
                    0x4D => bit!(L, 1),
                    0x4E => bit!(@HL, 1),
                    0x4F => bit!(A, 1),

                    0x50 => bit!(B, 2),
                    0x51 => bit!(C, 2),
                    0x52 => bit!(D, 2),
                    0x53 => bit!(E, 2),
                    0x54 => bit!(H, 2),
                    0x55 => bit!(L, 2),
                    0x56 => bit!(@HL, 2),
                    0x57 => bit!(A, 2),
                    0x58 => bit!(B, 3),
                    0x59 => bit!(C, 3),
                    0x5A => bit!(D, 3),
                    0x5B => bit!(E, 3),
                    0x5C => bit!(H, 3),
                    0x5D => bit!(L, 3),
                    0x5E => bit!(@HL, 3),
                    0x5F => bit!(A, 3),

                    0x60 => bit!(B, 4),
                    0x61 => bit!(C, 4),
                    0x62 => bit!(D, 4),
                    0x63 => bit!(E, 4),
                    0x64 => bit!(H, 4),
                    0x65 => bit!(L, 4),
                    0x66 => bit!(@HL, 4),
                    0x67 => bit!(A, 4),
                    0x68 => bit!(B, 5),
                    0x69 => bit!(C, 5),
                    0x6A => bit!(D, 5),
                    0x6B => bit!(E, 5),
                    0x6C => bit!(H, 5),
                    0x6D => bit!(L, 5),
                    0x6E => bit!(@HL, 5),
                    0x6F => bit!(A, 5),

                    0x70 => bit!(B, 6),
                    0x71 => bit!(C, 6),
                    0x72 => bit!(D, 6),
                    0x73 => bit!(E, 6),
                    0x74 => bit!(H, 6),
                    0x75 => bit!(L, 6),
                    0x76 => bit!(@HL, 6),
                    0x77 => bit!(A, 6),
                    0x78 => bit!(B, 7),
                    0x79 => bit!(C, 7),
                    0x7A => bit!(D, 7),
                    0x7B => bit!(E, 7),
                    0x7C => bit!(H, 7),
                    0x7D => bit!(L, 7),
                    0x7E => bit!(@HL, 7),
                    0x7F => bit!(A, 7),

                    0x80 => res!(B, 0),
                    0x81 => res!(C, 0),
                    0x82 => res!(D, 0),
                    0x83 => res!(E, 0),
                    0x84 => res!(H, 0),
                    0x85 => res!(L, 0),
                    0x86 => res!(@HL, 0),
                    0x87 => res!(A, 0),
                    0x88 => res!(B, 1),
                    0x89 => res!(C, 1),
                    0x8A => res!(D, 1),
                    0x8B => res!(E, 1),
                    0x8C => res!(H, 1),
                    0x8D => res!(L, 1),
                    0x8E => res!(@HL, 1),
                    0x8F => res!(A, 1),

                    0x90 => res!(B, 2),
                    0x91 => res!(C, 2),
                    0x92 => res!(D, 2),
                    0x93 => res!(E, 2),
                    0x94 => res!(H, 2),
                    0x95 => res!(L, 2),
                    0x96 => res!(@HL, 2),
                    0x97 => res!(A, 2),
                    0x98 => res!(B, 3),
                    0x99 => res!(C, 3),
                    0x9A => res!(D, 3),
                    0x9B => res!(E, 3),
                    0x9C => res!(H, 3),
                    0x9D => res!(L, 3),
                    0x9E => res!(@HL, 3),
                    0x9F => res!(A, 3),

                    0xA0 => res!(B, 4),
                    0xA1 => res!(C, 4),
                    0xA2 => res!(D, 4),
                    0xA3 => res!(E, 4),
                    0xA4 => res!(H, 4),
                    0xA5 => res!(L, 4),
                    0xA6 => res!(@HL, 4),
                    0xA7 => res!(A, 4),
                    0xA8 => res!(B, 5),
                    0xA9 => res!(C, 5),
                    0xAA => res!(D, 5),
                    0xAB => res!(E, 5),
                    0xAC => res!(H, 5),
                    0xAD => res!(L, 5),
                    0xAE => res!(@HL, 5),
                    0xAF => res!(A, 5),

                    0xB0 => res!(B, 6),
                    0xB1 => res!(C, 6),
                    0xB2 => res!(D, 6),
                    0xB3 => res!(E, 6),
                    0xB4 => res!(H, 6),
                    0xB5 => res!(L, 6),
                    0xB6 => res!(@HL, 6),
                    0xB7 => res!(A, 6),
                    0xB8 => res!(B, 7),
                    0xB9 => res!(C, 7),
                    0xBA => res!(D, 7),
                    0xBB => res!(E, 7),
                    0xBC => res!(H, 7),
                    0xBD => res!(L, 7),
                    0xBE => res!(@HL, 7),
                    0xBF => res!(A, 7),

                    0xC0 => set!(B, 0),
                    0xC1 => set!(C, 0),
                    0xC2 => set!(D, 0),
                    0xC3 => set!(E, 0),
                    0xC4 => set!(H, 0),
                    0xC5 => set!(L, 0),
                    0xC6 => set!(@HL, 0),
                    0xC7 => set!(A, 0),
                    0xC8 => set!(B, 1),
                    0xC9 => set!(C, 1),
                    0xCA => set!(D, 1),
                    0xCB => set!(E, 1),
                    0xCC => set!(H, 1),
                    0xCD => set!(L, 1),
                    0xCE => set!(@HL, 1),
                    0xCF => set!(A, 1),

                    0xD0 => set!(B, 2),
                    0xD1 => set!(C, 2),
                    0xD2 => set!(D, 2),
                    0xD3 => set!(E, 2),
                    0xD4 => set!(H, 2),
                    0xD5 => set!(L, 2),
                    0xD6 => set!(@HL, 2),
                    0xD7 => set!(A, 2),
                    0xD8 => set!(B, 3),
                    0xD9 => set!(C, 3),
                    0xDA => set!(D, 3),
                    0xDB => set!(E, 3),
                    0xDC => set!(H, 3),
                    0xDD => set!(L, 3),
                    0xDE => set!(@HL, 3),
                    0xDF => set!(A, 3),

                    0xE0 => set!(B, 4),
                    0xE1 => set!(C, 4),
                    0xE2 => set!(D, 4),
                    0xE3 => set!(E, 4),
                    0xE4 => set!(H, 4),
                    0xE5 => set!(L, 4),
                    0xE6 => set!(@HL, 4),
                    0xE7 => set!(A, 4),
                    0xE8 => set!(B, 5),
                    0xE9 => set!(C, 5),
                    0xEA => set!(D, 5),
                    0xEB => set!(E, 5),
                    0xEC => set!(H, 5),
                    0xED => set!(L, 5),
                    0xEE => set!(@HL, 5),
                    0xEF => set!(A, 5),

                    0xF0 => set!(B, 6),
                    0xF1 => set!(C, 6),
                    0xF2 => set!(D, 6),
                    0xF3 => set!(E, 6),
                    0xF4 => set!(H, 6),
                    0xF5 => set!(L, 6),
                    0xF6 => set!(@HL, 6),
                    0xF7 => set!(A, 6),
                    0xF8 => set!(B, 7),
                    0xF9 => set!(C, 7),
                    0xFA => set!(D, 7),
                    0xFB => set!(E, 7),
                    0xFC => set!(H, 7),
                    0xFD => set!(L, 7),
                    0xFE => set!(@HL, 7),
                    0xFF => set!(A, 7),
                }
            }
            _ => panic!(
                "Unimplemented instruction {:X} at {:X}\n{:?}",
                instr, self.pc, self
            ),
        }
        if last_pc == self.pc {
            println!("{}", self.mem.read_u8(self.regs.read(HL)));
            panic!("infinite loop detected!");
        }
    }
}

struct Regs {
    file: [u8; 10],
}
impl Regs {
    fn new() -> Self {
        Self {
            file: [0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0, 0xDE, 0xAD],
        }
    }
}

impl fmt::Debug for Regs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "SP {:04X}", self.read(Reg16::SP))?;
        writeln!(f, "AF {:04X}", self.read(Reg16::AF))?;
        writeln!(f, "BC {:04X}", self.read(Reg16::BC))?;
        writeln!(f, "DE {:04X}", self.read(Reg16::DE))?;
        writeln!(f, "HL {:04X}", self.read(Reg16::HL))
    }
}

trait Reg<T, U> {
    fn write(&mut self, reg: T, value: U) -> U;
    fn read(&self, reg: T) -> U;
}

impl Reg<Reg8, u8> for Regs {
    fn write(&mut self, reg: Reg8, value: u8) -> u8 {
        self.file[reg as usize] = value;
        value
    }
    fn read(&self, reg: Reg8) -> u8 {
        self.file[reg as usize]
    }
}
impl Reg<Reg16, u16> for Regs {
    fn write(&mut self, reg: Reg16, value: u16) -> u16 {
        self.file[reg as usize] = (value >> 8) as u8;
        self.file[reg as usize + 1] = value as u8;
        value
    }
    fn read(&self, reg: Reg16) -> u16 {
        ((self.file[reg as usize] as u16) << 8) | (self.file[reg as usize + 1] as u16)
    }
}

#[repr(u8)]
#[derive(Copy, Clone)]
enum Reg8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    _S,
    _P,
}
#[repr(u8)]
#[derive(Copy, Clone)]
enum Reg16 {
    AF,
    BC = 2,
    DE = 4,
    HL = 6,
    SP = 8,
}
