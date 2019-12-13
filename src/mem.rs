use crate::{Audio, Lcd, Ram, Rom};

pub trait Io: std::fmt::Debug {
    fn read_u8(&self, addr: u16) -> u8;
    fn read_u16(&self, addr: u16) -> u16 {
        ((self.read_u8(addr + 1) as u16) << 8) | (self.read_u8(addr) as u16)
    }
    fn write_u8(&mut self, addr: u16, val: u8);
    fn write_u16(&mut self, addr: u16, val: u16) {
        self.write_u8(addr + 1, (val >> 8) as u8);
        self.write_u8(addr, val as u8);
    }
}

#[derive(Debug)]
pub struct Mem {
    mbc: Box<dyn Mbc>,
    vram: Ram,
    audio: Audio,
    lcd: Lcd,
    hram: Ram,
    boot: bool,
    bootrom: Rom,
    wram: Ram,
}
impl Io for Mem {
    fn read_u8(&self, addr: u16) -> u8 {
        if self.boot && addr < 0x100 {
            return self.bootrom.read_u8(addr as usize);
        }
        match (addr & 0xF000) >> 12 {
            0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 0xA | 0xB => self.mbc.read_u8(addr),
            8 | 9 => self.vram.read_u8(addr & 0xFFF),
            0xC | 0xD => self.wram.read_u8(addr & 0x1FFF),
            0xF => match addr & 0xFFF {
                0xF10..=0xF3F => self.audio.read_u8(addr & 0xFF),
                0xF40..=0xF49 => self.lcd.read_u8(addr & 0xFF),
                0xF80..=0xFFE => self.hram.read_u8(addr & 0x7F),
                _ => panic!("can't read from io @{:X}", addr),
            },
            _ => panic!("can't read from memory @{:X}", addr),
        }
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        match (addr & 0xF000) >> 12 {
            0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 0xA | 0xB => self.mbc.write_u8(addr, val),
            8 | 9 => self.vram.write_u8(addr & 0xFFF, val),
            0xC | 0xD => self.wram.write_u8(addr & 0x1FFF, val),
            0xF => match addr & 0xFFF {
                0xF10..=0xF3F => self.audio.write_u8(addr & 0xFF, val),
                0xF40..=0xF49 => self.lcd.write_u8(addr & 0xFF, val),
                0xF50 => self.boot = false,
                0xF80..=0xFFE => self.hram.write_u8(addr & 0x7F, val),
                _ => panic!("ignoring writes to io @{:X}={:X}", addr, val),
            },
            _ => panic!("Stores not supported @{:X}={:X}", addr, val),
        };
    }
}
impl Mem {
    pub fn new(mbc: Box<dyn Mbc>) -> Self {
        Self {
            mbc,
            vram: Ram::new(8 * 1024),
            audio: Audio::new(),
            lcd: Lcd::new(),
            hram: Ram::new(126),
            boot: true,
            bootrom: Rom::with_data(crate::loader::load_file("DMG_ROM.bin").unwrap()),
            wram: Ram::new(8 * 1024),
        }
    }
}

#[derive(Debug)]
enum RomRamSelect {
    Rom,
    Ram,
}
pub trait Mbc: Io {}
#[derive(Debug)]
pub struct Mbc1 {
    rom: Rom,
    rom_bank: usize,
    rom_bank_lower: u8,
    rom_bank_upper: u8,
    rom_ram_select: RomRamSelect,
}
impl Mbc for Mbc1 {}
impl Io for Mbc1 {
    fn read_u8(&self, addr: u16) -> u8 {
        match (addr & 0xF000) >> 12 {
            0 | 1 | 2 | 3 => self.rom.read_u8(addr as usize),
            4 | 5 | 6 | 7 => self.rom.read_u8((addr & 0xFFF) as usize | self.rom_bank),
            0xA | 0xB => panic!("RAM Bank not implemented"),
            _ => panic!("Invalid address {:X}", addr),
        }
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        /*match (addr & 0xF000) >> 12 {
            4 | 5 => {
                if let RomRamSelect::Rom = self.rom_ram_select {
                    self.rom_bank_upper = val & 0x3;
                }
            }
        }*/
        panic!("Stores not supported for Mbc1 @{:X}={:X}", addr, val)
    }
}
impl Mbc1 {
    pub fn new(rom: Rom) -> Self {
        Self {
            rom,
            rom_bank: 0x4000,
            rom_bank_lower: 1,
            rom_bank_upper: 0,
            rom_ram_select: RomRamSelect::Rom,
        }
    }
}
