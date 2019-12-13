use crate::Io;

#[derive(Default, Debug)]
pub struct Lcd {
    bgp: u8, //palettes
    obp0: u8,
    obp1: u8,
    scy: u8,
    lcdc: u8,
    ly: u8,
}

impl Lcd {
    pub fn new() -> Self {
        Self {
            ly: 0x90,
            ..Default::default()
        }
    }
}

impl Io for Lcd {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x40 => self.lcdc,
            0x42 => self.scy,
            0x44 => self.ly,
            0x47 => self.bgp,
            0x48 => self.obp0,
            0x49 => self.obp1,
            _ => panic!("Lcd: Invalid address {:X}", addr),
        }
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        if addr == 0x44 {
            panic!("0x44 {}", val);
        }
        match addr {
            0x40 => self.lcdc = val,
            0x42 => self.scy = val,
            0x44 => self.ly = val,
            0x47 => self.bgp = val,
            0x48 => self.obp0 = val,
            0x49 => self.obp1 = val,
            _ => panic!("Stores not supported for Lcd @{:X}={:X}", addr, val),
        }
    }
}
