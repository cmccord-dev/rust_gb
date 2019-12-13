use crate::Io;

pub struct Timer {
    tac: u8,
}

impl Io for Timer {
    fn read_u8(addr: u16) -> u8 {
        panic!("Timer read {:X}", addr);
    }
    fn write_u8(addr: u16, val: u8) {
        panic!("Timer read @{:X}=val", addr);
    }
}