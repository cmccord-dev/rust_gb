use crate::Io;

#[derive(Debug)]
pub struct Ram {
    data: Vec<u8>
}
impl Ram {
    pub fn new(size:usize)-> Self {
        Self {
            data: (0..size).map(|_| 0x33).collect()
        }
    }
}
impl Io for Ram {
    fn read_u8(&self, addr: u16) -> u8 {
        self.data[addr as usize]
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        self.data[addr as usize] = val;
    }
}