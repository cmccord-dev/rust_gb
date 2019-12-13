use std::fmt;

pub struct Rom {
    pub data: Vec<u8>
}

impl fmt::Debug for Rom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ROM")
    }
}

impl Rom {
    pub fn with_data(data: Vec<u8>) -> Self {
        Self { data }
    }
    /// read_str
    /// up to max_size
    pub fn read_str(&self, addr: usize, max_size: usize) -> String {
        self.data[addr..addr + max_size]
            .iter()
            .take_while(|&&x| x != 0)
            .map(|&c| c as char)
            .collect::<String>()
    }

    pub fn cartridge_type(&self) {
        let t = self.read_u8(0x147);
        assert_eq!(t, 1, "Must be MBC1");
    }
    pub fn name(&self) -> String {
        self.read_str(0x134, 16)
    }
    pub fn check_rom_size(&self) {
        assert_eq!(self.rom_size(), self.data.len());
    }
    pub fn rom_size(&self) -> usize {
        let size = self.read_u8(0x148);
        match size {
            s if size < 8 => (32 * 1024) << s,
            0x52 => 72 * (1024 * 32),
            0x53 => 80 * (1024 * 32),
            0x54 => 96 * (1024 * 32),
            _ => panic!("Unknown rom size {:X}", size),
        }
    }
    pub fn ram_size(&self) -> usize {
        let size = self.read_u8(0x149);
        match size {
            0 => 0,
            1 => 2 * 1024,
            2 => 8 * 1024,
            3 => 32 * 1024,
            _ => panic!("Unknown ram size {:X}", size),
        }
    }
    pub fn read_u8(&self, addr: usize) -> u8 {
        self.data[addr]
    }
}
