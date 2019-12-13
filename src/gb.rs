use crate::Rom;

pub struct Gb {
    rom: Rom
}

impl Gb {
    pub fn new(rom: Rom) -> Self {
        Self {
            rom
        }
    }
}
