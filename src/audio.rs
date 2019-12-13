use crate::Io;

#[derive(Default, Debug)]
pub struct SoundChannel {
    sweep: u8,
    snd_len_wave: u8,
    vol_env: u8,
    freq_lo: u8,
    freq_hi: u8,
}

#[derive(Default, Debug)]
pub struct Audio {
    channel1: SoundChannel,
    Nr50: u8,
    Nr51: u8, //terminal selection
    Nr52: u8, //on
}

impl Audio {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Io for Audio {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x10 => self.channel1.sweep,
            0x11 => self.channel1.snd_len_wave,
            0x12 => self.channel1.vol_env,
            0x13 => self.channel1.freq_lo,
            0x14 => self.channel1.freq_hi,
            0x24 => self.Nr50,
            0x25 => self.Nr51,
            0x26 => self.Nr52,
            _ => panic!("Audio: Invalid address {:X}", addr),
        }
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        match addr {
            0x10 => self.channel1.sweep = val,
            0x11 => self.channel1.snd_len_wave = val,
            0x12 => self.channel1.vol_env = val,
            0x13 => self.channel1.freq_lo = val,
            0x14 => self.channel1.freq_hi = val,
            0x24 => self.Nr50 = val,
            0x25 => self.Nr51 = val,
            0x26 => self.Nr52 = val,
            _ => panic!("Stores not supported for Audio @{:X}={:X}", addr, val),
        }
    }
}
