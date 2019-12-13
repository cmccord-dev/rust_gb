use crate::Gb;
use crate::Rom;
use crate::Io;
use crate::RomLoadErr;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub fn load_file(file: &str) -> std::io::Result<Vec<u8>> {
    let mut file = File::open(file)?;
    let mut ret = Vec::new();
    file.read_to_end(&mut ret)?;
    Ok(ret)
}


pub fn load_rom(file: &str) -> Result<Rom, RomLoadErr> {
    /*let bios = match load_file("DMG_ROM.bin") {
        Err(err) => return Err(RomLoadErr::IoError(err)),
        Ok(data) => {
            assert_eq!(data.len(), 256, "BIOS size");
            data
        }
    };*/
    match load_file(file) {
        Err(err) => Err(RomLoadErr::IoError(err)),
        Ok(data) => {
            //assert!(data.len() > bios.len());
            //bios.iter().enumerate().for_each(|(i, v)| data[i] = *v);
            let rom = Rom::with_data(data);
            let name = rom.name();
            let name = if name.len() == 0 {
                let p = Path::new(file);
                String::from(p.file_stem().unwrap().to_str().unwrap())
            } else {
                name
            };
            rom.cartridge_type();
            rom.check_rom_size();
            assert_eq!(rom.ram_size(), 0, "cart RAM not supported");
            assert_eq!(rom.read_u8(0x146),0x00, "SGB function not supported");
            println!("Loading rom {} ({})", file, name);
            Ok(rom)
        }
    }
}
