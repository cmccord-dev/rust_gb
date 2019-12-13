use std::io;

#[derive(Debug)]
pub enum RomLoadErr {
    IoError(io::Error)
}
