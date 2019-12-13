use rust_gb::loader::load_rom;
use rust_gb::{Cpu, Mbc1, Mem};

fn main() {
    let rom = load_rom("gb-test-roms/cpu_instrs/individual/03-op sp,hl.gb");
    if let Err(err) = rom {
        panic!("Failure, {:#?}", err)
    }
    let rom = rom.unwrap();
    let mbc = Box::new(Mbc1::new(rom));
    let mem = Mem::new(mbc);
    let mut cpu = Cpu::new(mem);
    let res =  std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        loop {
            cpu.step();
            //println!("{:X?}", cpu);
        }
    }));
    match res {
        Ok(_) => println!("Successful"),
        Err(err) => {
            println!("{:X?}", err);
            println!("{:X?}", cpu);
        }
    }
}
