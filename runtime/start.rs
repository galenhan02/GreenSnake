use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64, memory: *mut i64) -> i64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    match errcode {
      3 => eprintln!("invalid - not a number"),
      7 => eprintln!("invalid argument"),
      9 => eprintln!("overflow"),
      12 => eprintln!("index out of bounds"),
      _ => eprintln!("an error ocurred {errcode}")
    }
    std::process::exit(1);
}

fn parse_arg(v : &Vec<String>) -> i64 {
  if v.len() < 2 { return 1 }
  let s = &v[1];
  if s == "true" { 7 }
  else if s == "false" { 3 }
  else if s == "nil" {1}
  else { 
    let parsed = s.parse::<i64>().expect("Invalid");
      if(parsed < -4611686018427387904 || parsed > 4611686018427387903) {
        eprintln!("overflow");
        std::process::exit(1);
      }
      else { parsed << 1}
  }
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val : i64) -> i64 {
  println!("{}", snek_str(val));
  return val;
}

fn snek_str(val : i64) -> String {
  if val == 7 { "true".to_string() }
  else if val == 3 { "false".to_string() }
  else if val % 2 == 0 { format!("{}", val >> 1) }
  else if val == 1 { "nil".to_string() }
  else if val & 1 == 1 {
    let addr = (val - 1) as *const i64;
    let length = unsafe {*addr };
    let mut result = "(tuple".to_string();
    for i in 1..=length as isize{
      let element = unsafe { *addr.offset(i) };
      result = result + &format!(" {}", snek_str(element));
    }
    result = result.to_string() + ")";
    return result.to_string();
  }
  else {
    format!("Unknown value: {}", val)
  }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = parse_arg(&args);

    let mut memory = Vec::<i64>::with_capacity(1000000);
    let buffer: *mut i64 = memory.as_mut_ptr();

    let i: i64 = unsafe { our_code_starts_here(input, buffer) };
    snek_print(i);
}