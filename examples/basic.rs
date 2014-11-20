#![feature(phase)]
#![feature(if_let)]

#[phase(plugin, link)]
extern crate incidents;

use incidents::{Error, IResult};

struct BadBehavior;

impl Error for BadBehavior {
    fn name(&self) -> &str {
        "Bad behavior"
    }
}

struct FileNotFound {
    file: Option<Path>,
}

impl Error for FileNotFound {
    fn name(&self) -> &str {
        "File not found"
    }

    fn detail(&self) -> Option<String> {
        match self.file {
            Some(ref file) => {
                Some(format!("file={}", file.display()))
            },
            None => None,
        }
    }
}

fn testing() -> IResult<()> {
    fail!(FileNotFound { file: Some(Path::new("/missing.txt")) },
          "The file could not be found");
}

fn bubble() -> IResult<()> {
    try!(testing());
    Ok(())
}

fn fail_without_incident() -> Result<int, BadBehavior> {
    let x : Result<_, BadBehavior> = Ok(1 + 1);
    Ok(1 + try!(x))
}

fn main() {
    match bubble() {
        Ok(x) => println!("Produced {}", x),
        Err(err) => err.print_traceback(),
    }
    if let Ok(x) = fail_without_incident() {
        println!("Produced {}", x);
    }
}
