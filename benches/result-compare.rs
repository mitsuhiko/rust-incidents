extern crate test;

use test::Bencher;

type TheError = std::io::IoError;

fn make_error() -> std::io::IoError {
    std::io::IoError {
        kind: std::io::IoErrorKind::FileNotFound,
        desc: "This is just some stuff",
        detail: Some("this is a detail".to_string()),
    }
}

#[inline(never)]
fn large_failure(fail: bool) -> Result<(), TheError> {
    if fail {
        Err(make_error())
    } else {
        Ok(())
    }
}

#[inline(never)]
fn large_failure_caller(fail: bool) -> Result<(), TheError> {
    try!(large_failure(fail));
    Ok(())
}

#[inline(never)]
fn large_failure_caller2(fail: bool) -> Result<(), TheError> {
    try!(large_failure_caller(fail));
    Ok(())
}

#[inline(never)]
fn large_failure_caller3(fail: bool) -> Result<(), TheError> {
    try!(large_failure_caller2(fail));
    Ok(())
}

#[inline(never)]
fn large_failure_caller4(fail: bool) -> Result<(), TheError> {
    try!(large_failure_caller3(fail));
    Ok(())
}

#[inline(never)]
fn large_failure_caller5(fail: bool) -> Result<(), TheError> {
    try!(large_failure_caller4(fail));
    Ok(())
}

#[inline(never)]
fn large_failure_caller6(fail: bool) -> Result<(), TheError> {
    try!(large_failure_caller5(fail));
    Ok(())
}

#[inline(never)]
fn large_failure_caller7(fail: bool) -> Result<(), TheError> {
    try!(large_failure_caller6(fail));
    Ok(())
}

#[inline(never)]
fn small_failure(fail: bool) -> Result<(), Box<TheError>> {
    if fail {
        Err(box make_error())
    } else {
        Ok(())
    }
}

#[inline(never)]
fn small_failure_caller(fail: bool) -> Result<(), Box<TheError>> {
    try!(small_failure(fail));
    Ok(())
}

#[inline(never)]
fn small_failure_caller2(fail: bool) -> Result<(), Box<TheError>> {
    try!(small_failure_caller(fail));
    Ok(())
}

#[inline(never)]
fn small_failure_caller3(fail: bool) -> Result<(), Box<TheError>> {
    try!(small_failure_caller2(fail));
    Ok(())
}

#[inline(never)]
fn small_failure_caller4(fail: bool) -> Result<(), Box<TheError>> {
    try!(small_failure_caller3(fail));
    Ok(())
}

#[inline(never)]
fn small_failure_caller5(fail: bool) -> Result<(), Box<TheError>> {
    try!(small_failure_caller4(fail));
    Ok(())
}

#[inline(never)]
fn small_failure_caller6(fail: bool) -> Result<(), Box<TheError>> {
    try!(small_failure_caller5(fail));
    Ok(())
}

#[inline(never)]
fn small_failure_caller7(fail: bool) -> Result<(), Box<TheError>> {
    try!(small_failure_caller6(fail));
    Ok(())
}


#[bench]
fn bench_large_result_ok_7(b: &mut Bencher) {
    b.iter(|| {
        let _ = test::black_box(large_failure_caller7(false));
    });
}

#[bench]
fn bench_small_result_ok_7(b: &mut Bencher) {
    b.iter(|| {
        let _ = test::black_box(small_failure_caller7(false));
    });
}

#[bench]
fn bench_large_result_7(b: &mut Bencher) {
    b.iter(|| {
        let _ = test::black_box(large_failure_caller7(true));
    });
}

#[bench]
fn bench_small_result_7(b: &mut Bencher) {
    b.iter(|| {
        let _ = test::black_box(small_failure_caller7(true));
    });
}

#[bench]
fn bench_large_result_ok_3(b: &mut Bencher) {
    b.iter(|| {
        let _ = test::black_box(large_failure_caller3(false));
    });
}

#[bench]
fn bench_small_result_ok_3(b: &mut Bencher) {
    b.iter(|| {
        let _ = test::black_box(small_failure_caller3(false));
    });
}

#[bench]
fn bench_large_result_3(b: &mut Bencher) {
    b.iter(|| {
        let _ = test::black_box(large_failure_caller3(true));
    });
}

#[bench]
fn bench_small_result_3(b: &mut Bencher) {
    b.iter(|| {
        let _ = test::black_box(small_failure_caller3(true));
    });
}
