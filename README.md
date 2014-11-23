# rust-incidents

This library provides an experiment for error handling in Rust.

It provides efficient error handling and provides powerful debugging
features.

- keeps results small by providing a de-refable failure type
- provides support for python-style tracebacks in debug builds
- allows errors to be freely convertible between each other through
  the `FromError` trait.
- provides a flexible trait for error interoperability.

```rust
struct BadOperation {
    desc: &str,
}

impl Error for BadOperation {
    fn name(&self) -> &str { "Bad Operation" }
    fn description(&self) -> Option<&str> { Some(self.desc) }
}

fn something_that_fails(good: bool) -> FResult<int, BadOperation> {
    if !good {
        fail!(BadOperation { desc: "Something wend badly wrong" });
    }
    Ok((42))
}

fn function_that_tries() -> FResult<int, BadOperation> {
    let rv = try!(function_that_fails());
    Ok(rv + 42)
}
```
