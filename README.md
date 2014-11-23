# rust-incidents

This library provides an experiment for error handling in Rust.

It provides efficient error handling and provides powerful debugging
features.

- keeps results small by providing a de-refable failure type
- provides support for python-style tracebacks in debug builds
- allows errors to be freely convertible between each other through
  the `FromError` trait.
- provides a flexible trait for error interoperability.
