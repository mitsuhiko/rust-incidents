//! Incidents is a library for Rust that explores error handling.  The
//! idea is to eventually get an RFC together with the ideas and concepts
//! of the library.
//!
//! # The Error Trait
//!
//! Errors are arbitrary, they just need to implement the `Error` trait.
//! This trait currently differs from the one in the `std::error` module
//! and has different associated functions.
//!
//! The bare minimum an error needs to implement is `name` which should
//! be the capitalized name of the error as it should be displayed to a
//! user if it appears in error messages.
//!
//! Example:
//!
//! ```rust
//! struct BadOperation {
//!     desc: &str,
//! }
//!
//! impl Error for BadOperation {
//!     fn name(&self) -> &str { "Bad Operation" }
//!     fn description(&self) -> Option<&str> { Some(self.desc) }
//! }
//! ```
//!
//! # Failures
//!
//! Incidents provides a special wrapper type called a `Failure<T>` which
//! will automatically box an error.  In addition to moving the error to
//! the heap, which will keep results small, this also adds automatic debug
//! helpers for debug builds.  In release the failure is represented as a
//! simple box, whereas in debug it actually builds a `Traceback` behind
//! the scenes that aids debugging.
//!
//! A failure can be dereferenced which will return the error.
//!
//! # Throwing Errors
//!
//! Errors can be thrown with the `fail!` macro and be automatically
//! propagated with the `try!` macro.  For this to work the return value
//! needs to be a `Result` with a `Failure<E: Error>` or `Error` on the
//! error side.  For the latter a `FResult` is provided.
//!
//! Example:
//!
//! ```rust
//! fn function_that_fails(good: bool) -> FResult<int, TheError> {
//!     if !good {
//!         fail!(TheError { desc: "something went wrong" });
//!     }
//!     Ok(23)
//! }
//!
//! fn function_that_tries() -> FResult<int, TheError> {
//!     let rv = try!(function_that_fails());
//!     Ok(rv + 42)
//! }
//! ```
//!
//! # Handling Errors
//!
//! Errors that are in failures can be handled very transparently.  The
//! failure is strongly typed and derefs into the actual error:
//!
//! ```rust
//! fn function_that_handles() {
//!     match function_that_fails() {
//!         Ok(result) => {
//!             println!("The result: {}", result);
//!         }
//!         Err(ref err) => {
//!             println!("Error description: {}", err.desc);
//!         }
//!     }
//! }
//! ```
//!
//! # Error Conversion
//!
//! Errors can be converted through the use of the `FromError` trait.  This
//! allows a library to easily work with other libraries that it might use
//! internally and give the user a consistent error experience.
//!
//! Example:
//!
//! ```rust
//! enum MyError {
//!     BadOperation,
//!     IoError(io::IoError),
//! }
//!
//! impl Error for MyError {
//!     fn name(&self) -> &str {
//!         match *self {
//!             MyError::BadOperation => "Bad Operation",
//!             MyError::IoError(ref err) => err.name(),
//!         }
//!     }
//! }
//!
//! impl FromError<IoError> for MyError {
//!     fn from_error(err: io::IoError) -> MyError {
//!         MyError::IoError(err)
//!     }
//! }
//! ```
//!
//! # Tracebacks
//!
//! In debug builds tracebacks are available.  At any point can you call
//! `Traceback::from_failure()` with a failure to get the traceback.  This
//! also works in release builds but the return value will be `None`.
//!
//! Tracebacks can also be printed with `print_traceback` and `TraceFormatter`.
//! This looks a bit like Python tracebacks:
//!
//! ```
//! Traceback (most recent cause last):
//!   File "/Users/mitsuhiko/Development/rust-incidents/examples/basic.rs", line 37
//!     fail!(FileNotFound { file: Some(Path::new("/missing.txt")) });
//!   File "/Users/mitsuhiko/Development/rust-incidents/examples/basic.rs", line 41
//!     try!(testing());
//! File Not Found: the file does not exist (file=/missing.txt)
//! ```
#![crate_name = "incidents"]
#![crate_type = "lib"]
#![license = "BSD"]
#![comment = "Rust standardized error handling."]

#![deny(non_camel_case_types)]
#![feature(macro_rules)]
#![feature(associated_types)]
#![feature(while_let)]
#![feature(if_let)]

use std::{raw, mem, io};
use std::intrinsics::TypeId;


/// This struct provides error location information.
///
/// This is intentionally kept private internally so that at a later point
/// it can be changed to not actually contain the debug information but
/// to only hold a frozen PC value and later read the debug info from
/// DWARF.
#[deriving(Clone, PartialEq, Eq, Send)]
pub struct LocationInfo {
    file: Path,
    line: uint,
}

impl LocationInfo {

    /// Creates a new location info from specific values.
    pub fn new(file: &str, line: uint) -> LocationInfo {
        LocationInfo {
            file: Path::new(file),
            line: line,
        }
    }

    /// Returns the source line which caused the error.
    ///
    /// This loads the file from the file system and fetches the line
    /// mentioned in the location info.  This might be quite an
    /// expensive operation and it can also return invalid data
    /// in case the file was changed after the compilation took place.
    pub fn get_source_line(&self) -> io::IoResult<String> {
        // can't use try! here because of bootstrapping issues.
        let file = match io::File::open(&self.file) {
            Err(err) => { return Err(err); },
            Ok(f) => f,
        };
        let mut reader = io::BufferedReader::new(file);
        match reader.lines().skip(self.line - 1).next() {
            Some(Ok(line)) => Ok(line),
            _ => Err(io::IoError {
                kind: io::EndOfFile,
                desc: "Reached end of file unexpectedly",
                detail: None,
            }),
        }
    }

    /// Get the filename of this location info.
    pub fn file(&self) -> &Path {
        &self.file
    }

    /// Get the line number of this location info.
    pub fn line(&self) -> uint {
        self.line
    }
}

/// Represents an error.
///
/// At the very least the `name` part of an error needs to be implemented.
/// Everything else is pretty much optional but it's recommended to implement
/// as much as possible to aid the user for debugging.  Note that a lot of
/// information can be computed lazily.
pub trait Error: 'static + Send {

    /// The human readable name of the error.
    fn name(&self) -> &str;

    /// An optional description of the error.
    fn description(&self) -> Option<&str> {
        None
    }

    /// Optional detail information generated from the error's data.
    fn detail(&self) -> Option<String> {
        None
    }

    /// Optionally the cause of an error.
    fn cause(&self) -> Option<&Error> {
        None
    }

    #[doc(hidden)]
    fn get_error_type(&self) -> TypeId { TypeId::of::<Self>() }
}

/// Adds dynamic helpers to errors.
pub trait ErrorExt<'a> {
    /// Checks if an error is of a specific type.
    fn is<E: Error>(self) -> bool;

    /// Casts an error to a concrete type.
    fn cast<E: Error>(self) -> Option<&'a E>;
}

impl<'a> ErrorExt<'a> for &'a Error {

    #[inline(always)]
    fn is<E: Error>(self) -> bool {
        self.get_error_type() == TypeId::of::<E>()
    }

    #[inline(always)]
    fn cast<E: Error>(self) -> Option<&'a E> {
        if self.is::<E>() {
            unsafe {
                let to: raw::TraitObject = mem::transmute_copy(&self);
                Some(mem::transmute(to.data))
            }
        } else {
            None
        }
    }
}


/// Frames are an internal construct in incidents for debugging.
///
/// Frames primarily are used to chain errors together or to attach
/// additional debug information to them.  For the most part you can
/// ignore that frames exist.  The primary use for frames is to render
/// stacktraces.
pub trait Frame: Send {
    /// If the frame points to an error, this returns it.
    fn error(&self) -> Option<&Error> {
        None
    }

    /// If the frame contains location information, this returns it.
    fn location(&self) -> Option<&LocationInfo> {
        None
    }

    /// If the frame was caused by another frame, this returns a reference
    /// to it.  This is used if an error gets handled by failing with
    /// another error.
    fn cause_frame(&self) -> Option<&Frame + Send> {
        None
    }

    /// If the frame is linked directly to another frame, this returns a
    /// reference to it.  This will never be the cause frame.
    fn previous_frame(&self) -> Option<&Frame + Send> {
        None
    }

    /// If it is possible to construct a trace from this frame, it will
    /// return the error and the trace to it.  Note that this is not always
    /// possible.
    fn trace(&self) -> Option<(&Error, Vec<&Frame + Send>)> {
        let mut root = None;
        let mut causes = vec![];
        let mut ptr = Some(&*self as &Frame + Send);

        while let Some(frm) = ptr {
            causes.push(frm);
            match frm.error() {
                Some(err) => {
                    root = Some(err);
                    break;
                }
                None => {
                    ptr = frm.previous_frame();
                }
            }
        }

        if causes.len() > 1 {
            causes.reverse();
            if let Some(err) = root {
                return Some((err, causes));
            }
        }

        return None
    }
}


#[cfg(not(ndebug))]
struct BasicErrorFrame<E: Error> {
    error: E,
    location: Option<LocationInfo>,
}

#[cfg(not(ndebug))]
struct ErrorFrameWithCause<E: Error> {
    error: E,
    cause: Box<Frame + Send>,
    location: Option<LocationInfo>,
}

#[cfg(not(ndebug))]
struct PropagationFrame {
    parent: Box<Frame + Send>,
    location: Option<LocationInfo>,
}

#[cfg(not(ndebug))]
impl<E: Error> Frame for BasicErrorFrame<E> {
    fn error(&self) -> Option<&Error> {
        Some(&self.error as &Error)
    }

    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }
}

#[cfg(not(ndebug))]
impl<E: Error> Frame for ErrorFrameWithCause<E> {
    fn error(&self) -> Option<&Error> {
        Some(&self.error as &Error)
    }

    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }

    fn cause_frame(&self) -> Option<&Frame + Send> {
        Some(&*self.cause)
    }
}

#[cfg(not(ndebug))]
impl Frame for PropagationFrame {
    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }

    fn previous_frame(&self) -> Option<&Frame + Send> {
        Some(&*self.parent)
    }
}

/// Encapsulates errors.
///
/// Tracebacks are generally only available in debug builds.  In debug
/// builds they associate errors with as much debug information as
/// possible.
pub struct Traceback {
    frame: Option<Box<Frame + Send>>,
}

impl Traceback {

    /// Return the traceback object from a failure.
    ///
    /// This will return a borrowed reference to the traceback contained
    /// in a failure.  This operation will return `None` for release builds
    /// for which a failure does not actually contain a traceback.
    pub fn from_failure<E: Error>(failure: &Failure<E>) -> Option<&Traceback> {
        return resolve(failure);

        #[cfg(not(ndebug))]
        fn resolve<E: Error>(failure: &Failure<E>) -> Option<&Traceback> {
            Some(&failure.traceback)
        }

        #[cfg(ndebug)]
        fn resolve<E: Error>(_: &Failure<E>) -> Option<&Traceback> {
            None
        }
    }

    /// Returns the first frame of the traceback.
    pub fn frame(&self) -> &Frame + Send {
        match self.frame {
            Some(ref x) => &**x,
            None => panic!("frame went away")
        }
    }

    /// Returns the error of the traceback.
    ///
    /// Note that a traceback can be chained to another error behind the scenes.
    /// This will only ever give you one error and it will always be the most
    /// recent one.  To discover causes you will need to traverse the frames
    /// manually.
    ///
    /// Example usage:
    ///
    /// ```rust
    /// println!("Error: {}", traceback.error().name());
    /// ```
    pub fn error(&self) -> &Error {
        let mut ptr = self.frame.as_ref().map(|x| &**x);
        while let Some(frm) = ptr {
            match frm.error() {
                Some(err) => { return err; }
                None => { ptr = frm.previous_frame(); }
            }
        }
        panic!("Traceback does not contain an error");
    }

    /// Returns the frame that contains the error.
    pub fn error_frame(&self) -> &Frame + Send {
        let mut ptr = self.frame.as_ref().map(|x| &**x);
        while let Some(frm) = ptr {
            match frm.error() {
                Some(_) => { return frm; }
                None => { ptr = frm.previous_frame(); }
            }
        }
        panic!("Traceback does not contain an error");
    }

    /// Returns the error as a pointer to a concrete type.
    ///
    /// Example usage:
    ///
    /// ```rust
    /// match traceback.error_as::<ResourceNotFound>() {
    ///     Some(rnf) => {
    ///         println!("Resource not found: {}", rnf.resource);
    ///     },
    ///     None => {}
    /// }
    /// ```
    #[inline(always)]
    pub fn error_as<E: Error>(&self) -> Option<&E> {
        self.error().cast::<E>()
    }

    /// Checks if the error is of a specific type.
    ///
    /// Example usage:
    ///
    /// ```rust
    /// if traceback.is::<ResourceNotFound>() {
    ///     println!("Looks like a resource is missing.");
    /// }
    /// ```
    #[inline(always)]
    pub fn is<E: Error>(&self) -> bool {
        self.error().is::<E>()
    }

    /// Shorthand to get the name of the error.
    #[inline(always)]
    pub fn name(&self) -> &str {
        self.error().name()
    }

    /// Gets the description the error.
    #[inline(always)]
    pub fn description(&self) -> Option<&str> {
        self.error().description()
    }

    /// Gets the detail of the error.
    pub fn detail(&self) -> Option<String> {
        let mut ptr = self.frame.as_ref().map(|x| &**x);
        while let Some(frm) = ptr {
            match frm.error() {
                Some(err) => { return err.detail(); }
                None => { ptr = frm.previous_frame(); }
            }
        }
        None
    }

    /// Returns the traces in this traceback.
    ///
    /// Keep in mind that it is entirely permissible for the return
    /// value to be an empty vector.  This can for instance happen
    /// in release builds.
    pub fn traceback(&self) -> Vec<(&Error, Vec<&Frame + Send>)> {
        let mut traces = vec![];
        let mut ptr = self.frame.as_ref().map(|x| &**x);

        while let Some(frm) = ptr {
            if let Some((root, trace)) = frm.trace() {
                ptr = trace[0].cause_frame();
                traces.push((root, trace));
            } else {
                break;
            }
        }

        traces.reverse();
        traces
    }
}

/// A failure wraps an error in a box.
///
/// This solves two purposes.  The first one is that it moves the error
/// to the heap which causes the result value to become smaller for the
/// optimal (non error) case.  The second purpose is to hold additional
/// information for debug builds that aid debugging.
///
/// In debug builds a failure holds a `Traceback` so that it can become
/// possible to discover how an error came about.
///
/// Failures can be dereferenced to get the error that created them.
///
/// To get the traceback of a failure use the `Traceback::from_failure`
/// function.
pub struct Failure<E: Error> {
    #[cfg(ndebug)]
    error: Option<E>,
    #[cfg(not(ndebug))]
    traceback: Traceback,
}

impl<E: Error> Deref<E> for Failure<E> {
    #[cfg(not(ndebug))]
    fn deref(&self) -> &E {
        self.traceback.error_as().expect(
            "Debug failure does not contain a compatible error.")
    }

    #[cfg(ndebug)]
    fn deref(&self) -> &E {
        match self.error {
            Some(ref val) => &*val,
            None => panic!("Failure does not contain an error.")
        }
    }
}

/// Print the traceback of a failure.
///
/// In release builds this will not contain an actual traceback but
/// an emulation based on the information that is available on the
/// error itself.
/// 
/// This internally uses `TraceFormatter`.
pub fn print_traceback<E: Error>(failure: &Failure<E>) {
    let mut fmt = TraceFormatter::new(std::io::stdio::stderr());
    let _ = match Traceback::from_failure(&*failure) {
        Some(tb) => fmt.format_traces(tb),
        None => fmt.format_fallback_trace(&**failure as &Error),
    };
}


/// A trait for types that can be converted from a given error type `E`.
pub trait FromError<E> {

    /// Perform the conversion.
    fn from_error(err: E) -> Self;
}

impl<E> FromError<E> for E {
    fn from_error(err: E) -> E {
        err
    }
}

/// A trait that is used to construct failures.
///
/// Generally this trait should currently be considered internal as there
/// are no public ways to construct `Failure` objects.  This is used by
/// `fail!` and `try!` to fail and convert errors.
pub trait ConstructFailure<A> {
    fn construct_failure(args: A, loc: Option<LocationInfo>) -> Self;
}

impl<E: Error> ConstructFailure<(E,)> for E {
    fn construct_failure((err,): (E,), _: Option<LocationInfo>) -> E {
        err
    }
}

impl<E: Error, T: Error+FromError<E>> ConstructFailure<(E,)> for Box<Failure<T>> {
    #[cfg(ndebug)]
    fn construct_failure((err,): (E,), _: Option<LocationInfo>) -> Box<Failure<T>> {
        box Failure {
            error: Some(FromError::from_error(err)),
        }
    }

    #[cfg(not(ndebug))]
    fn construct_failure((err,): (E,), loc: Option<LocationInfo>) -> Box<Failure<T>> {
        let err: T = FromError::from_error(err);
        box Failure {
            traceback: Traceback {
                frame: Some(box BasicErrorFrame {
                    error: err,
                    location: loc,
                } as Box<Frame + Send>)
            }
        }
    }
}

impl<E: Error, C: Error, T: Error+FromError<E>> ConstructFailure<(E, Box<Failure<C>>)> for Box<Failure<T>> {
    #[cfg(ndebug)]
    fn construct_failure((err, _): (E, Box<Failure<C>>), _: Option<LocationInfo>) -> Box<Failure<T>> {
        box Failure {
            error: Some(FromError::from_error(err)),
        }
    }

    #[cfg(not(ndebug))]
    fn construct_failure((err, cause): (E, Box<Failure<C>>), loc: Option<LocationInfo>) -> Box<Failure<T>> {
        let err: T = FromError::from_error(err);
        let mut cause = cause;
        box Failure {
            traceback: Traceback {
                frame: Some(box ErrorFrameWithCause {
                    error: err,
                    cause: cause.traceback.frame.take().expect(
                        "attempted to use a failure as cause that was already used."),
                    location: loc,
                } as Box<Frame + Send>)
            }
        }
    }
}

impl<E: Error> ConstructFailure<(Box<Failure<E>>,)> for Box<Failure<E>> {
    #[cfg(ndebug)]
    fn construct_failure((parent,): (Box<Failure<E>>,), _: Option<LocationInfo>) -> Box<Failure<E>> {
        parent
    }

    #[cfg(not(ndebug))]
    fn construct_failure((parent,): (Box<Failure<E>>,), loc: Option<LocationInfo>) -> Box<Failure<E>> {
        let mut parent = parent;
        box Failure {
            traceback: Traceback {
                frame: Some(box PropagationFrame {
                    parent: parent.traceback.frame.take().expect(
                        "attempted to propagate a failure that was already propagated."),
                    location: loc,
                } as Box<Frame + Send>)
            }
        }
    }
}


/// Fails with an error.
///
/// This works by taking some arguments and causing an early return with
/// an error value.  The conversion of the error value is performed to the
/// signature of the return value of the function.  This allows this to be
/// used in functions that return `Result<T, E: Error>` as well as
/// functions that return `Result<T, Failure<E: Error>>`.
///
/// `fail!(error)` -> Result<T, Failure<Error>>
///     Fails with an error and wraps it in a failure.
///
/// `fail!(error) -> Result<T, Error>
///     Fails with an error and returns it unwrapped.  This should only be
///     used if the error is small and if debugging is not needed.  This can
///     be useful in certain specialized situations.
///
/// `fail!(failure)`
///     Just fails with an already existing failure and passes it onwards.
///
/// `fail!(error, causing_failure)`
///     Fails with an error and a causing failure.  In debug builds this
///     will allow the errors to be chained even if an error gets "lost".
#[macro_export]
macro_rules! fail {
    ($($expr:expr),*) => ({
        #[cold]
        #[inline(never)]
        fn fail<A, X, T: ::incidents::ConstructFailure<A>>(args: A) -> Result<X, T> {
            Err(::incidents::ConstructFailure::construct_failure(
                args,
                if cfg!(ndebug) {
                    None
                } else {
                    Some(::incidents::LocationInfo::new(file!(), line!()))
                }
            ))
        }
        return fail(($($expr,)*));
    });
}

/// Tries to unwrap the result or propagates an error.
///
/// This is a shorthand for invoking `fail!` for an error branch.
#[macro_export]
macro_rules! try {
    ($expr:expr) => (match $expr {
        Err(x) => fail!(x),
        Ok(x) => x,
    });
    ($expr:expr, $cause:expr) => (match $expr {
        Err(x) => fail!(x, $cause),
        Ok(x) => x,
    });
}

/// A result type which wraps the error in a failure.
pub type FResult<T, E> = Result<T, Box<Failure<E>>>;


// local hack
mod incidents {
    pub use super::{ConstructFailure, LocationInfo};
}


//// Standard Implementations ///////////////////////////////////////////

impl Error for io::IoError {
    fn name(&self) -> &str {
        match self.kind {
            io::IoErrorKind::OtherIoError => "IO Error",
            io::IoErrorKind::EndOfFile => "End Of File",
            io::IoErrorKind::FileNotFound => "File Not Found",
            io::IoErrorKind::PermissionDenied => "Permission Denied",
            io::IoErrorKind::ConnectionFailed => "Connection Failed",
            io::IoErrorKind::Closed => "File Closed",
            io::IoErrorKind::ConnectionRefused => "Connection Refused",
            io::IoErrorKind::ConnectionReset => "Connection Reset",
            io::IoErrorKind::ConnectionAborted => "Connection Aborted",
            io::IoErrorKind::NotConnected => "Not Connected",
            io::IoErrorKind::BrokenPipe => "Broken Pipe",
            io::IoErrorKind::PathAlreadyExists => "Path Already Exists",
            io::IoErrorKind::PathDoesntExist => "Path Does Not Exist",
            io::IoErrorKind::MismatchedFileTypeForOperation => "Mismatched File Type",
            io::IoErrorKind::ResourceUnavailable => "Resource Unavailable",
            io::IoErrorKind::IoUnavailable => "IO Unavailable",
            io::IoErrorKind::InvalidInput => "Invalid Input",
            io::IoErrorKind::TimedOut => "Timed Out",
            io::IoErrorKind::ShortWrite(_) => "Short Write",
            io::IoErrorKind::NoProgress => "No Progress",
        }
    }

    fn description(&self) -> Option<&str> {
        Some(self.desc)
    }

    fn detail(&self) -> Option<String> {
        self.detail.clone()
    }
}


//// Error Tools ////////////////////////////////////////////////////////

/// Formats tracebacks.
pub struct TraceFormatter<W: Writer> {
    writer: W,
}

/// The trace formatter can be used to render tracebacks.
impl<W: Writer> TraceFormatter<W> {

    /// Constructs a new trace formatter.
    pub fn new(writer: W) -> TraceFormatter<W> {
        TraceFormatter {
            writer: writer,
        }
    }

    fn format_divider(&mut self) -> io::IoResult<()> {
        try!(writeln!(&mut self.writer, ""));
        try!(writeln!(&mut self.writer, "The above error resulted in another:"));
        try!(writeln!(&mut self.writer, ""));
        Ok(())
    }

    /// Formats a fallback trace from an error.
    ///
    /// This is useful for non debug builds where tracebacks are genereally
    /// not available.  In that case you can still get some debug information
    /// from the errors alone.  This will try to find as much information as
    /// possible and print them out in a similar format.
    pub fn format_fallback_trace(&mut self, err: &Error) -> io::IoResult<()> {
        let mut causes = vec![];
        let mut ptr = Some(err);

        while let Some(err) = ptr {
            causes.push(err);
            ptr = err.cause();
        }
        causes.reverse();

        for (idx, cause) in causes.iter().enumerate() {
            if idx > 0 {
                try!(self.format_divider());
            }
            try!(self.format_cause(*cause));
        }

        Ok(())
    }

    /// Formats all traces of the incident.
    pub fn format_traces(&mut self, i: &Traceback) -> io::IoResult<()> {
        let mut traces = vec![];
        let mut ptr = Some(i.frame());

        while let Some(frm) = ptr {
            if let Some((root, trace)) = frm.trace() {
                ptr = trace[0].cause_frame();
                traces.push((root, trace));
            } else {
                break;
            }
        }

        traces.reverse();
        for (idx, &(ref root, ref trace)) in traces.iter().enumerate() {
            if idx > 0 {
                try!(self.format_divider());
            }
            try!(self.format_trace(*root, trace.as_slice()));
        }

        Ok(())
    }

    /// Formats a single trace.
    fn format_trace(&mut self, err: &Error, trace: &[&Frame]) -> io::IoResult<()> {
        try!(writeln!(&mut self.writer, "Traceback (most recent cause last):"));
        for cause in trace.iter() {
            try!(self.format_frame(*cause));
        }
        try!(self.format_cause(err));
        Ok(())
    }

    /// Formats a single frame.
    pub fn format_frame(&mut self, frm: &Frame) -> io::IoResult<()> {
        match frm.location() {
            Some(loc) => {
                try!(writeln!(&mut self.writer, "  File \"{}\", line {}",
                              loc.file().display(), loc.line()));
                match loc.get_source_line() {
                    Ok(line) => try!(writeln!(&mut self.writer, "    {}",
                        line.trim_chars([' ', '\t', '\r', '\n'].as_slice()))),
                    Err(_) => {}
                }
            }
            None => {
                try!(writeln!(&mut self.writer, "  File <unknown>, line ?"));
            }
        }
        Ok(())
    }

    /// Formats the cause of an error.
    pub fn format_cause(&mut self, err: &Error) -> io::IoResult<()> {
        try!(write!(&mut self.writer, "{}", err.name()));
        match err.description() {
            Some(desc) => try!(write!(&mut self.writer, ": {}", desc)),
            None => {}
        }
        match err.detail() {
            Some(detail) => try!(write!(&mut self.writer, " ({})", detail)),
            None => {}
        }
        try!(writeln!(&mut self.writer, ""));
        Ok(())
    }
}
