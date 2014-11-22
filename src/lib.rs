//! rust-incidents is a generic error handling system for rust.
//!
//! # What does it do?
//!
//! This library explores the concept of explicit error bubbling in Rust.
//!
//! It is based on the concept of "incidents".  An incident is the chain events
//! that occurr as a result of failing with an error.  Roughly speaking an
//! incident is somewhat similar to a traceback in programming languages with
//! exceptions.
//!
//! In a nutshell:
//!
//! ```rust
//! #![feature(phase)]
//! #[phase(plugin, link)]
//! extern crate incidents;
//! use incidents::{Error, IResult};
//!
//! struct DivisionByZero;
//!
//! impl Error for DivisionByZero {
//!     fn name(&self) -> &str { "Division by zero" }
//! }
//!
//! fn divide_something(a: int, b: int) -> IResult<int> {
//!     if b == 0 {
//!         fail!(DivisionByZero, "cannot divide by zero");
//!     }
//!     Ok(a / b)
//! }
//!
//! fn divide_some_numbers(nums: &[int], divisor: int) -> IResult<Vec<int>> {
//!     let mut rv = vec![];
//!     for num in nums.iter() {
//!         rv.push(try!(divide_something(*num, divisor)));
//!     }
//!     Ok(rv)
//! }
//!
//! fn main() {
//!     match divide_some_numbers(&[1, 2, 3, 4].as_slice(), 3) {
//!         Ok(results) => println!("results: {}", results),
//!         Err(incident) => {
//!             incident.print_traceback();
//!             if incident.is::<DivisionByZero>() {
//!                 println!("Oops, divided by zero?");
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! # How does it work?
//!
//! Functions need to use the `IResult<T>` type provided by this library which
//! makes the function either produce an `Ok(T)` or return an incident that
//! wraps a specific error.  The incident wrapper serves multiple purposes:
//!
//! *   the wrapper will automatically box the error which keeps your results
//!     small.  This will help code that fails infrequently.
//! *   if a piece of code does not want to handle an error, the incident gets
//!     augmented with more debug information so that it becomes obvious
//!     where the error was bubbling through.
//!
//! In addition the incident internally can capture a bit more generic
//! information such as static description strings.
//!
//! # Defining Errors
//!
//! To define an error you need to implement the `Error` trait on a type of
//! yours.  It's recommended that you define as many of these as you need
//! and not repurpose errors if you can avoid it.  This allows a user to
//! conditionally do things depending on which error was raised.
//!
//! ## Simple errors
//!
//! For the most simple error you just need to define a struct without
//! any fields and an implementation for `Error`:
//!
//! ```rust
//! struct BadOperation;
//!
//! impl Error for BadOperation {
//!     fn name(&self) -> &str { "Bad operation" }
//! }
//! ```
//!
//! ## Complex errors
//!
//! For more complex errors it's recommended to implement `detail` so
//! that error messages can see what's happening:
//!
//! ```rust
//! struct FileNotFound {
//!     filename: Option<String>,
//! }
//! 
//! impl Error for FileNotFound {
//!     fn name(&self) -> &str { "File not found" }
//!     fn detail(&self) -> Option<String> {
//!         match self.filename {
//!             Some(ref f) => { Some(format!("filename={}", f)) },
//!             None => None,
//!         }
//!     }
//! }
//! ```
//!
//! # Failing
//!
//! To fail with an error you can use the `fail!` macro.  In the most
//! simplest case all you need to do is to use it with one of your
//! errors:
//!
//! ```rust
//! fail!(BadOperation);
//! fail!(FileNotFound { file: Some("missing.txt") });
//! ```
//!
//! You can also provide a static description which gives a bit of
//! information about why the error occurred as second argument:
//!
//! ```rust
//! fail!(BadOperation, "unable to process the row due to a logic error.");
//! ```
//!
//! Optionally you can also provide a last argument which is another
//! incident.  This is very useful when you are already handling an
//! error and you want to replace it with a different one:
//!
//! ```rust
//! fail!(BadOperation, "could not load record", original_incident);
//! ```
//!
//! # Trying
//!
//! Very often error handling involves bubbling through errors.  This can
//! be achieved through the `try!` macro.  In the simplest form it takes
//! a single argument.  It will then unwrap the result or propagate the
//! error onwards:
//!
//! ```rust
//! let result = try!(a_failing_operation());
//! ```
//!
//! In addition it takes extra arguments it forwards to `fail!`:
//!
//! ```rust
//! let result = try!(a_failing_operation(), OtherError,
//!     "will fail with a different error if it goes wrong");
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

macro_rules! stdtry {
    ($expr:expr) => (match $expr {
        Err(x) => { return Err(::std::error::FromError::from_error(x)); }
        Ok(x) => { x }
    })
}


/// This struct provides error location information.
#[deriving(Clone, PartialEq, Eq, Send)]
pub struct LocationInfo {
    /// the source file that caused the error.
    pub file: Path,
    /// the line in which the error occurred.
    pub line: uint,
    /// the column in which the error occurred.
    pub column: uint,
}

impl LocationInfo {

    /// Returns the source line which caused the error.
    pub fn get_source_line(&self) -> io::IoResult<String> {
        let file = try!(io::File::open(&self.file));
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
}

/// Represents an error.
///
/// At the very least a name is required, however for most errors it's
/// also a good idea to implement the detail.
pub trait Error: 'static + Send {

    /// The human readable name of the error.
    fn name(&self) -> &str;

    /// Optional detail information generated from the error's data.
    fn detail(&self) -> Option<String> {
        None
    }

    #[doc(hidden)]
    fn get_error_type(&self) -> TypeId { TypeId::of::<Self>() }
}

#[doc(hidden)]
pub trait ErrorExt<'a> {
    fn is<E: Error>(self) -> bool;
    fn cast<E: Error>(self) -> Option<&'a E>;
}

#[doc(hidden)]
impl<'a> ErrorExt<'a> for &'a Error {

    #[inline(always)]
    fn is<E: Error>(self) -> bool {
        self.get_error_type() == TypeId::of::<E>()
    }

    #[inline(always)]
    fn cast<E: Error>(self) -> Option<&'a E> {
        if self.is() {
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
    fn error(&self) -> Option<&Error> { None }
    /// If the frame contains a description, this returns it.
    fn description(&self) -> Option<&str> { None }
    /// If the frame contains location information, this returns it.
    fn location(&self) -> Option<&LocationInfo> { None }
    /// If the frame was caused by another frame, this returns a reference
    /// to it.  This is used if an error gets handled by failing with
    /// another error.
    fn cause_frame(&self) -> Option<&Frame + Send> { None }
    /// If the frame is linked directly to another frame, this returns a
    /// reference to it.  This will never be the cause frame.
    fn previous_frame(&self) -> Option<&Frame + Send> { None }
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

#[cfg(ndebug)]
type ErrorLocation = ();

#[cfg(ndebug)]
fn freeze_location(_: Option<LocationInfo>) -> ErrorLocation { () }

#[cfg(not(ndebug))]
type ErrorLocation = Option<LocationInfo>;

#[cfg(not(ndebug))]
fn freeze_location(loc: Option<LocationInfo>) -> ErrorLocation { loc }


struct BasicErrorFrame<E: Error> {
    error: E,
    #[allow(dead_code)]
    location: ErrorLocation,
}

struct DescribedErrorFrame<E: Error> {
    error: E,
    description: &'static str,
    #[allow(dead_code)]
    location: ErrorLocation,
}

struct ErrorFrameWithCause<E: Error> {
    error: E,
    cause: Box<Frame + Send>,
    #[allow(dead_code)]
    location: ErrorLocation,
}

struct ErrorFrameWithCauseAndDescription<E: Error> {
    error: E,
    cause: Box<Frame + Send>,
    description: &'static str,
    #[allow(dead_code)]
    location: ErrorLocation,
}

struct PropagationFrame {
    parent: Box<Frame + Send>,
    #[allow(dead_code)]
    location: ErrorLocation,
}

impl<E: Error> Frame for BasicErrorFrame<E> {
    fn error(&self) -> Option<&Error> {
        Some(&self.error as &Error)
    }

    #[cfg(not(ndebug))]
    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }
}

impl<E: Error> Frame for DescribedErrorFrame<E> {
    fn error(&self) -> Option<&Error> {
        Some(&self.error as &Error)
    }

    fn description(&self) -> Option<&str> {
        Some(self.description)
    }

    #[cfg(not(ndebug))]
    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }
}

impl<E: Error> Frame for ErrorFrameWithCause<E> {
    fn error(&self) -> Option<&Error> {
        Some(&self.error as &Error)
    }

    #[cfg(not(ndebug))]
    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }

    fn cause_frame(&self) -> Option<&Frame + Send> {
        Some(&*self.cause)
    }
}

impl<E: Error> Frame for ErrorFrameWithCauseAndDescription<E> {
    fn error(&self) -> Option<&Error> {
        Some(&self.error as &Error)
    }

    fn description(&self) -> Option<&str> {
        Some(self.description)
    }

    #[cfg(not(ndebug))]
    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }

    fn cause_frame(&self) -> Option<&Frame + Send> {
        Some(&*self.cause)
    }
}

impl Frame for PropagationFrame {
    #[cfg(not(ndebug))]
    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }

    fn previous_frame(&self) -> Option<&Frame + Send> {
        Some(&*self.parent)
    }
}

/// Encapsulates errors.
pub struct Incident {
    frame: Option<Box<Frame + Send>>,
}

/// An incident wraps a trace of errors.  Generally it gives access to the
/// outermost error only but there is extra functionality to return the
/// whole trace in debug builds.
///
/// Incidents cannot be manually created other than through the
/// `ConstructIncident` trait.  Normally you would be using the `fail!`
/// macro to create one.
impl Incident {

    /// Returns the first frame of the incident.
    pub fn frame(&self) -> &Frame + Send {
        match self.frame {
            Some(ref x) => &**x,
            None => panic!("frame went away")
        }
    }

    /// Returns the error of the incident.
    ///
    /// Note that an incident in theory can be chained to another error
    /// behind the scenes.  This will only ever give you one error and it
    /// will always be the most recent one.  To discover causes you will
    /// need to traverse the frames manually.
    ///
    /// Example usage:
    ///
    /// ```rust
    /// println!("Error: {}", incident.error().name());
    /// ```
    pub fn error(&self) -> &Error {
        let mut ptr = self.frame.as_ref().map(|x| &**x);
        while let Some(frm) = ptr {
            match frm.error() {
                Some(err) => { return err; }
                None => { ptr = frm.previous_frame(); }
            }
        }
        panic!("Incident does not contain an error");
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
        panic!("Incident does not contain an error");
    }

    /// Returns the error as a pointer to a concrete type.
    ///
    /// Example usage:
    ///
    /// ```rust
    /// match incident.error_as::<ResourceNotFound>() {
    ///     Some(rnf) => {
    ///         println!("Resource not found: {}", rnf.resource);
    ///     },
    ///     None => {}
    /// }
    /// ```
    #[inline(always)]
    pub fn error_as<E: Error>(&self) -> Option<&E> {
        self.error().cast()
    }

    /// Checks if the error is of a specific type.
    ///
    /// Example usage:
    ///
    /// ```rust
    /// if incident.is::<ResourceNotFound>() {
    ///     println!("Looks like a resource is missing.");
    /// }
    /// ```
    #[inline(always)]
    pub fn is<E: Error>(&self) -> bool {
        self.error().is()
    }

    /// Shorthand to get the name of the error.
    #[inline(always)]
    pub fn name(&self) -> &str {
        self.error().name()
    }

    /// Gets the description the error was thrown with.
    #[inline(always)]
    pub fn description(&self) -> Option<&str> {
        let mut ptr = self.frame.as_ref().map(|x| &**x);
        while let Some(frm) = ptr {
            match frm.description() {
                Some(desc) => { return Some(desc); }
                None => { ptr = frm.previous_frame(); }
            }
        }
        None
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

    /// Returns the traces in this incident.
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

    /// Prints the traceback of the incident to stderr.
    pub fn print_traceback(&self) {
        let mut fmt = TraceFormatter::new(std::io::stdio::stderr());
        let _ = fmt.format_traces(self);
    }
}

/// This trait is indirectly used by `fail!` to construct incidents.
///
/// The following forms are implemented:
///
/// *   `fail!(Incident)`: propagates an already existing incident.
/// *   `fail!(Error)`: fails with an error.
/// *   `fail!(Error, &'static str)`: fails with an error and description.
/// *   `fail!(Error, Incident)`: fails with an error, caused by an incident.
/// *   `fail!(Error, &'static str, Incident)`: fails with an error and
///     description caused by an incident.
pub trait ConstructIncident {
    fn construct_incident(args: Self, loc: Option<LocationInfo>) -> Incident;
}

impl ConstructIncident for (Incident,) {
    fn construct_incident((parent,): (Incident,), loc: Option<LocationInfo>) -> Incident {
        if cfg!(ndebug) {
            return parent;
        }

        let mut parent = parent;
        Incident {
            frame: Some(box PropagationFrame {
                parent: parent.frame.take().unwrap(),
                location: freeze_location(loc),
            } as Box<Frame + Send>)
        }
    }
}

impl<E: Error> ConstructIncident for (E,) {
    fn construct_incident((err,): (E,),
                          loc: Option<LocationInfo>) -> Incident {
        Incident {
            frame: Some(box BasicErrorFrame {
                error: err,
                location: freeze_location(loc),
            } as Box<Frame + Send>)
        }
    }
}

impl<E: Error> ConstructIncident for (E, &'static str)  {
    fn construct_incident((err, desc): (E, &'static str),
                          loc: Option<LocationInfo>) -> Incident {
        Incident {
            frame: Some(box DescribedErrorFrame {
                error: err,
                description: desc,
                location: freeze_location(loc),
            } as Box<Frame + Send>)
        }
    }
}

impl<E: Error> ConstructIncident for (E, Incident) {
    fn construct_incident((err, cause): (E, Incident),
                          loc: Option<LocationInfo>) -> Incident {
        let mut cause = cause;
        Incident {
            frame: Some(box ErrorFrameWithCause {
                error: err,
                cause: cause.frame.take().unwrap(),
                location: freeze_location(loc),
            } as Box<Frame + Send>)
        }
    }
}

impl<E: Error> ConstructIncident for (E, &'static str, Incident) {
    fn construct_incident((err, desc, cause): (E, &'static str, Incident),
                          loc: Option<LocationInfo>) -> Incident {
        let mut cause = cause;
        Incident {
            frame: Some(box ErrorFrameWithCauseAndDescription {
                error: err,
                cause: cause.frame.take().unwrap(),
                description: desc,
                location: freeze_location(loc),
            } as Box<Frame + Send>)
        }
    }
}

/// This trait is used by `fail!` to convert failures.
///
/// The default implementations dispatch to `ConstructIncident` and
/// to just passing errors through transparently.
pub trait FailureConverter<A> {
    fn convert_failure(args: A, loc: Option<LocationInfo>) -> Self;
}

impl<E: ConstructIncident> FailureConverter<E> for Incident {
    #[cold]
    #[inline(never)]
    fn convert_failure(args: E, loc: Option<LocationInfo>) -> Incident {
        ConstructIncident::construct_incident(args, loc)
    }
}

impl<E: Error> FailureConverter<(E,)> for E {
    #[cold]
    #[inline(never)]
    fn convert_failure((err,): (E,), _: Option<LocationInfo>) -> E {
        err
    }
}

/// Formats tracebacks.
pub struct TraceFormatter<W: Writer> {
    writer: W,
}

/// The trace formatter can be used to render tracebacks.
///
/// It's used for instance for the `print_traceback` method on incidents.
impl<W: Writer> TraceFormatter<W> {

    /// Constructs a new trace formatter
    pub fn new(writer: W) -> TraceFormatter<W> {
        TraceFormatter {
            writer: writer,
        }
    }

    /// Formats all traces of the incident.
    pub fn format_traces(&mut self, i: &Incident) -> io::IoResult<()> {
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
                stdtry!(writeln!(&mut self.writer, ""));
                stdtry!(writeln!(&mut self.writer, "The above error resulted in another:"));
                stdtry!(writeln!(&mut self.writer, ""));
            }
            stdtry!(self.format_trace(*root, trace.as_slice()));
        }

        Ok(())
    }

    /// Formats a single trace.
    fn format_trace(&mut self, err: &Error, trace: &[&Frame]) -> io::IoResult<()> {
        stdtry!(writeln!(&mut self.writer, "Traceback (most recent cause last):"));
        for cause in trace.iter() {
            stdtry!(self.format_frame(*cause));
        }
        match trace.iter().next() {
            Some(cause) => stdtry!(self.format_cause(err, *cause)),
            None => {}
        }
        Ok(())
    }

    /// Formats a single frame.
    pub fn format_frame(&mut self, frm: &Frame) -> io::IoResult<()> {
        match frm.location() {
            Some(loc) => {
                stdtry!(writeln!(&mut self.writer, "  File \"{}\", line {}",
                              loc.file.display(), loc.line));
                match loc.get_source_line() {
                    Ok(line) => stdtry!(writeln!(&mut self.writer, "    {}",
                        line.trim_chars([' ', '\t', '\r', '\n'].as_slice()))),
                    Err(_) => {}
                }
            }
            None => {
                stdtry!(writeln!(&mut self.writer, "  File <unknown>, line ?"));
            }
        }
        Ok(())
    }

    /// Formats the cause of an error.  Requires both the error and the
    /// frame that goes with it.
    pub fn format_cause(&mut self, err: &Error, frm: &Frame) -> io::IoResult<()> {
        stdtry!(write!(&mut self.writer, "{}", err.name()));
        match frm.description() {
            Some(desc) => stdtry!(write!(&mut self.writer, ": {}", desc)),
            None => {}
        }
        match err.detail() {
            Some(detail) => stdtry!(write!(&mut self.writer, " ({})", detail)),
            None => {}
        }
        stdtry!(writeln!(&mut self.writer, ""));
        Ok(())
    }
}


#[doc(hidden)]
#[macro_export]
macro_rules! __error_location {
    () => ({
        if cfg!(ndebug) {
            None
        } else {
            Some(::incidents::LocationInfo {
                file: ::std::path::Path::new(file!()),
                line: line!(),
                column: col!(),
            })
        }
    })
}

/// Fails with an error.
#[macro_export]
macro_rules! fail {
    ($($expr:expr),*) => ({
        return Err(::incidents::FailureConverter::convert_failure(
            ($($expr,)*), __error_location!()))
    });
}

/// Tries to unwrap the result or propagates an error.
#[macro_export]
macro_rules! try {
    ($expr:expr) => (match $expr {
        Err(x) => fail!(x),
        Ok(x) => x,
    });
    ($expr:expr, $($args:expr),*) => (match $expr {
        Err(x) => fail!($($args),*, x),
        Ok(x) => x,
    });
}

/// A result type that uses an incidents as `Err` case.
pub type IResult<T> = Result<T, Incident>;
