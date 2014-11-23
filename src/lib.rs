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

// temporary
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
pub trait Error: 'static + Send + Clone {

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


struct BasicErrorFrame<E: Error> {
    error: E,
    location: Option<LocationInfo>,
}

struct ErrorFrameWithCause<E: Error> {
    error: E,
    cause: Box<Frame + Send>,
    location: Option<LocationInfo>,
}

struct PropagationFrame {
    parent: Box<Frame + Send>,
    location: Option<LocationInfo>,
}

impl<E: Error> Frame for BasicErrorFrame<E> {
    fn error(&self) -> Option<&Error> {
        Some(&self.error as &Error)
    }

    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }
}

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

impl Frame for PropagationFrame {
    fn location(&self) -> Option<&LocationInfo> {
        self.location.as_ref()
    }

    fn previous_frame(&self) -> Option<&Frame + Send> {
        Some(&*self.parent)
    }
}

/// Encapsulates errors.
pub struct Traceback {
    frame: Option<Box<Frame + Send>>,
}

impl Traceback {

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
    pub fn print(&self) {
        let mut fmt = TraceFormatter::new(std::io::stdio::stderr());
        let _ = fmt.format_traces(self);
    }
}

/// A failure wraps an error in a box.
///
/// In debug builds it also augments it with debug information.
pub struct Failure<E: Error> {
    error: Option<Box<E>>,
    // XXX: remove from non debug builds
    traceback: Traceback,
}

impl<E: Error> Deref<E> for Failure<E> {
    fn deref(&self) -> &E {
        match self.error {
            Some(ref val) => &**val,
            None => panic!("Error went away!?")
        }
    }
}

pub fn get_traceback<E: Error>(failure: &Failure<E>) -> Option<&Traceback> {
    Some(&failure.traceback)
}

pub fn print_traceback<E: Error>(failure: &Failure<E>) {
    match get_traceback(failure) {
        Some(tb) => tb.print(),
        None => {},
    }
}


// error conversion trait
pub trait FromError<E> {
    fn from_error(err: E) -> Self;
}

impl<E> FromError<E> for E {
    fn from_error(err: E) -> E {
        err
    }
}

pub trait ConstructFailure<A> {
    fn construct_failure(args: A, loc: Option<LocationInfo>) -> Self;
}

impl<E: Error, T: Error+FromError<E>> ConstructFailure<(E,)> for Failure<T> {
    fn construct_failure((err,): (E,), loc: Option<LocationInfo>) -> Failure<T> {
        let err: T = FromError::from_error(err);
        Failure {
            error: Some(box err.clone()),
            traceback: Traceback {
                frame: Some(box BasicErrorFrame {
                    // XXX: would be nice if this would not be a clone but a
                    // pointer to the error, would that be possible?
                    error: err.clone(),
                    location: loc,
                } as Box<Frame + Send>)
            }
        }
    }
}

impl<E: Error> ConstructFailure<(Failure<E>,)> for Failure<E> {
    fn construct_failure((parent,): (Failure<E>,), loc: Option<LocationInfo>) -> Failure<E> {
        let mut parent = parent;
        Failure {
            error: Some(parent.error.take().expect(
                "attempted to move error that was already moved")),
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

impl<E: Error, C: Error, T: Error+FromError<E>> ConstructFailure<(E, Failure<C>)> for Failure<T> {
    fn construct_failure((err, cause): (E, Failure<C>), loc: Option<LocationInfo>) -> Failure<T> {
        let err: T = FromError::from_error(err);
        let mut cause = cause;
        Failure {
            error: Some(box err.clone()),
            traceback: Traceback {
                frame: Some(box ErrorFrameWithCause {
                    error: err.clone(),
                    cause: cause.traceback.frame.take().expect(
                        "attempted to use a failure as cause that was already used."),
                    location: loc,
                } as Box<Frame + Send>)
            }
        }
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
        stdtry!(self.format_cause(err));
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

    /// Formats the cause of an error.
    pub fn format_cause(&mut self, err: &Error) -> io::IoResult<()> {
        stdtry!(write!(&mut self.writer, "{}", err.name()));
        match err.description() {
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


/// Fails with an error.
#[macro_export]
macro_rules! fail {
    ($($expr:expr),*) => ({
        return Err(::incidents::ConstructFailure::construct_failure(
            ($($expr,)*),
            if cfg!(ndebug) {
                None
            } else {
                Some(::incidents::LocationInfo {
                    file: ::std::path::Path::new(file!()),
                    line: line!(),
                    column: column!(),
                })
            }
        ))
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

pub type FResult<T, E> = Result<T, Failure<E>>;
