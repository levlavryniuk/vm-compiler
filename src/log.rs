use std::fmt::Display;
use std::sync::atomic::{AtomicUsize, Ordering};

// ANSI color codes
pub const RED: &str = "\x1b[31m";
pub const YELLOW: &str = "\x1b[33m";
pub const GREEN: &str = "\x1b[32m";
pub const BLUE: &str = "\x1b[34m";
pub const MAGENTA: &str = "\x1b[35m";
pub const CYAN: &str = "\x1b[36m";
pub const RESET: &str = "\x1b[0m";
pub const BOLD: &str = "\x1b[1m";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Off,
    Error,
    Warning,
    Info,
    Debug,
    Trace,
}

static CURRENT_LOG_LEVEL: AtomicUsize = AtomicUsize::new(LogLevel::Info as usize);

pub fn set_log_level(level: LogLevel) {
    CURRENT_LOG_LEVEL.store(level as usize, Ordering::SeqCst);
}

pub fn get_log_level() -> LogLevel {
    match CURRENT_LOG_LEVEL.load(Ordering::SeqCst) {
        0 => LogLevel::Off,
        1 => LogLevel::Error,
        2 => LogLevel::Warning,
        3 => LogLevel::Info,
        4 => LogLevel::Debug,
        _ => LogLevel::Trace,
    }
}

pub fn is_enabled(level: LogLevel) -> bool {
    level as usize <= CURRENT_LOG_LEVEL.load(Ordering::SeqCst)
}

fn log_message(level: LogLevel, component: &str, message: &str) {
    if !is_enabled(level) {
        return;
    }

    let (level_name, color) = match level {
        LogLevel::Off => unreachable!(),
        LogLevel::Error => ("ERROR", RED),
        LogLevel::Warning => ("WARN ", YELLOW),
        LogLevel::Info => ("INFO ", GREEN),
        LogLevel::Debug => ("DEBUG", BLUE),
        LogLevel::Trace => ("TRACE", MAGENTA),
    };

    eprintln!(
        "{}{}[{}]{} {}{}[{}]{} {}",
        color, BOLD, level_name, RESET, CYAN, BOLD, component, RESET, message
    );
}

pub fn error(component: &str, message: impl Display) {
    log_message(LogLevel::Error, component, &format!("{message}"));
}

pub fn warn(component: &str, message: impl Display) {
    log_message(LogLevel::Warning, component, &format!("{message}"));
}

pub fn info(component: &str, message: impl Display) {
    log_message(LogLevel::Info, component, &format!("{message}"));
}

pub fn debug(component: &str, message: impl Display) {
    log_message(LogLevel::Debug, component, &format!("{message}"));
}

pub fn trace(component: &str, message: impl Display) {
    log_message(LogLevel::Trace, component, &format!("{message}"));
}

#[macro_export]
macro_rules! log_if {
    ($level:expr, $component:expr, $($arg:tt)*) => {
        if $crate::log::is_enabled($level) {
            let message = format!($($arg)*);
            match $level {
                $crate::log::LogLevel::Error => $crate::log::error($component, message),
                $crate::log::LogLevel::Warning => $crate::log::warn($component, message),
                $crate::log::LogLevel::Info => $crate::log::info($component, message),
                $crate::log::LogLevel::Debug => $crate::log::debug($component, message),
                $crate::log::LogLevel::Trace => $crate::log::trace($component, message),
                _ => {},
            }
        }
    };
}

#[macro_export]
macro_rules! error {
    ($component:expr, $($arg:tt)*) => {
        $crate::log_if!($crate::log::LogLevel::Error, $component, $($arg)*)
    };
}

#[macro_export]
macro_rules! warn {
    ($component:expr, $($arg:tt)*) => {
        $crate::log_if!($crate::log::LogLevel::Warning, $component, $($arg)*)
    };
}

#[macro_export]
macro_rules! info {
    ($component:expr, $($arg:tt)*) => {
        $crate::log_if!($crate::log::LogLevel::Info, $component, $($arg)*)
    };
}

#[macro_export]
macro_rules! debug {
    ($component:expr, $($arg:tt)*) => {
        $crate::log_if!($crate::log::LogLevel::Debug, $component, $($arg)*)
    };
}

#[macro_export]
macro_rules! trace {
    ($component:expr, $($arg:tt)*) => {
        $crate::log_if!($crate::log::LogLevel::Trace, $component, $($arg)*)
    };
}
