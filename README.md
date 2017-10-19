# Log4p

A simple logging library for Prolog, inspired by [log4j](https://logging.apache.org/log4j/).

## Installation

This is a package for [SWI-Prolog](http://www.swi-prolog.org), installable using the built-in [package manager](http://www.swi-prolog.org/pldoc/man?section=prologpack) with the package name `log4p`.

```
?- pack_install(log4p).
```

## Use

The basic model for logging involves a few simple concepts.

 * **Messages** - the basic unit of logging, generally from a single line of code and corresponding to a single line in log output
 * **Handlers** - messages aren't directly written to output logfiles or destinations, but instead are provided to any each log _handler_ (maintained per-thread) which are predicates that accepts a formatted message and emit the message to the log destination specific to each log handler implementation
 * **Levels** - messages have a _level_, and this library maintains a concept of current _log level_ (per-thread) such that messages of a lower level than the current log level are not emitted to any handlers.

 The log levels understood by this library, in ascending order of priority, are: `trace`, `debug`, `info`, `warn`, `error`, `fatal`. If the current log level is set to `info`, for example, then message of `debug` or to further left in that list will not be given to a handler.

 Generating messages is usually a matter of using a number of predicates named for each level: `info/1`, `info/2`, `warn/1`, `warn/2` etc. The `/1` variant logs a constant string (or term rendered as a string). The `/2` variant takes a format string and an array of arguments, then calls `swritef` to generate a constant string which is then passed onto log handlers.

 The current level can be set using the `set_log_level/2` predicate, which will also return the previous value. 

 To add or remove log handlers, use `add_log_handler/1` and `remove_log_handler/1` respectively.

