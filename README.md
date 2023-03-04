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

 Generating messages is usually a matter of using a number of predicates named for each level: `info/1`, `info/2`, `warn/1`, `warn/2` etc. The `/1` variant logs a constant string (or term rendered as a string). The `/2` variant takes a format string and an array of arguments, then calls `swritef` to generate a constant string which is then passed onto log handlers. If the currently effective log level for the calling thread is not equal to or "higher" (e.g., more to the right in the above list of log levels), then the message will be filtered out and not delivered to any handlers.

The effective log level is a layered per-thread choice, depending on which settings are specified:

* `default_log_level` : the `log4p` library defines `info` as the log level to assume if no other setting has been specified for the current thread.
* `global_log_level`: any configured global level overrides the default for all threads, new or existing. A global setting remains in place until its cleared with `clear_global_log_level`.
* `local_log_level`: if a level has been specified for the current thread, then this value will override any global setting, even if the global value is later changed. Using `clear_local_log_level` will remove any local setting, thus resetting the thread's level to either the global or default level.

The current level can be set using the any of the following techniques:
  * `set_local_log_level/1` or `set_local_log_level/2`: this will change the log level in effect for the current thread
  * `set_global_log_level/1` or `set_global_log_level/2`: this will change the global log level for all threads that do not already have local log level set.
  * `clear_local_log_level/0`: this removes any value set for the current thread's log level
  * `clear_global_log_level/0`: clear any global log level setting for all threads

 Log handlers disseminate messages to an output desintation, such as `stdout` or `stderr` or a log file. To add or remove log handlers, use `add_log_handler/1` and `remove_log_handler/1` respectively. A log handler is a simple function that simply takes 2 arguments: the `level` of the message, and the `message` etc. (after formatting).
