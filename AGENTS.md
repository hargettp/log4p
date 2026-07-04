# log4p Project Guide

## What is log4p?

**log4p** is a simple logging library for Prolog, inspired by [log4j](https://logging.apache.org/log4j/). It provides a straightforward API for logging messages at different severity levels (trace, debug, info, warn, error, fatal) with support for handlers that direct log output to various destinations.

### Key Features

- **Log Levels** — Messages can be logged at different priority levels (trace, debug, info, warn, error, fatal)
- **Log Handlers** — Pluggable handlers determine where and how messages are output
- **Per-Thread Control** — Log levels can be configured globally or per-thread
- **Format Strings** — Support for formatted logging with arguments (like printf)
- **Simple API** — Easy-to-use predicates for each log level (info/1, warn/2, etc.)

## Project Structure

```
log4p/
├── prolog/           # Prolog source code for the library
├── docs/             # Project documentation (Architecture, API, concepts, examples)
├── features/         # Feature planning and tracking (planned work with checkboxes)
├── README.md         # Quick start and usage guide
├── AGENTS.md         # This file — guide for developers and AI assistants
├── pack.pl           # SWI-Prolog package metadata
└── test.pl           # Tests
```

### `docs/` Directory

Documentation files that help understand and work with log4p:
- Deep dives into architecture and design
- API reference beyond the main README
- Explanations of core concepts
- Code examples and use cases
- Contributing guidelines

### `features/` Directory

Each planned or in-progress feature gets a dedicated Markdown file with:
- **Overview** — What we're building and why
- **Goal** — Measurable outcome
- **Assumptions** — What we're taking for granted
- **Plan** — Step-by-step implementation with checkboxes

## Getting Started

1. **Install dependencies** — log4p is a SWI-Prolog package
   ```
   ?- pack_install(log4p).
   ```

2. **Read the README** — Start with [README.md](README.md) for usage examples

3. **Explore the docs** — Check [docs/README.md](docs/README.md) for architectural details

4. **Run tests** — Execute `test.pl` to verify functionality

## How to Add a Feature

1. Create a new file in `features/` like `features/my-feature.md`
2. Fill out the Overview, Goal, Assumptions, and Plan sections
3. Use `- [ ]` checkboxes in the Plan section to track progress
4. Reference the feature doc when discussing implementation
5. Check off boxes as work completes

Example:
```markdown
# Feature: Custom Log Format

## Overview
Allow users to customize log message formatting beyond the default format.

## Goal
Implement a predicate `set_log_format/1` that accepts a format string/template.

## Assumptions
- Format strings use a simple template syntax
- Users need this for integration with existing systems

## Plan
- [ ] Design format string syntax
- [ ] Implement parser for format strings
- [ ] Update handlers to use custom format
- [ ] Write tests
- [ ] Update documentation
```

## Running Tests

```bash
?- consult('test.pl').
```

## Contributing

When contributing to log4p:

1. Write clear commit messages
2. Add tests for new functionality
3. Update documentation in `docs/` if behavior changes
4. Follow the existing Prolog code style
5. Create a feature doc in `features/` for significant work

## Key Concepts

### Log Levels (in order of priority)
- `trace` — Most verbose, for detailed tracing
- `debug` — Debug information
- `info` — Informational messages (default level)
- `warn` — Warning messages
- `error` — Error messages
- `fatal` — Severe errors

Messages at or above the current log level are emitted; lower levels are filtered out.

### Log Handlers

Handlers are predicates that accept formatted messages and emit them to destinations (stdout, stderr, files, etc.). Each thread maintains its own set of handlers.

### Log Level Configuration

The effective log level per-thread is determined by (in priority order):
1. **Local level** — Thread-specific setting (if set)
2. **Global level** — System-wide setting (if set)
3. **Default level** — `info` (built-in default)

## Questions?

- Check the [README](README.md) for quick reference
- Browse [docs/](docs/) for detailed explanations
- Look at [features/](features/) to see planned work
- Review the Prolog source in `prolog/` directory
