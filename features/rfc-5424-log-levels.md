# rfc-5424-log-levels

## Overview

Extend log4p's log level system to support RFC 5424 (Syslog Protocol) severity levels. RFC 5424 defines 8 severity levels (0-7: Emergency, Alert, Critical, Error, Warning, Notice, Informational, Debug) which differ from log4p's current 6 levels (trace, debug, info, warn, error, fatal). This feature will add support for RFC 5424 compliant logging.

## Goal

Implement RFC 5424 severity level support in log4p, allowing:
1. Configuration of log output to use RFC 5424 severity values instead of or alongside current levels
2. Mapping of log4p's existing levels to their nearest RFC 5424 equivalents
3. Support for RFC 5424-specific syslog handler outputs
4. Backward compatibility with existing log4p code

## Assumptions

- RFC 5424 mapping to log4p levels will use: Emergency→fatal, Alert→error, Critical→error, Error→error, Warning→warn, Notice→info, Informational→info, Debug→debug
- Trace level (not in RFC 5424) will map to Debug or be handled specially
- Log handlers can opt-in to RFC 5424 mode without breaking existing handlers
- Syslog output is the primary use case for RFC 5424 compliance

## Plan

- [ ] Research RFC 5424 specification and severity levels in detail
- [ ] Design RFC 5424 level mapping strategy and API
- [ ] Create documentation in `docs/RFC5424.md` explaining the mapping
- [ ] Implement new log level configuration option (e.g., `set_log_level_format/1`)
- [ ] Add RFC 5424 level constants/predicates to log4p API
- [ ] Create RFC 5424 syslog handler implementation
- [ ] Update existing handlers to support RFC 5424 output format
- [ ] Write tests for RFC 5424 level mapping and handlers
- [ ] Update main README.md with RFC 5424 information
- [ ] Add examples in `docs/` for RFC 5424 usage
- [ ] Ensure backward compatibility with existing tests
