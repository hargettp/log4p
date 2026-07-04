# rfc-5424-log-levels

## Overview

Extend log4p's log level system to support RFC 5424 (Syslog Protocol) severity levels. RFC 5424 defines 8 severity levels (0-7: Emergency, Alert, Critical, Error, Warning, Notice, Informational, Debug). We'll add these new levels to log4p by extending the `log_levels/1` predicate and adding corresponding wrapper predicates for each new level, maintaining backward compatibility with existing code.

## Goal

Add RFC 5424 severity levels to log4p while maintaining backward compatibility:
1. Extend `log_levels/1` to include new RFC 5424 levels (emergency, alert, critical, notice) in correct priority order
2. Add wrapper predicates (1 and 2 argument versions) for each new level
3. Export new level predicates from the module
4. All existing log4p code continues to work unchanged

## Assumptions

- New RFC 5424 levels fit naturally in the priority ordering: emergency, alert, critical, error, warning, notice, informational, debug, trace
- The existing core logging logic (validation, filtering, handler dispatch) needs no changes
- No RFC 5424-specific handlers are needed for this initial implementation
- Backward compatibility is maintained - existing `trace/1,2`, `debug/1,2`, `info/1,2`, `warn/1,2`, `error/1,2`, `fatal/1,2` predicates work unchanged

## Plan

- [ ] Extend `log_levels/1` predicate to include RFC 5424 levels in priority order
- [ ] Add `emergency/1` and `emergency/2` wrapper predicates (RFC 5424 Emergency, maps to log4p fatal)
- [ ] Add `alert/1` and `alert/2` wrapper predicates (RFC 5424 Alert)
- [ ] Add `critical/1` and `critical/2` wrapper predicates (RFC 5424 Critical)
- [ ] Add `notice/1` and `notice/2` wrapper predicates (RFC 5424 Notice)
- [ ] Add `informational/1` and `informational/2` wrapper predicates (RFC 5424 Informational, alias for info)
- [ ] Update module exports to include new predicates
- [ ] Run existing tests to verify backward compatibility
- [ ] Add tests for new RFC 5424 levels
- [ ] Update README.md with RFC 5424 level availability
- [ ] Create `docs/RFC5424.md` documenting the level mapping and usage
