:- begin_tests(log4p).

:- use_module(log4p).

test(default_log_level) :-
  log4p:get_log_level(info).

test(set_global_log_level) :-
  log4p:get_log_level(info),
  set_global_log_level(warn),
  log4p:get_log_level(warn),
  set_global_log_level(info,warn),
  log4p:get_log_level(info),
  findall(Level,log4p:global_log_level(Level),[info]).

test(set_local_log_level) :-
  log4p:get_log_level(info),
  set_local_log_level(warn),
  log4p:get_log_level(warn),
  set_local_log_level(info,warn),
  log4p:get_log_level(info),
  findall(Level,log4p:local_log_level(Level),[info]).

test(log_levels, [setup(set_global_log_level(informational, Current)), cleanup((clear_global_log_level, set_global_log_level(Current,_)))]) :-
  log_levels([trace,debug,informational,notice,warning,error,critical,alert,emergency]).

test(valid_default_log_levels, [setup((clear_local_log_level, set_global_log_level(informational, Current))), cleanup((clear_local_log_level, clear_global_log_level, set_global_log_level(Current,_)))]) :-
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[informational,notice,warning,error,critical,alert,emergency]]).

test(valid_warning_log_levels) :-
  clear_local_log_level,
  set_global_log_level(warning),
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[warning,error,critical,alert,emergency]]),
  set_global_log_level(informational,warning).

test(valid_emergency_log_levels) :-
  set_global_log_level(emergency),
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[emergency]]),
  set_global_log_level(informational,emergency).

% Backward compatibility tests - legacy aliases should work
test(legacy_info_alias) :-
  clear_global_log_level,
  set_global_log_level(informational),
  info('test'),  % should work, mapped to informational
  clear_global_log_level.

test(legacy_warn_alias) :-
  clear_global_log_level,
  set_global_log_level(warning),
  warn('test'),  % should work, mapped to warning
  clear_global_log_level.

test(legacy_fatal_alias) :-
  clear_global_log_level,
  set_global_log_level(emergency),
  fatal('test'),  % should work, mapped to emergency
  clear_global_log_level.

% RFC 5424 level tests
test(rfc5424_emergency_level) :-
  clear_global_log_level,
  set_global_log_level(emergency),
  emergency('test message'),
  clear_global_log_level.

test(rfc5424_alert_level) :-
  clear_global_log_level,
  set_global_log_level(alert),
  alert('test message'),
  clear_global_log_level.

test(rfc5424_critical_level) :-
  clear_global_log_level,
  set_global_log_level(critical),
  critical('test message'),
  clear_global_log_level.

test(rfc5424_notice_level) :-
  clear_global_log_level,
  set_global_log_level(notice),
  notice('test message'),
  clear_global_log_level.

test(rfc5424_informational_level) :-
  clear_global_log_level,
  set_global_log_level(informational),
  informational('test message'),
  clear_global_log_level.

% RFC 5424 with format strings
test(rfc5424_emergency_format) :-
  clear_global_log_level,
  set_global_log_level(emergency),
  emergency('test ~w', [message]),
  clear_global_log_level.

test(rfc5424_alert_format) :-
  clear_global_log_level,
  set_global_log_level(alert),
  alert('alert ~w ~w', [test, message]),
  clear_global_log_level.
:- end_tests(log4p).
