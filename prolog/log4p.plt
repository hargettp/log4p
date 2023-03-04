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

test(log_levels, [setup(set_global_log_level(info, Current)), cleanup((clear_global_log_level, set_global_log_level(Current,_)))]) :-
  log_levels([trace,debug,info,warn,error,fatal]).

test(valid_default_log_levels, [setup(set_global_log_level(info, Current)), cleanup((clear_global_log_level, set_global_log_level(Current,_)))]) :-
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[info,warn,error,fatal]]).

test(valid_warn_log_levels) :-
  clear_local_log_level,
  set_global_log_level(warn),
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[warn,error,fatal]]),
  set_global_log_level(info,warn).

test(valid_fatal_log_levels) :-
  set_global_log_level(fatal),
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[fatal]]),
  set_global_log_level(info,fatal).

:- end_tests(log4p).
