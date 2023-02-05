:- begin_tests(log4p).

:- use_module(log4p).

test(set_log_level, [setup(set_log_level(info, Current)), cleanup(set_log_level(Current,_))]) :-
  set_log_level(warn,_),
  log4p:log_level(warn),
  set_log_level(info,warn),
  log4p:log_level(info).

test(log_levels, [setup(set_log_level(info, Current)), cleanup(set_log_level(Current,_))]) :-
  log_levels([trace,debug,info,warn,error,fatal]).

test(valid_default_log_levels, [setup(set_log_level(info, Current)), cleanup(set_log_level(Current,_))]) :-
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[info,warn,error,fatal]]).

test(valid_warn_log_levels, [setup(set_log_level(info, Current)), cleanup(set_log_level(Current,_))]) :-
  set_log_level(warn,info),
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[warn,error,fatal]]),
  set_log_level(info,warn).

test(valid_fatal_log_levels, [setup(set_log_level(info, Current)), cleanup(set_log_level(Current,_))]) :-
  set_log_level(fatal,info),
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[fatal]]),
  set_log_level(info,fatal).

:- end_tests(log4p).
