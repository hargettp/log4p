:- module(log4p,[
  fatal/1,
  error/1,
  warn/1,
  info/1,
  debug/1,
  trace/1,

  fatal/2,
  error/2,
  warn/2,
  info/2,
  debug/2,
  trace/2,

  get_log_level/1,

  set_global_log_level/1,
  set_global_log_level/2,
  clear_global_log_level/0,

  set_local_log_level/1,
  set_local_log_level/2,
  clear_local_log_level/0,

  log_levels/1,

  add_log_handler/1,
  remove_log_handler/1,
  stderr_log_handler/2,

  use_default_log_handler/0,
  use_stderr_log_handler/0,

  logf/3,
  log/2
  ]).

fatal(Message) :- logf(fatal,Message,[]).
error(Message) :- logf(error,Message,[]).
warn(Message) :- logf(warn,Message,[]).
info(Message) :- logf(info,Message,[]).
debug(Message) :- logf(debug,Message,[]).
trace(Message) :- logf(trace,Message,[]).


fatal(Message,Arguments) :- logf(fatal,Message,Arguments).
error(Message,Arguments) :- logf(error,Message,Arguments).
warn(Message,Arguments) :- logf(warn,Message,Arguments).
info(Message,Arguments) :- logf(info,Message,Arguments).
debug(Message,Arguments) :- logf(debug,Message,Arguments).
trace(Message,Arguments) :- logf(trace,Message,Arguments).

use_default_log_handler :-
  retractall(log_handler(_)),
  add_log_handler(default_log_handler).


use_stderr_log_handler :-
  retractall(log_handler(_)),
  add_log_handler(stderr_log_handler).

:- dynamic log_handler/1.
:- thread_local log_handler/1.

log_handler(default_log_handler).

add_log_handler(NewHandler) :-
  Clause = log_handler(NewHandler),
  Clause ; assertz(log_handler(NewHandler)).

remove_log_handler(OldHandler) :-
  retractall(log_handler(OldHandler)).

default_log_handler(Level,Message) :-
  writef('%w: %w\n',[Level, Message]),
  flush_output.

stderr_log_handler(Level,Message) :-
  with_output_to(user_error, (
    default_log_handler(Level, Message)
    )
  ).

:- dynamic global_log_level/1.

:- thread_local local_log_level/1.

% Defines the default log level, if no other value
% has been set
default_log_level(info).

% Return the current effective log level,
% choosing any locally set log level first,
% then the global log level, and finally the 
% default (info)
get_log_level(LogLevel) :-
  local_log_level(LogLevel), !.

get_log_level(LogLevel) :-
  global_log_level(LogLevel), !.

get_log_level(LogLevel) :-
  default_log_level(LogLevel),!.

set_global_log_level(NewLevel) :-
  set_global_log_level(NewLevel, _).

set_global_log_level(NewLevel,OldLevel) :-
  ( global_log_level(OldLevel)
    -> clear_global_log_level
    ; true
    ),
  asserta(global_log_level(NewLevel)).

clear_global_log_level :-
  retractall(global_log_level(_)).

set_local_log_level(NewLevel) :-
  set_local_log_level(NewLevel, _).

set_local_log_level(NewLevel,OldLevel) :-
  ( local_log_level(OldLevel)
    -> clear_local_log_level
    ; true
    ),
  retractall(local_log_level(OldLevel)),
  asserta(local_log_level(NewLevel)).

clear_local_log_level :-
  retractall(local_log_level(_)).

log_levels([trace,debug,info,warn,error,fatal]).

valid_log_levels(ValidLevels) :-
  get_log_level(Level),
  log_levels(Levels),
  valid_log_levels(Level,Levels,ValidLevels).

valid_log_levels(LogLevel,[LogLevel | Rest],[LogLevel | Rest]).

valid_log_levels(LogLevel,[_Head|Rest],ValidLevels) :-
  valid_log_levels(LogLevel,Rest,ValidLevels).

logf(Level,Message,Arguments) :-
  swritef(FullMessage,Message,Arguments),
  log(Level,FullMessage).

% We don't log if the level is not valid
log(Level,_Message) :-
  log_levels(Levels),
  \+member(Level,Levels),
  !.

% We also don't log if the level is too low (e.g, below current)
log(Level,_Message) :-
  log_levels(Levels),
  get_log_level(Current),
  index_of(Level,Levels,LevelIndex),
  index_of(Current,Levels,CurrentIndex),
  LevelIndex < CurrentIndex,
  !.

log(Level,Message) :-
  forall(log_handler(Handler),apply(Handler,[Level,Message])).

index_of(Element,List,Index) :-
  index_of(0,Element,List,Index).

index_of(Current,Element,[Element|_],Current) :- !.

index_of(Current, Element, [_|RestOfList], Index) :- 
  Next is Current + 1,
  index_of(Next, Element, RestOfList,Index).
