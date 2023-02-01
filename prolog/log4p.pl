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

  set_log_level/2,
  log_levels/1,

  add_log_handler/1,
  remove_log_handler/1,
  stderr_log_handler/2,

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

:- dynamic log_level/1.
:- thread_local log_level/1.

log_level(info).

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
  
set_log_level(NewLevel,OldLevel) :-
  log_level(OldLevel),
  retractall(log_level(OldLevel)),
  asserta(log_level(NewLevel)).

log_levels([trace,debug,info,warn,error,fatal]).

valid_log_levels(ValidLevels) :-
  log_level(Level),
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
  log_level(Current),
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
