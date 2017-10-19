:- use_module('prolog/log4p').
:- use_module(library(plunit)).

:- set_test_options([
  run(make(all))
  ]).

:- load_test_files(_X).

:- run_tests.
