
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CWT-Prolog Server Interface %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(lambda)).
:- use_module(library(mavis)).
:- use_module(library(func)).
:- use_module(database).

:- dynamic port/1.

:- http_handler(/, status, []).
:- http_handler('/query', query, []).

main :-
  attach_db('store.db'),
  server(num_here).


server(Port) :-
  (  port(Port)
  -> true
  ;  asserta(port(Port))
  ),
  http_server(http_dispatch, [port(Port)]).


status(_Request) :-
  format('Content-type: text/plain~n~n'),
  format('Server is up.~n').


query(Query) :-
  http_parameters(Query, [login(User, [string, optional(true)])]),
  login(User, Status),
  format('Content-type: text/plain~n~n'),
  format('~s', [Status]).





disconnect :-
  port(Port),
  http_stop_server(Port, []).



