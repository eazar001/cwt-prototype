
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
:- http_handler('/login', login, []).
:- http_handler('/logout', logout, []).

main :-
  attach_db('store.db'),
  server(port_here).


server(Port) :-
  (  port(Port)
  -> true
  ;  asserta(port(Port))
  ),
  http_server(http_dispatch, [port(Port)]).


status(_Request) :-
  format('Content-type: text/plain~n~n'),
  format('Server is up.~n').

login(Query) :-
  http_parameters(Query, [name(User, [string])]),
  login(User, Status),
  format('Content-type: text/plain~n~n'),
  format('~s', [Status]).

logout(Query) :-
  http_parameters(Query, [name(User, [string])]),
  logout(User, Status),
  format('Content-type: text/plain~n~n'),
  format('~s', [Status]).

disconnect :-
  port(Port),
  http_stop_server(Port, []).



