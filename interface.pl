
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


%--------------------------------------------------------------------------------%
% Main Interface
%--------------------------------------------------------------------------------%


:- dynamic port/1.

:- http_handler(/, server_status, []).
:- http_handler('/login', login, []).
:- http_handler('/logout', logout, []).


%% start_server(+File:atom, +Port:between(1, 0xffff)) is semidet.
%
% Attach to the specified database file, and start the server on the specified
% port.

start_server(File, Port) :-
  must_be(between(1, 0xffff), Port),
  attach_db(File),
  asserta(port(Port)),
  http_server(http_dispatch, [port(Port)]).


%% server_status(+Request) is det.
%
% Friendly message to let client know that the server is up.

server_status(_Request) :-
  format('Content-type: text/plain~n~n'),
  format('Server is up.~n').


%% send_status(+Status:string) is det.
%
% Takes a response status and sends the information to the client.

send_status(Status) :-
  format('Content-type: text/plain~n~n'),
  format('~s', [Status]).


%% disconnect is det.
%
% Shut down server on specified port and clean up information from top level.

disconnect :-
  port(Port),
  http_stop_server(Port, []),
  retractall(port(_)).


%--------------------------------------------------------------------------------%
% Queries
%--------------------------------------------------------------------------------%


%% login(+Query:compound) is det.
%
% Attempt login and send status back to client.

login(Query) :-
  http_parameters(Query, [name(User, [string])]),
  send_status(login $ User).


%% login(+Query:compound) is det.
%
% Attempt logout and send status back to client.

logout(Query) :-
  http_parameters(Query, [name(User, [string])]),
  send_status(logout $ User).


