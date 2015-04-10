
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CWT-Prolog Server Interface %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(lambda)).
:- use_module(library(mavis)).
:- use_module(library(func)).
:- use_module(library(typedef)).
:- use_module(database).


:- type user ---> user(string).

:- type position ---> pos(between(1,4)).

:- type limit ---> limit(between(1,4)).

:- type game ---> game(string, string, limit, list(atom)).

:- type player ---> player(string, string, position, atom).

:- type action(T) ---> action(T).

% Basic header for plain text
header(text/plain, 'Content-type: text/plain~n~n').


%--------------------------------------------------------------------------------%
% Main Interface
%--------------------------------------------------------------------------------%


:- dynamic port/1.

:- http_handler(/, server_status, []).
:- http_handler('/login', login, []).
:- http_handler('/logout', logout, []).
:- http_handler('/ping', ping, []).
:- http_handler('/create_game', create_game, []).
:- http_handler('/join_game', join_game, []).
:- http_handler('/resign_game', resign_game, []).


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
  format(header $ text/plain),
  format('Server is up.~n').


%% send_status(+Status:string) is det.
%
% Takes a response status and sends the information to the client.

send_status(Status) :-
  format(header $ text/plain),
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
  send_status(login $ user(User)).


%% login(+Query:compound) is det.
%
% Attempt logout and send status back to client.

logout(Query) :-
  http_parameters(Query, [name(User, [string])]),
  send_status(logout $ user(User)).


%% ping(+Query:compound) is det.
%
% Receive ping from client with username.

ping(Query) :-
  http_parameters(Query, [name(User, [string])]),
  send_status(ping $ user(User)).


%% create_game(+Query:compound) is det.
%
% Create a game if all internal restrictions are met for creation.

create_game(Query) :-
  http_parameters(Query,
    [ user(User, [string])
     ,pos(Pos, [between(1,4)])
     ,game(Game, [string])
     ,limit(Limit, [between(1,4)])
     ,layout(Layout, [string]) ]),
  Code_char_lower = (\Code^Char^
    (to_lower(Code, Lower), char_code(Char, Lower))),
  Layout_codes = maplist(Code_char_lower) $ string_codes $ Layout,
  send_status(create_game(game(User, Game, limit(Limit), Layout_codes)) $
    pos(Pos)).


%% join_game(+Query:compound) is det.
%
% Allow a user to join a game if all internal restrictions are met for admission.

join_game(Query) :-
  http_parameters(Query,
    [ user(User, [string])
     ,pos(Pos, [between(1,4)])
     ,game(Game, [string]) ]),
  send_status(join_game(user(User), pos(Pos)) $ Game).


%% resign_game(+Query:compound) is det.
%
% Resign a user from a game.

resign_game(Query) :-
  http_parameters(Query,
    [ user(User, [string])
     ,game(Game, [string])
     ,pos(Pos, [between(1,4)]) ]),
  send_status(resign_game(user(User), Game) $ pos(Pos)).


