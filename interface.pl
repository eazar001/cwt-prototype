%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CWT-Prolog Server Interface %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(database).


% Basic header for plain text
header('text/plain', 'Content-type: text/plain~n~n').


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
%  Attach to the specified database file, and start the server on the specified
%  port.
start_server(File, Port) :-
    must_be(between(1, 0xffff), Port),
    attach_db(File),
    db_sync(gc),
    asserta(port(Port)),
    http_server(http_dispatch, [port(Port)]).

%% server_status(+Request) is det.
%
%  Friendly message to let client know that the server is up.
server_status(_Request) :-
    header('text/plain', Header),
    format(Header),
    format('Server is up.~n').

%% send_status(+Status:string) is det.
%
%  Takes a response status and sends the information to the client.
send_status(Status) :-
    header('text/plain', Header),
    format(Header),
    format('~s', [Status]).

%% disconnect is det.
%
%  Shut down server on specified port and clean up information from top level.

disconnect :-
    port(Port),
    http_stop_server(Port, []),
    retractall(port(_)).


%--------------------------------------------------------------------------------%
% Queries
%--------------------------------------------------------------------------------%


%% login(+Query:compound) is det.
%
%  Attempt login and send status back to client.
login(Query) :-
    http_parameters(Query, [name(User, [string])]),
    login(user(User), Response),
    send_status(Response).

%% login(+Query:compound) is det.
%
%  Attempt logout and send status back to client.
logout(Query) :-
    http_parameters(Query, [name(User, [string])]),
    logout(user(User), Response),
    send_status(Response).

%% ping(+Query:compound) is det.
%
%  Receive ping from client with username.
ping(Query) :-
    http_parameters(Query, [name(User, [string])]),
    ping(user(User), Response),
    send_status(Response).

%% create_game(+Query:compound) is det.
%
%  Create a game if all internal restrictions are met for creation.
create_game(Query) :-
    http_parameters(Query, [
        user(User, [string]),
        pos(Pos, [between(1, 4)]),
        game(Game, [string]),
        limit(Limit, [between(1, 4)]),
        layout(Layout, [string])
    ]),
    string_codes(Layout, Codes),
    maplist(code_lower_char, Codes, Chars),
    create_game(game(User, Game, limit(Limit), Chars), pos(Pos), Response),
    send_status(Response).

code_lower_char(Code, Char) :-
    to_lower(Code, Lower),
    char_code(Char, Lower).

%% join_game(+Query:compound) is det.
%
%  Allow a user to join a game if all internal restrictions are met for admission.
join_game(Query) :-
    http_parameters(Query, [
        user(User, [string]),
        pos(Pos, [between(1, 4)]),
        game(Game, [string]) 
    ]),
    join_game(user(User), pos(Pos), Game, Response),
    send_status(Response).


%% resign_game(+Query:compound) is det.
%
%  Resign a user from a game.
resign_game(Query) :-
    http_parameters(Query, [
        user(User, [string]),
        game(Game, [string]),
        pos(Pos, [between(1, 4)])
    ]),
    resign_game(user(User), Game, pos(Pos), Response),
    send_status(Response).
