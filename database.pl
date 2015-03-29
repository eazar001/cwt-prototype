
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CWT-Prolog Database API %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(database,
     [ attach_db/1
      ,login/2
      ,logout/2
      ,ping/2
      ,create_game/5
      ,join_game/3
      ,resign_game/2
      ,add_action/3 ]).


:- use_module(library(persistency)).

:- persistent
     user(name:string).


%--------------------------------------------------------------------------------%


% Predicates below are to be considered thread-safe *only if marked as such*.


%% attach_db(+File:atom) is det.

attach_db(File) :-
  db_attach(File, []).


%% login(+User:string, -Response:string) is det.
%
% A user has attempted to login via login USERNAME. The status of the login will
% be unified with the appropriate Response. (thread-safe)

login(User, Response) :-
  with_mutex(user_db, add_user(User, Response)).


%% logout(+User:string, -Response:string) is det.
%
% A user has attempted to logout via logout USERNAME. The status of the logout
% will be unified with the appropriate Response. If the user already exists then
% response should be be success and User will be retracted from the database.
% (thread-safe)

logout(User, Response) :-
  with_mutex(user_db, remove_user(User, Response)).


%% ping(+User:string, -Response:string) is det.
%
% ping USERNAME
% This updates a user token in the database, useful for keeping a user logged in.
% (thread-safe)

ping(User, Response) :-
  with_mutex(user_db, check_ping(User, Response)).


%% create_game(+User, +Pos, +Gamename, +Playerlimit, +Teamlayout) is det.
%
% creategame USERNAME:POSITION:GAMENAME:PLAYERLIMIT:TEAMLAYOUT
% Create a game along with the host player for the game.
% Pos = The player army faction position in the game (the player order)
% Gamename = Title of the game
% Playerlimit = Total amount of players that can be in the game
% Teamlayout = Layout of the teams {"AB" would mean p1 is Team A and p2 is Team B.

create_game(_User, _Pos, _Gamename, _Playerlimit, _Teamlayout) :-
  true.


% join_game(+User, +Pos, +Gamename) is det.
%
% joingame USERNAME:POSITION:GAMENAME
% This allows a player to join an already created game.

join_game(_User, _Pos, _Gamename) :-
  true.


% resign_game(+User, +Gamename) is det.
%
% leavegame USERNAME:GAMENAME
% In an inactive game, it'll remove the player from the list of players..
% In an active game, it'll change a player to inactive, making him/her unable to
% take any turns.

resign_game(_User, _Gamename) :-
  true.


%% add_action(+User, +Gamename, +Actions) is det.
%
% addaction USERNAME:GAMENAME:P(0):P(1):...:P(n)
% This allows to player to push actions into the action list. An empty list will
% change to the next player turn.
% P(n): A section of a string, int, or float array. Can extend to as many as
% needed.

add_action(_User, _Gamename, _Actions) :-
  true.


%% active_game(+Layout) is semidet.
%
% Active game iff more than one team is present in the game.

active_game([H|Layout]) :-
  \+maplist(call(=,H), Layout).

			
%--------------------------------------------------------------------------------%
% Reads
%--------------------------------------------------------------------------------%


%% current_user(+User:string) is semidet.

current_user(User) :-
  user(User).


%--------------------------------------------------------------------------------%
% Writes
%--------------------------------------------------------------------------------%


%% check_ping(+User:string, -Response:string) is det.

check_ping(User, Response) :-
  (  current_user(User)
  -> response(success, Response)
  ;  response(failure, Response)
  ).


%% add_user(+User:string, -Response:string) is det.

add_user(User, Response) :-
  (
     current_user(User)
  ->
     response(failure, Response)
  ;
     assert_user(User),
     db_sync(reload),
     response(success, Response)
  ).
 

%% remove_user(+User:string, -Response:string) is det.

remove_user(User, Response) :-
  (
     current_user(User)
  ->
     db_sync(gc),
     retract_user(User),
     db_sync(reload),
     response(success, Response)
  ;
     response(failure, Response)
  ).


%--------------------------------------------------------------------------------%
% Responses
%--------------------------------------------------------------------------------%


%% response(+Status:atom, -Response:string) is det.

response(success, "{ status:ok }").
response(failure, "{ status:fail }").


