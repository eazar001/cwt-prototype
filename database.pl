
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CWT-Prolog Database API %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(database,
     [ attach_db/1
      ,login/2
      ,logout/2
      ,create_game/5
      ,join_game/3
      ,add_action/3 ]).


:- use_module(library(persistency)).

:- persistent
     user(name:string).


%--------------------------------------------------------------------------------%


%% attach_db(+File:atom) is det.

attach_db(File) :-
  db_attach(File, []).


%% login(+User:string, -Response:string) is det.
%
% A user has attempted to login via login USERNAME. The status of the login will
% be unified with the appropriate Response.

login(User, Response) :-
  (
     current_user(User)
  ->
     login_response(user_exists, Response)
  ;
     add_user(User),
     login_response(no_user, Response)
  ).


%% logout(+User:string, -Response:string) is det.
%
% A user has attempted to logout via logout USERNAME. The status of the logout
% will be unified with the appropriate Response. If the user already exists then
% response should be be success and User will be retracted from the database.

logout(User, Response) :-
  (
     current_user(User)
  ->
     remove_user(User),
     logout_response(user_exists, Response)
  ;
     logout_response(no_user, Response)
  ).


%% create_game(+User, +Pos, +Gamename, +Playerlimit, +Teamlayout) is det.
%
% creategame USERNAME:POSITION:GAMENAME:PLAYERLIMIT:TEAMLAYOUT
% Create a game along with the host player for the game.
% Pos = The player army faction position in the game (the player order)
% Gamename = Title of the game
% Playerlimit = Total amount of players that can be in the game
% Teamlaout = Layout of the teams {"AB" would mean p1 is Team A and p2 is Team B.

create_game(User, Pos, Gamename, Playerlimit, Teamlayout) :-
  true.


% join_game(+User, +Pos, +Gamename) is det.
%
% joingame USERNAME:POSITION:GAMENAME
% This allows a player to join an already created game.

join_game(User, Pos, Gamename) :-
  true.


% leave_game(+User, +Gamename) is det.
%
% leavegame USERNAME:GAMENAME
% In an inactive game, it'll remove the player from the list of players..
% In an active game, it'll change a player to inactive, making him/her unable to
% take any turns.

leave_game(User, Gamename) :-
  true.


%% add_action(+User, +Gamename, +Actions) is det.
%
% addaction USERNAME:GAMENAME:P(0):P(1):...:P(n)
% This allows to player to push actions into the action list. An empty list will
% change to the next player turn.
% P(n): A section of a string, int, or float array. Can extend to as many as
% needed. 

add_action(User, Gamename, Actions) :-
  true.


%--------------------------------------------------------------------------------%
% Reads
%--------------------------------------------------------------------------------%


%% current_user(+User:string) is semidet.

current_user(User) :-
  with_mutex(user_db, user(User)).


%--------------------------------------------------------------------------------%
% Writes
%--------------------------------------------------------------------------------%


%% add_user(+User:string) is semidet.

add_user(User) :-
  Add_sync = (\User^(assert_user(User), db_sync(reload))),
  with_mutex(user_db, call(Add_sync, User)).


%% remove_user(+User:string) is semidet.

remove_user(User) :-
  Remove_sync =
    (\User^(db_sync(gc), retract_user(User), db_sync(reload))),
  with_mutex(user_db, call(Remove_sync, User)).


%--------------------------------------------------------------------------------%
% Responses
%--------------------------------------------------------------------------------%


%% login_response(+Status:atom, -Response:string) is det.

login_response(no_user, "{ status:ok }").
login_response(user_exists, "{ status:fail }").


%% logout_response(+Status:atom, -Response:string) is det.

logout_response(user_exists, "{ status:ok }").
logout_response(no_user, "{ status:fail }").


