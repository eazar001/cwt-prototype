
%%% Server


%--------------------------------------------------------------------------------%
% Commands
%--------------------------------------------------------------------------------%

% This creates a new user in the database, or updates a user token in the database
% USERNAME: The user to update
% ping USERNAME
% This updates a user token in the database, useful for keeping a user logged in
% This logs a user out of the system
% creategame USERNAME:POSITION:GAMENAME:PLAYERLIMIT:TEAMLAYOUT
% This creates a game in the system and creates the host player for the game.
% POSITION: The player army faction position in the game (the player order)
% GAMENAME: The title of this game
% PLAYERLIMIT: The total amount of players that can be in this game
% TEAMLAYOUT: The layout of the teams {"AB" would mean p1 is A team and p2 is B
% team.}
% joingame USERNAME:POSITION:GAMENAME
% This allows a player to join into an already created game
% addaction USERNAME:GAMENAME:P(0):P(1):...:P(n)
% This allows to player to push actions into the action list. An empty list will
% change to the next player turn.
% P(n): A section of a string, int, or float array. Can extend to as many as
% needed.
% leavegame USERNAME:GAMENAME
% In an inactive game, it'll remove the player from the list of players
% In an active game, it'll change the player to inactive making them unable to
% take any turns.


%--------------------------------------------------------------------------------%


% creategame USERNAME:POSITION:GAMENAME:PLAYERLIMIT:TEAMLAYOUT


:- module(database,
     [ attach_db/1 ]).


:- use_module(library(persistency)).
:- use_module(library(mavis)).


:- persistent
     user(name:atom).


attach_db(File) :-
  db_attach(File, []).



%% login(+User:atom, -Response:string) is det.
%
% A user has attempted to login. The status of the login will be unified with
% the appropriate Response.

login(User, Response) :-
  (
     current_user(User)
  ->
     login_response(user_exists, Response)
  ;
     login_response(no_user, Response),
     add_user(User),
     db_sync(reload)
  ).


%% logout(+User:atom, -Response:string) is det.
%
% A user has attempted to logout. The status of the logout will be unified with
% the appropriate Response. If the user already exists then response should be
% be success and User will be retracted from the database.

logout(User, Response) :-
  (
     current_user(User)
  ->
     db_sync(gc),
     remove_user(User),
     db_sync(reload),
     logout_response(user_exists, Response)
  ;
     logout_response(no_user, Response)
  ).


%--------------------------------------------------------------------------------%
% Reads
%--------------------------------------------------------------------------------%


%% current_user(+User:atom) is semidet.

current_user(User) :-
  with_mutex(user_db, user(User)).


%--------------------------------------------------------------------------------%
% Writes
%--------------------------------------------------------------------------------%


%% add_user(+User:atom) is semidet.

add_user(User) :-
  with_mutex(user_db, assert_user(User)).


%% remove_user(+User:atom) is semidet.

remove_user(User) :-
  with_mutex(user_db, retract_user(User)).


%--------------------------------------------------------------------------------%
% Responses
%--------------------------------------------------------------------------------%


%% login_response(+User:atom, -Response:string) is det.

login_response(no_user, "{ status:ok }").
login_response(user_exists, "{ status:fail }").


%% logout_response(+User:atom, -Response:string) is det.

logout_response(user_exists, "{ status:ok }").
logout_response(no_user, "{ status:fail }").


