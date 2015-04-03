
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CWT-Prolog Database API %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(database,
     [ attach_db/1
      ,login/2
      ,logout/2
      ,ping/2
      ,create_game/6
      ,join_game/4
      ,resign_game/3
      ,add_action/4 ]).


:- use_module(library(persistency)).

:- persistent
     user(name:string).

:- persistent
     game(game:string, host:string, limit:between(1,4), layout:string).

:- persistent
     player(name:string, game:string, pos:between(1,4)).


%--------------------------------------------------------------------------------%


% TBD: double-check constraints on some of the game-based predicates to make
% sure nothing strange is going on during the requests.

% TBD: reimplement a few of the game-based predicates for efficiency boosts.

% TBD: implement add_action/4.

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
  remove_user(User, Response).


%% ping(+User:string, -Response:string) is det.
%
% ping USERNAME
% This updates a user token in the database, useful for keeping a user logged in.
% (thread-safe)

ping(User, Response) :-
  (  current_user(User)
  -> response(success, Response)
  ;  response(failure, Response)
  ).


%% create_game(+User:string, +Pos:between(1,4), +Game:string, Limit:between(1,4),
%%   +Layout:string, -Response:string) is det.
%
% creategame USERNAME:POSITION:GAMENAME:PLAYERLIMIT:TEAMLAYOUT
% Create a game along with the host player for the game.
% Pos = The player army faction position in the game (the player order)
% Gamename = Title of the game
% Playerlimit = Total amount of players that can be in the game
% Teamlayout = Layout of the teams {"AB" would mean p1 is Team A and p2 is Team B.
% (thread-safe)

create_game(User, Pos, Game, Limit, Layout, Response) :-
  add_game(User, Pos, Game, Limit, Layout, Response).


%% join_game(+User:string, +Pos:between(1,4), +Game:string,
%%   -Response:string) is det.
%
% joingame USERNAME:POSITION:GAMENAME
% This allows a player to join an already created game.
% (thread-safe)

join_game(User, Pos, Game, Response) :-
  join_user(User, Pos, Game, Response).


%% resign_game(+User:string, +Game:string, +Pos:between(1,4),
%%   -Response:string) is det.
%
% leavegame USERNAME:GAMENAME
% In an inactive game, it'll remove the player from the list of players..
% In an active game, it'll change a player to inactive, making him/her unable to
% take any turns.

resign_game(User, Game, Pos, Response) :-
  remove_player(User, Game, Pos, Response).


%% add_action(+User:string, +Game:string, +Actions:list, -Response:string) is det.
%
% addaction USERNAME:GAMENAME:P(0):P(1):...:P(n)
% This allows to player to push actions into the action list. An empty list will
% change to the next player turn.
% P(n): A section of a string, int, or float array. Can extend to as many as
% needed.

add_action(User, Game, Actions, Response) :-
  true.


%% active_game(+Layout) is semidet.
%
% Active game iff more than one team is present in the game.

active_game([H|Layout]) :-
  \+maplist(call(=,H), Layout).


%% with_game_db(:Goal) is semidet.
%
% Execute any Goal while holding mutex for sections critical to game data only.

with_game_db(Goal) :-
  with_mutex(game_db, Goal).


%--------------------------------------------------------------------------------%
% Reads
%--------------------------------------------------------------------------------%


%% current_user(+User:string) is semidet.

current_user(User) :-
  user(User).


%% current_game(+Game:string) is semidet.

current_game(Game) :-
  game(Game, _, _, _).


%--------------------------------------------------------------------------------%
% Writes
%--------------------------------------------------------------------------------%


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
  (  with_mutex(user_db, remove_user_(User))
  -> response(success, Response)
  ;  response(failure, Response)
  ).

remove_user_(User) :-
  db_sync(gc),
  retract_user(User),
  db_sync(reload).


%% add_game(+User:string, +Pos:between(1,4), +Game:string, +Limit:between(1,4),
%%   +Layout:string, -Response:string) is det.

add_game(User, Pos, Game, Limit, Layout, Response) :-
  current_user(User),
  (  with_mutex(game_db, add_game_(Pos, Game, Limit, Layout))
  -> response(success, Response)
  ;  response(failure, Response)
  ).

add_game_(Pos, Game, Limit, Layout) :-
  \+current_game(Game),
  string_length(Layout, Limit),
  assert_game(Game, User, Limit, Layout).


%% resign_user(+User:string, +Game:string, -Response:string) is det.

remove_player(User, Game, Pos, Response) :-
  (  retract_player(User, Game, Pos)
  -> response(success, Response)
  ;  response(failure, Response)
  ).


%% join_user(+User:string, +Pos:between(1,4), +Game:string,
%%   -Response:string) is det.

join_user(User, Pos, Game, Response) :-
  current_user(User),
  (  with_mutex(game_db, join_user_(User, Pos, Game))
  -> response(success, Response)
  ;  response(failure, Response)
  ).

join_user_(User, Pos, Game) :-
  % Game already exists
  game(Game, _Host, Limit, _Layout),
  % Player isn't already a part of the game
  \+player(User, Game, _),
  findall(_Player, player(_, Game, _), Players),
  % Adding User shouldn't break the player limit
  length(Players, Length),
  New is Length + 1,
  New =< Limit.


%--------------------------------------------------------------------------------%
% Responses
%--------------------------------------------------------------------------------%


%% response(+Status:atom, -Response:string) is det.

response(success, "{ status:ok }").
response(failure, "{ status:fail }").


