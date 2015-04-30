
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CWT-Prolog Database API %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(database,
     [ attach_db/1
      ,login/2
      ,logout/2
      ,ping/2
      ,create_game/3
      ,join_game/4
      ,resign_game/4
      ,add_action/4 ]).


:- use_module(library(persistency)).

:- persistent
     user(name:string).

:- persistent
     game(game:string, host:string, lim:limit, layout:list(atom)).

:- persistent
     player(name:string, game:string, pos:position, status:atom).


%--------------------------------------------------------------------------------%


% TBD: implement add_action/4.

% Predicates below are to be considered thread-safe *only if marked as such*.


%% attach_db(+File:atom) is det.

attach_db(File) :-
  db_attach(File, []).


%% login(+User:user, -Response:string) is det.
%
% A user has attempted to login via login USERNAME. The status of the login will
% be unified with the appropriate Response. (thread-safe)

login(user(User), Response) :-
  with_mutex(user_db, add_user(User, Response)).


%% logout(+User:user, -Response:string) is det.
%
% A user has attempted to logout via logout USERNAME. The status of the logout
% will be unified with the appropriate Response. If the user already exists then
% response should be be success and User will be retracted from the database.
% (thread-safe)

logout(user(User), Response) :-
  remove_user(User, Response).


%% ping(+User:user, -Response:string) is det.
%
% ping USERNAME
% This updates a user token in the database, useful for keeping a user logged in.
% (thread-safe)

ping(user(User), Response) :-
  (  current_user(User)
  -> response(success, Response)
  ;  response(failure, Response)
  ).


%% create_game(+Game:game, +Pos:position, -Response:string) is det.
%
% creategame USERNAME:POSITION:GAMENAME:PLAYERLIMIT:TEAMLAYOUT
% Create a game along with the host player for the game.
% Pos = The player army faction position in the game (the player order)
% Gamename = Title of the game
% Playerlimit = Total amount of players that can be in the game
% Teamlayout = Layout of the teams {"AB" would mean p1 is Team A and p2 is Team B.
% (thread-safe)

create_game(Game, Pos, Response) :-
  Game = game(User, Title, Limit, Layout),
  add_game(User, Pos, Title, Limit, Layout, Response).


%% join_game(+User:user, +Pos:position, +Game:string,
%%   -Response:string) is det.
%
% joingame USERNAME:POSITION:GAMENAME
% This allows a player to join an already created game.
% (thread-safe)

join_game(user(User), Pos, Game, Response) :-
  join_user(User, Pos, Game, Response).


%% resign_game(+User:user, +Game:string, +Pos:position, -Response:string) is det.
%
% leavegame USERNAME:GAMENAME
% In an inactive game, it'll remove the player from the list of players.
% In an active game, it'll change a player to inactive, making him/her unable to
% take any turns.
% (thread-safe)

resign_game(user(User), Game, Pos, Response) :-
  remove_player(User, Game, Pos, Response).


%% add_action(+User:string, +Game:string, +Actions:list, -Response:string) is det.
%
% addaction USERNAME:GAMENAME:P(0):P(1):...:P(n)
% This allows a player to push actions into the action list. An empty list will
% change to the next player turn.
% P(n): A section of a string, int, or float array. Can extend to as many as
% needed.

add_action(User, Game, Actions, Response) :-
  record_action(User, Game, Actions, Response).


%% active_game(+Game:string) is semidet.
%
% Active game iff more than one team is present in the game.

active_game(Game) :-
  game(Game, _, _, Layout),
  findall(Team,
    (player(_, Game, pos(Pos), active), nth1(Pos, Layout, Team)), [H|Teams]),
  \+maplist(call(=,H), Teams).


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
     response(success, Response)
  ).


%% remove_user(+User:string, -Response:string) is det.

remove_user(User, Response) :-
  (  retract_user(User)
  -> response(success, Response)
  ;  response(failure, Response)
  ).


%% add_game(+User:string, +Pos:position, +Game:string, +Limit:limit,
%%   +Layout:list(atom), -Response:string) is det.

add_game(User, Pos, Game, Limit, Layout, Response) :-
  (
     current_user(User),
     with_mutex(game_db, add_game_(User, Pos, Game, Limit, Layout))
  ->
     response(success, Response)
  ;
     response(failure, Response)
  ).

add_game_(User, Pos, Game, limit(Limit), Layout) :-
  \+current_game(Game),
  length(Layout, Limit),
  assert_game(Game, User, limit(Limit), Layout),
  assert_player(User, Game, Pos, active).


%% remove_player(+User:string, +Game:string, Pos:position,
%%   -Response:string) is det.

remove_player(User, Game, Pos, Response) :-
  with_mutex(game_db, remove_player_(User, Game, Pos, Response)).

remove_player_(User, Game, Pos, Response) :-
  (
     player(User, Game, Pos, active),
     active_game(Game)
  ->
     retract_player(User, Game, Pos, active),
     assert_player(User, Game, Pos, inactive)
   ;
     retract_player(User, Game, Pos, active)
  ),
  response(success, Response), !.  % Stop here, anything else is failure response

remove_player_(_, _, _, Response) :-
  response(failure, Response).


%% join_user(+User:string, +Pos:position, +Game:string, -Response:string) is det.

join_user(User, Pos, Game, Response) :-
  (
     current_user(User),
     with_mutex(game_db, join_user_(User, Pos, Game))
  ->
     response(success, Response)
  ;
     response(failure, Response)
  ).


join_user_(User, Pos, Game) :-
  % Game already exists
  game(Game, _, limit(Limit), _),
  % Player isn't already a part of the game
  \+player(User, Game, _, _),
  % Grab all positions and the total number of active players
  aggregate_all(bag(P)-count, player(_, Game, P, active), Ps-Players),
  \+memberchk(Pos, Ps),
  % Adding User or Pos shouldn't break the player limit
  Pos = pos(Slot),
  Slot =< Limit,
  Players < Limit,
  assert_player(User, Game, Pos, active).


%--------------------------------------------------------------------------------%
% Responses
%--------------------------------------------------------------------------------%


%% response(+Status:atom, -Response:string) is det.

response(success, "{ status:ok }").
response(failure, "{ status:fail }").


