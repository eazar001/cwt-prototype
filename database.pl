
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CWT-Prolog Database API %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(database,
     [ attach_db/1 ]).


:- use_module(library(persistency)).
:- use_module(library(mavis)).
:- use_module(library(lambda)).


:- persistent
     user(name:atom).


%% attach_db(+File:atom) is det.

attach_db(File) :-
  db_attach(File, []).


%% login(+User:atom, -Response:string) is det.
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


%% logout(+User:atom, -Response:string) is det.
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


%--------------------------------------------------------------------------------%
% Reads
%--------------------------------------------------------------------------------%


%% current_user(+User:atom) is semidet.

current_user(User) :-
  with_mutex(user_db, user(User)).


%--------------------------------------------------------------------------------%
% Writes
%--------------------------------------------------------------------------------%


%% add_user(+User:string) is semidet.

add_user(User) :-
  Add_sync = (\User^(assert_user(User), db_sync(reload))),
  with_mutex(user_db, call(Add_sync, User)).


%% remove_user(+User:atom) is semidet.

remove_user(User) :-
  Remove_sync =
    (\User^(db_sync(gc), retract_user(User), db_sync(reload))),
  with_mutex(user_db, call(Remove_sync, User)).


%--------------------------------------------------------------------------------%
% Responses
%--------------------------------------------------------------------------------%


%% login_response(+User:atom, -Response:string) is det.

login_response(no_user, "{ status:ok }").
login_response(user_exists, "{ status:fail }").


%% logout_response(+User:atom, -Response:string) is det.

logout_response(user_exists, "{ status:ok }").
logout_response(no_user, "{ status:fail }").


