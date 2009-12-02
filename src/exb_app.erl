%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%% @copyright 2009 Dmitrii Dimandt
%% @doc Callbacks for the exb application.

%% Copyright 2009 Dmitrii Dimandt
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

-module(exb_app).
-author('Dmitrii Dimandt <dmitrii@dmitriid.com>').

-behaviour(application).
-export([start/2, stop/1, get_path/0]).

-spec(ensure_started/1 :: (atom()) -> ok).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @doc Application start callback.

-spec(start/2 :: (any(), any()) -> {ok, pid()}).

start(_Type, _StartArgs) ->
	set_path(),
    ensure_started(exmpp),
	reloader:start(),
	
	Config = filename:join([exb_utils:lib_dir(), "config"]),
	Settings = exb_utils:read_settings(Config),
	
	exb_sup:start_link([Settings]).

%% @doc Application stop callback.

-spec(stop/1 :: (any()) -> ok).

stop(_State) ->
    ok.

%
% @doc Sets the environment variable lib_dir to application directory path
%
-spec(set_path/0 :: () -> any()).

set_path() ->
	P = code:all_loaded(),
	Path = filename:dirname(filename:dirname(proplists:get_value(?MODULE, P))),
	application:set_env(exb, lib_dir, Path).

%
% @doc Retrieves the application directory path
%

-spec(get_path/0 :: () -> string()).

get_path() ->
	application:get_env(exb, lib_dir).

	