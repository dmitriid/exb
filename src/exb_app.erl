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

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for zotonic.
start(_Type, _StartArgs) ->
	set_path(),
    ensure_started(exmpp),
	reloader:start(),
	exb_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for zotonic.
stop(_State) ->
    ok.

set_path() ->
	P = code:all_loaded(),
	Path = filename:dirname(filename:dirname(proplists:get_value(?MODULE, P))),
	application:set_env(exb, lib_dir, Path).

get_path() ->
	application:get_env(exb, lib_dir).

	