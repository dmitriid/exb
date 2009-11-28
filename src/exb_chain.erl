%% Copyright Dmitrii Dimandt 2009. All Rights Reserved.
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

%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>

%% @doc
%% The module <strong>{@module}</strong> creates a chainable list of plugins.
%%
%% <p>
%% Usage:
%% </p>
%% <pre>Chain = exb_chain:create([Plugin1, Plugin2, Plugin3]).</pre>

-module(exb_chain).

%%
%% Include files
%%

-include_lib("exmpp/include/exmpp.hrl").

%%
%% Exported Functions
%%
-export([plugin_chain/1]).

%%
%% API Functions
%%

-spec(plugin_chain/1 :: (list()) -> list()).

plugin_chain(Plugins) ->
	[{Name, create(Chain)} || {Name, Chain} <- Plugins].

%%
%% Local Functions
%%

-spec(create/1 :: (list()) -> fun()).

create(Plugins) -> 
	PluginAtoms = [plugin_atom(Plugin) || Plugin <- Plugins],
	F = fun(Request, Session) ->
	        stack(Request, Session, PluginAtoms)
	    end,
	F.

-spec(stack/3 :: (any, any, list()) -> any).

stack(Request, _Session, []) ->
	Request;
stack(Request, Session, [Plugin|T]) ->
	F = fun(R, S) ->
			stack(R, S, T)
		end,
	run(Plugin, Request, Session, F).
 
-spec(run/4 :: (any, any, any, fun()) -> any).

run(Module, Request, Session, F) when is_atom(Module) ->
	Module:run(Request, Session, [], F);
 
run({Module}, Request, Session, F) ->
	Module:run(Request, Session, [], F);
 
run({Module, Options}, Request, Session, F) ->
	Module:run(Request, Session, Options, F).

-spec(plugin_atom/1 :: (atom() | list()) -> atom()).

plugin_atom(Plugin) when is_atom(Plugin) ->
	plugin_atom(atom_to_list(Plugin));
plugin_atom(Plugin) ->
	list_to_atom("exb_plugin_" ++ Plugin).
