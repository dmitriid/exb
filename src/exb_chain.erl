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

%
% @doc Creates a chain of plugins
%
% <strong>Usage:</strong>
% <pre><code lang="erlang">
% &gt; exb_chain:plugin_chain([{default, [plugin1, plugin2, plugin3]}]).
% [{default,#Fun&lt;exb_chain.0.64534256&gt;}]</code></pre>
%
% The resulting function is a continuation-passing style function. When invoked
% it calls the next plugin in chain and passes to it a new function which will
% call the next plugin in chain etc.

-spec(plugin_chain/1 :: (list()) -> list()).

plugin_chain(Plugins) ->
	[{Name, create_chain(PluginList)} || {Name, PluginList} <- Plugins].

%%
%% Local Functions
%%


% @doc Handles the actual creation of the chain of plugins
%
% Creates a continuation passing style function to call each plugin in succession


-spec(create_chain/1 :: (list()) -> fun()).

create_chain(PluginList) -> 
	PluginAtoms = [exb_utils:plugin_atom(Plugin) || Plugin <- PluginList],
	F = fun(Request, Session) ->
	        stack(Request, Session, PluginAtoms)
	    end,
	F.

% @doc Creates the CPS function and calls the current plugin

-spec(stack/3 :: (any, any, list()) -> any).

stack(Request, _Session, []) ->
	Request;
stack(Request, Session, [Plugin|T]) ->
	F = fun(R, S) ->
			stack(R, S, T)
		end,
	run(Plugin, Request, Session, F).

% @doc Runs the current plugin

-spec(run/4 :: (any, any, any, fun()) -> any).

run(Module, Request, Session, F) when is_atom(Module) ->
	exb_plugin:run(Module, Request, Session, [], F);
%	Module:run(Request, Session, [], F);
 
run({Module}, Request, Session, F) ->
	exb_plugin:run(Module, Request, Session, [], F);
%	Module:run(Request, Session, [], F);
 
run({Module, Options}, Request, Session, F) ->
	exb_plugin:run(Module, Request, Session, Options, F).
%	Module:run(Request, Session, Options, F).

