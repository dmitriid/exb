%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%% @copyright 2009 Dmitrii Dimandt
%% @doc Supervisor for the exb application.

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

-module(exb_plugin_sup).
-author('Dmitrii Dimadt <dmitrii@dmitriid.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/1, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @doc API for starting the supervisor.
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @doc supervisor callback.
init(Config) ->
	PluginTimeout = proplists:get_value(plugin_timeout, Config, 1000),
	AllPlugins = proplists:get_value(plugins, Config, []),
	Plugins = plugins(AllPlugins),
	
	PluginProcesses = [
					   plugin_process(Plugin, PluginTimeout) || Plugin <- Plugins
					   ],
	
    {ok, {{one_for_one, 1000, 10}, PluginProcesses}}.

plugins(PluginList) ->
	Plugins = lists:flatten([
	    [exb_utils:plugin_atom(P) || P <- Plugins] || {_ ,Plugins} <- PluginList 
	]),
	lists:usort(Plugins).

plugin_process(Plugin, PluginTimeout) ->
	{Plugin, 
	 {exb_plugin, start_link, [[Plugin, PluginTimeout]]}, 
	 permanent, 1000, worker, [exb_plugin]}.