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
%% The module <strong>{@module}</strong> creates a redy-to-use gen_server 
%% wrapper around an exb plugin.
%%

-module(exb_plugin).

-behaviour(gen_server).


%% --------------------------------------------------------------------
%% Record definitions
%% --------------------------------------------------------------------
-record(state, {plugin=not_implemented, config=[]}).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([run/5]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).


%% ====================================================================
%% External functions
%% ====================================================================

%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link([Plugin, _] = Args) when is_list(Args) ->
    gen_server:start_link({local, Plugin}, ?MODULE, Args, [Args]).

%% ====================================================================
%% Server functions
%% ====================================================================

run(Plugin, Packet, Session, Args, Chain) ->
	gen_server:call(Plugin, {run, [Packet, Session, Args, Chain]}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Plugin, PluginTimeout]) ->
	PathToConfig = exb_utils:plugin_dir(Plugin),
	Config = exb_utils:read_settings(PathToConfig),
	Timeout = proplists:get_value(timeout, Config, PluginTimeout),
    {ok, #state{plugin=Plugin, config=Config}, Timeout}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({run, [Packet, Session, Args, Chain]}, _From, State) ->
    Plugin = State#state.plugin,
    Config = State#state.config,
	Reply = Plugin:run(Packet, Session, Args, Chain, Config),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

