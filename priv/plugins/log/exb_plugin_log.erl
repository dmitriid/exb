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
%% The module <strong>{@module}</strong> is a simple logging plugin.
%%
%% <h3>Description</h3>
%% It logs all incoming and outgoing packets.
%%
%% It should be placed <strong>before</strong> any other plugin in the chain
%% 
%% In it's default implementation the log plugin places all log data in it's own 
%% directory, so make sure it's writable
%% 
%% <h3>Commands</h3>
%% None
%% 
%% <h3>Example chat session:</h3>
%% <pre>
%% User: hello
%% Bot: hello
%% </pre>
%% Both messages are logged to exb/priv/plugins/log/logs/User/Year.Month.Day.Hour.log
%%

-module(exb_plugin_log).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([run/5, log/4]).

%%
%% API Functions
%%

-spec(run/5 :: (any, any, list(), list(), list()) -> any).

run(Packet, Session, Args, Chain, Config) ->
	Log = proplists:get_value(logger, Args, proplists:get_value(logger, Config, fun ?MODULE:log/4)),
	
	Log(Packet, Session, Args, Config),
	NewPacket = Chain(Packet, Session),
	Log(Packet, Session, Args, Config),
	
	NewPacket.

%
% @doc Simply dumps all packets to a log file
%

-spec(log/4 :: (any, any, list(), list()) -> any).

log(Packet, _Session, _Args, _Config) ->
	PluginDir = exb_utils:plugin_dir(?MODULE),
	FromJID = exmpp_jid:parse(exmpp_xml:get_attribute_as_list(Packet, from, <<"unknown">>)),
	
	From = io_lib:format("~s.~s/",[exmpp_jid:node_as_list(FromJID), exmpp_jid:domain_as_list(FromJID)]),
	
	Path = filename:join([PluginDir, "logs", From]),

	filelib:ensure_dir(Path ++ "/"),
	
	{{Year,Month,Day},{Hour,_,_}} = erlang:localtime(),
	FileName = io_lib:format("~p.~p.~p.~p.log", [Year, Month, Day, Hour]),
	LogFile = filename:join([Path, FileName]),
	
	
	
	Log = io_lib:format("~p.~n", [Packet]),
	
	file:write_file(LogFile, [Log], [append]).

%%
%% Local Functions
%%

