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
%% The module <strong>{@module}</strong> implements an interface to module_info.
%%
%% <h3>Description</h3>
%% It will try to invoke <code>module_info(exports)</code> on the argument that's
%% passed in
%% 
%% <h3>Commands</h3>
%% !m ModuleName
%% 
%% <h3>Example chat session:</h3>
%% <pre>
%% User: !m exb
%% Bot: [{start,1},{stop,1},{init,1},{module_info,0},{module_info,1}]
%%
%% User: !m exb_utils
%% Bot: [{lib_dir,0},
%% {lib_dir,1},
%%  {plugin_dir,1},
%%  {read_settings,1},
%%  {module_info,0},
%%  {module_info,1},
%%  {plugin_atom,1}]
%%
%% User: !m non_existent
%% Bot: Eh? Could not find module_info for that
%% </pre>

-module(exb_plugin_m).

%%
%% Include files
%%

-include("exmpp_xml.hrl").

%%
%% Exported Functions
%%
-export([run/5]).

%%
%% API Functions
%%

-spec(run/5 :: (any, any, list(), list(), list()) -> any).

run(Packet, Session, _Args, Chain, _Config) ->
	NewPacket = case exmpp_message:is_message(Packet) of 
		true ->
			case exmpp_message:get_body(Packet) of
				undefined ->
					Packet;
				<<"!m ", Rest/binary>> ->
					ChildNodes = exmpp_xml:get_child_elements(Packet),
					Nodes = [Node || Node <- ChildNodes, Node#xmlel.name =/= html],
					Packet1 = exmpp_xml:set_children(Packet, Nodes),
					Response = try_m(Rest),
					exmpp_message:set_body(Packet1, Response);
				_ ->
					Packet
			end;
		false ->
			Packet
	end,
	A = Chain(NewPacket, Session),
	A.

%%
%% Local Functions
%%

try_m(Binary) ->
	try
		M = list_to_atom(binary_to_list(Binary)),
		R = io_lib:format("~p", [M:module_info(exports)]),
		R
	catch
		_:_ ->
			"Eh? Could not find module_info for that"
	end.