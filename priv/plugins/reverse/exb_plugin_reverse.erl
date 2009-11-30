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
%% The module <strong>{@module}</strong> is a simple reverse plugin.
%%

-module(exb_plugin_reverse).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([run/5]).

%%
%% API Functions
%%

%%
%% TODO: Add description of run/function_arity
%%

-spec(run/5 :: (any, any, list(), list(), list()) -> any).

run(Packet, Session, _Args, Chain, _Config) ->
	NewPacket = case exmpp_message:is_message(Packet) of 
		true ->
			case exmpp_message:get_body(Packet) of
				undefined ->
					Packet;
				Binary ->
					Body = lists:reverse(binary_to_list(Binary)),
					N = exmpp_message:set_body(Packet, Body),
					N
			end;
		false ->
			Packet
	end,
	A = Chain(NewPacket, Session),
	A.

%%
%% Local Functions
%%

