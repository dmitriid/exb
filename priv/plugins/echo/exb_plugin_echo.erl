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
%% The module <strong>{@module}</strong> is a simple echo plugin.
%%
%% <h3>Description</h3>
%% It swaps "To:" and "From:" fields in an XMPP request to make sure that the outgoing
%% response will go back to the user that initiates the chat.
%%
%% It should be placed <strong>before</strong> any other plugn that modifies the 
%% request
%% 
%% <h3>Commands</h3>
%% None
%% 
%% <h3>Example chat session:</h3>
%% <pre>
%% User: hello
%% Bot: hello
%% </pre>
-module(exb_plugin_echo).

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

-spec(run/5 :: (any, any, list(), list(), list()) -> any).

run(Packet, Session, _Args, Chain, _Config) -> 
	NewPacket = Chain(Packet, Session),
	From = exmpp_xml:get_attribute(NewPacket, from, <<"unknown">>),
    To = exmpp_xml:get_attribute(NewPacket, to, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(NewPacket, from, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, From),
    NewPacket1 = exmpp_xml:remove_attribute(TmpPacket2, id),
	exmpp_session:send_packet(Session, NewPacket1),
	NewPacket1.


%%
%% Local Functions
%%

