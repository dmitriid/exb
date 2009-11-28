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
%% The module <strong>{@module}</strong> implements the exb bot.

-module(exb).

-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([start/0, stop/1]).
-export([init/0]).

-spec(start/0 :: () -> pid()).

start() ->
	init().
    %%spawn(?MODULE, init, []).

-spec(stop/1 :: (pid) -> any).

stop(EchoClientPid) ->
    EchoClientPid ! stop.

init() ->
	Config = filename:join([exb_utils:lib_dir(), "config"]),
	Settings = exb_utils:read_settings(Config),
	
	Node     = proplists:get_value(node, Settings),
	Domain   = proplists:get_value(domain, Settings),
	Resource = proplists:get_value(resource, Settings, ""),
	Server   = proplists:get_value(server, Settings),
	Port     = proplists:get_value(port, Settings),
	Password = proplists:get_value(password, Settings),
	AuthType = proplists:get_value(auth_type, Settings),
	
	Plugins  = proplists:get_value(plugins, Settings),
	PluginChain = exb_chain:plugin_chain(Plugins),

    %% Start XMPP session: Needed to start service (like exmpp_stringprep):
    MySession = exmpp_session:start(),
	
    %% Create XMPP ID (Session Key):
    MyJID = case Resource of
				"" -> exmpp_jid:make(Node, Domain);
				_ -> exmpp_jid:make(Node, Domain, Resource)
			end,

	%% Create a new session with basic (digest) authentication:
    exmpp_session:AuthType(MySession, MyJID, Password),
    %% Connect in standard TCP:
    _StreamId = exmpp_session:connect_TCP(MySession, Server, Port),

    session(MySession, MyJID, PluginChain).

%% We are connected. We now log in (and try registering if authentication fails)
session(MySession, _MyJID, PluginChain) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    %% Try creating a new user:
	    io:format("Register~n",[]),
	    %% In a real life client, we should trap error case here
	    %% and print the correct message.
	    exmpp_session:register_account(MySession, "password"),
	    %% After registration, retry to login:
	    exmpp_session:login(MySession)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "Echo Ready")),
    loop(MySession, PluginChain).

%% Process exmpp packet:
loop(MySession, PluginChain) ->
    receive
        stop ->
            exmpp_session:stop(MySession);
        %% If we receive a message, we reply with the same message
        Record = #received_packet{packet_type=message, raw_packet=Packet} ->
            io:format("~p~n", [Record]),
            handle(MySession, Packet, PluginChain),
            loop(MySession, PluginChain);
        Record ->
            io:format("~p~n", [Record]),
            loop(MySession, PluginChain)
    end.

%% Send the same packet back for each message received
handle(MySession, Packet, PluginChain) ->
    From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
	Plugin = proplists:get_value(From, PluginChain, proplists:get_value(default, PluginChain)),
	NewPacket =
	try
		Plugin(Packet, MySession)
	catch
		Any:Reason ->
		    From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
    		To = exmpp_xml:get_attribute(Packet, to, <<"unknown">>),
    		TmpPacket = exmpp_xml:set_attribute(Packet, from, To),
    		TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, From),
    		NewPacket1 = exmpp_xml:remove_attribute(TmpPacket2, id),
			exmpp_xml:set_cdata(NewPacket1, <<"error",$\n,Any,$\n,Reason>>)
	end,
    exmpp_session:send_packet(MySession, NewPacket).

echo(MySession, Packet, _) ->
    From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, to, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, from, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, id),
    exmpp_session:send_packet(MySession, NewPacket).
