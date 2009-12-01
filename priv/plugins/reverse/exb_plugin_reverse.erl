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

-include("exmpp_xml.hrl").

%%
%% Exported Functions
%%
-export([run/5]).
-compile(export_all).

%%
%% API Functions
%%

-spec(run/5 :: (any, any, list(), list(), list()) -> any).

run(Packet, Session, _Args, Chain, _Config) ->
	NewPacket = case exmpp_message:is_message(Packet) of 
		true ->
			ChildNodes = [reverse(Node) || Node <- exmpp_xml:get_child_elements(Packet)],
			N = exmpp_xml:set_children(Packet, ChildNodes),
			N;
		false ->
			Packet
	end,
	A = Chain(NewPacket, Session),
	A.

%%
%% Local Functions
%%

%
% @doc Reverses the message body (both the body and html)
%
% An XMPP message may contain both the message body and an xhtml body,
% see http://xmpp.org/protocols/xhtml-im/
%
% So, in order to GET the message body, it is actualy enough to 
% invoke exmpp_message:get_body/1 on the entire Packet
%
% However, in order to SET the message body, you have to account for both
% the message body *and* the XHTML message body.
%
% That is why the simple reversal of a string becomes slightly nightmarish.
%
% Thanks to the good folks over at Process One, however, as the XHTML body
% is also parsed into #xmlel{} tuples, which makes the process easier
%

-spec(reverse/1 :: (xmlel()) -> xmlel()).

reverse(#xmlel{name=html} = Node) ->
	exmpp_xml:set_children(Node, [reverse_html(exmpp_xml:get_element(Node, body))]);
reverse(#xmlel{name=body} = Node) ->
	exmpp_xml:set_cdata(Node, reverse_body(Node));
reverse(Node) ->
	Node.

-spec(reverse_html/1 :: (xmlel()) -> xmlel()).

reverse_html(undefined) ->
	undefined;
reverse_html(XHTMLNode) ->
	ChildNodes = exmpp_xml:get_child_elements(XHTMLNode),
	exmpp_xml:set_children(XHTMLNode, 
		[exmpp_xml:set_cdata(Node, reverse_body(Node)) || Node <- ChildNodes]).

-spec(reverse_body/1 :: (xmlel()) -> string()).

reverse_body(Node) ->
	CData = exmpp_xml:get_cdata(Node),
	CDataChars = unicode:characters_to_list(CData),
	CDataReversed = lists:reverse(CDataChars),
	unicode:characters_to_binary(CDataReversed).

