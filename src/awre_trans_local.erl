%%
%% Copyright (c) 2015 Bas Wegh
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%


-module(awre_trans_local).
-behaviour(awre_transport).

-export([init/1]).
-export([send_to_router/2]).
-export([handle_info/2]).
-export([shutdown/1]).

-record(state,{
               awre_con = none,
               version = unknown,
               client_details = unknown,
               session = none
               }).


init(Args) ->
  #{realm := Realm, awre_con := Con, client_details := CDetails, version := Version} = Args,
  Session = erwa_session:set_source(local,erwa_session:create()),
  State = #state{session=Session,awre_con=Con, version = Version, client_details=CDetails},
  send_to_router({hello,Realm,#{version => Version, roles => CDetails}},State).

send_to_router(MsgToRouter, #state{session=Session,awre_con=Con} = State) ->
  case erwa_session:handle_message(MsgToRouter,Session) of
    {ok,NewSession} ->
      {ok,State#state{session=NewSession}};
    {stop,NewSession} ->
      awre_con:close_connection(Con),
      {ok,State#state{session=NewSession}};
    {reply,Msg,NewSession} ->
      awre_con:send_to_client(Msg,Con),
      {ok,State#state{session=NewSession}};
    {reply_stop,Msg,NewSession} ->
      awre_con:send_to_client(Msg,Con),
      awre_con:close_connection(Con),
      {ok,State#state{session=NewSession}}
  end.

handle_info({erwa,MsgFromRouter},#state{session=Session,awre_con=Con}=State) ->
  case erwa_session:handle_info(MsgFromRouter,Session) of
    {ok,NewSession} ->
      {ok,State#state{session=NewSession}};
    {stop,NewSession} ->
      awre_con:close_connection(Con),
      {ok,State#state{session=NewSession}};
    {send,Msg,NewSession} ->
      awre_con:send_to_client(Msg,Con),
      {ok,State#state{session=NewSession}};
    {send_stop,Msg,NewSession} ->
      awre_con:send_to_client(Msg,Con),
      awre_con:close_connection(Con),
      {ok,State#state{session=NewSession}}
  end.

shutdown(_State) ->
  ok.
