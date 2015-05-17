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


-module(awre_transport).

-export([init/1]).


-callback init(Args :: map()) -> {ok,State :: any()}.
-callback send_to_router(Message :: term(), State :: any()) -> {ok, NewState :: any()}.
-callback handle_info(Data :: any(), State :: any()) -> {ok, NewState :: any()}.
-callback shutdown(State :: any()) -> ok.



init(Args) ->
  #{host := Host} = Args,
  Module = case Host == undefined of
             true ->
               awre_trans_local;
             false ->
               awre_trans_tcp
           end,
  {ok,State} = Module:init(Args),
  {Module,State}.
