%%
%% Copyright (c) 2014 Bas Wegh
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

-module(client_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).


%% Tests.
-export([client_test/1]).

%% ct.

-define(REALM,<<"internal.client.test">>).


all() ->
	[client_test].


init_per_suite(Config) ->
  {ok,_} = application:ensure_all_started(erwa),
  {ok,_} = application:ensure_all_started(awre),
  ok = erwa:start_realm(?REALM,erwa_mw_allow),
  Config.

end_per_suite(Config) ->
  {ok,_} = erwa:stop_realm(?REALM),
  Config.


client_test(_) ->
  {ok,Routing} = erwa_realms:get_routing(?REALM),
  {ok,Pid} = gen_server:start(client_simple,?REALM,[]),
  EventUrl = client_simple:get_event_url(),
  RpcUrl = client_simple:get_rpc_url(),
  ct:log("starting client test (pid: ~p)~n",[self()]),
  Session = erwa_session:set_id(234,erwa_session:create()),
  {ok,Broker} = erwa_routing:get_broker(Routing),
  {ok,Dealer} = erwa_routing:get_dealer(Routing),

  erwa_broker:publish(),
  ok = erwa_router:handle_wamp(Router,{publish,1,#{},EventUrl,undefined,undefined}),
  ok = erwa_router:handle_wamp(Router,{call,2,#{},RpcUrl,[5,9],undefined}),
  ok =
    receive
      {erwa,{result,2,_,[14],_}} -> ok
     after 1000 ->
       timeout
    end,
  true = gen_server:call(Pid,{all_done}),
  ok.


