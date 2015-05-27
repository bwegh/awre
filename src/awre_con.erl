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

%% @private
-module(awre_con).
-behaviour(gen_server).


-export([send_to_client/2]).
-export([close_connection/1]).

%% API.
-export([start_link/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(CLIENT_DETAILS, #{
                          callee => #{features => #{}},
                          caller => #{features => #{}},
                          publisher => #{features => #{}},
                          subscriber => #{features => #{}}
                          }).

-record(state,{
               ets = undefined,
               goodbye_sent = false,

               transport = {none,none},

               subscribe_id=1,
               unsubscribe_id=1,
               publish_id=1,
               register_id=1,
               unregister_id=1,
               call_id=1

  }).

-record(ref, {
              key = {none,none},
              req = undefined,
              method = undefined,
              ref=undefined,
              args = []
              }).

-record(subscription,{
  id = undefined,
  mfa = undefined,
  pid=undefined}).

-record(registration,{
  id = undefined,
  mfa = undefined,
  pid = undefined
                      }).


-spec start_link(Args :: map()) -> {ok, pid()}.
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).



-spec init(Args :: map() ) -> {ok,#state{}}.
init(_Args) ->
  Ets = ets:new(con_data,[set,protected,{keypos,2}]),
  {ok,#state{ets=Ets}}.


-spec send_to_client(Msg :: term(), Pid :: pid()) -> ok.
send_to_client(Msg,Pid) ->
  gen_server:cast(Pid,{awre_out,Msg}).


close_connection(Pid) ->
  gen_server:cast(Pid,terminate).


-spec handle_call(Msg :: term(), From :: term(), #state{}) -> {reply,Msg :: term(), #state{}}.
handle_call({awre_call,Msg},From,State) ->
  handle_message_from_client(Msg,From,State);
handle_call(_Msg,_From,State) ->
  {noreply,State}.


handle_cast({awre_out,Msg}, State) ->
  {ok,NewState} = handle_message_from_router(Msg,State),
  {noreply,NewState};
handle_cast({shutdown,Details,Reason}, #state{goodbye_sent=GS,transport= {TMod,TState}}=State) ->
  NewState = case GS of
               true ->
                 State;
               false ->
                 {ok,NewTState} = TMod:send_to_router({goodbye,Details,Reason},TState),
                 State#state{transport={TMod,NewTState}}
             end,
  {noreply,NewState#state{goodbye_sent=true}};

handle_cast(terminate,#state{transport={TMod,TState}} = State) ->
  ok = TMod:shutdown(TState),
  {stop,normal,State};

handle_cast(_Request, State) ->
	{noreply, State}.





handle_info(Data,#state{transport = {T,TState}} = State) ->
  {ok,NewTState} = T:handle_info(Data,TState),
  {noreply,State#state{transport={T,NewTState}}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


-spec handle_message_from_client(Msg :: term(), From :: term(), State :: #state{}) ->
  {noreply, #state{}} | {reply, Result :: term(), #state{}}.
handle_message_from_client({connect,Host,Port,Realm,Encoding}=Msg,From,
                           #state{transport={T,_}}=State) ->

  Args = #{awre_con => self(), host => Host, port => Port, realm => Realm, enc => Encoding,
           version => awre:get_version(), client_details => ?CLIENT_DETAILS},
  {Trans,TState} = case T of
                         none ->
                           awre_transport:init(Args);
                         T ->
                           NewTState = T:init(Args),
                           {T,NewTState}
                       end,
  {_,NewState} = create_ref_for_message(Msg,From,#{},State),
  {noreply,NewState#state{transport={Trans,TState}}};
handle_message_from_client({subscribe,Options,Topic,Mfa},From,State) ->
  {ok,NewState} = send_and_ref({subscribe,request_id,Options,Topic},From,#{mfa => Mfa},State),
  {noreply,NewState};
handle_message_from_client({unsubscribe,SubscriptionId},From,State) ->
  {ok,NewState} = send_and_ref({unsubscribe,request_id,SubscriptionId},From,#{sub_id=>SubscriptionId},State),
  {noreply,NewState};
handle_message_from_client({publish,Options,Topic,Arguments,ArgumentsKw},From,State) ->
  {ok,NewState} = send_and_ref({publish,request_id,Options,Topic,Arguments,ArgumentsKw},From,#{},State),
  {reply,ok,NewState};
handle_message_from_client({register,Options,Procedure,Mfa},From,State) ->
  {ok,NewState} = send_and_ref({register,request_id,Options,Procedure},From,#{mfa=>Mfa},State),
  {noreply,NewState};
handle_message_from_client({unregister,RegistrationId},From,State) ->
  {ok,NewState} = send_and_ref({unregister,request_id,RegistrationId},From,#{reg_id => RegistrationId},State),
  {noreply,NewState};
handle_message_from_client({call,Options,Procedure,Arguments,ArgumentsKw},From,State) ->
  {ok,NewState} = send_and_ref({call,request_id,Options,Procedure,Arguments,ArgumentsKw},From,#{},State),
  {noreply,NewState};
handle_message_from_client({yield,_,_,_,_}=Msg,_From,State) ->
  {ok,NewState} = send_to_router(Msg,State),
  {reply,ok,NewState};
handle_message_from_client({error,invocation,RequestId,ArgsKw,ErrorUri},_From,State) ->
  {ok,NewState} = send_to_router({error,invocation,RequestId,#{},ErrorUri,[],ArgsKw},State),
  {reply,ok,NewState};
handle_message_from_client(_Msg,_From,State) ->
  {noreply,State}.




handle_message_from_router({welcome,SessionId,RouterDetails},State) ->
  {From,_} = get_ref(hello,hello,State),
  gen_server:reply(From,{ok,SessionId,RouterDetails}),
  {ok,State};

handle_message_from_router({abort,Details,Reason},State) ->
  {From,_} = get_ref(hello,hello,State),
  gen_server:reply(From,{abort,Details,Reason}),
  {stop,normal,State};

handle_message_from_router({goodbye,_Details,_Reason},#state{goodbye_sent=GS}=State) ->
  NewState = case GS of
               true ->
                 State;
               false ->
                 {ok,NState} = send_to_router({goodbye,[],goodbye_and_out},State),
                 NState
             end,
  close_connection(),
  {ok,NewState};

%handle_message_from_router({error,},#state{ets=Ets}) ->

%handle_message_from_router({published,},#state{ets=Ets}) ->

handle_message_from_router({subscribed,RequestId,SubscriptionId},#state{ets=Ets}=State) ->
  {From,Args} = get_ref(RequestId,subscribe,State),
  Mfa = maps:get(mfa,Args),
  {Pid,_} = From,
  ets:insert_new(Ets,#subscription{id=SubscriptionId,mfa=Mfa,pid=Pid}),
  gen_server:reply(From,{ok,SubscriptionId}),
  {ok,State};

handle_message_from_router({unsubscribed,RequestId},#state{ets=Ets}=State) ->
  {From,Args} = get_ref(RequestId,unsubscribe,State),
  SubscriptionId = maps:get(sub_id,Args),
  ets:delete(Ets,SubscriptionId),
  gen_server:reply(From,ok),
  {ok,State};

handle_message_from_router({event,SubscriptionId,PublicationId,Details},State) ->
  handle_message_from_router({event,SubscriptionId,PublicationId,Details,undefined,undefined},State);
handle_message_from_router({event,SubscriptionId,PublicationId,Details,Arguments},State) ->
  handle_message_from_router({event,SubscriptionId,PublicationId,Details,Arguments,undefined},State);
handle_message_from_router({event,SubscriptionId,_PublicationId,Details,Arguments,ArgumentsKw}=Msg,#state{ets=Ets}=State) ->
  [#subscription{
                id = SubscriptionId,
                mfa = Mfa,
                pid=Pid}] = ets:lookup(Ets,SubscriptionId),
  case Mfa of
    undefined ->
      % send it to user process
      Pid ! {awre,Msg};
    {M,F,S}  ->
      try
        erlang:apply(M,F,[Details,Arguments,ArgumentsKw,S])
      catch
        Error:Reason ->
          io:format("error ~p:~p with event: ~n~p~n",[Error,Reason,erlang:get_stacktrace()])
      end
  end,
  {ok,State};
handle_message_from_router({result,RequestId,Details},State) ->
  handle_message_from_router({result,RequestId,Details,undefined,undefined},State);
handle_message_from_router({result,RequestId,Details,Arguments},State) ->
  handle_message_from_router({result,RequestId,Details,Arguments,undefined},State);
handle_message_from_router({result,RequestId,Details,Arguments,ArgumentsKw},State) ->
  {From,_} = get_ref(RequestId,call,State),
  gen_server:reply(From,{ok,Details,Arguments,ArgumentsKw}),
  {ok,State};

handle_message_from_router({registered,RequestId,RegistrationId},#state{ets=Ets}=State) ->
  {From,Args} = get_ref(RequestId,register,State),
  Mfa = maps:get(mfa,Args),
  {Pid,_} = From,
  ets:insert_new(Ets,#registration{id=RegistrationId,mfa=Mfa,pid=Pid}),
  gen_server:reply(From,{ok,RegistrationId}),
  {ok,State};

handle_message_from_router({unregistered,RequestId},#state{ets=Ets}=State) ->
  {From,Args} = get_ref(RequestId,unregister,State),
  RegistrationId = maps:get(reg_id,Args),
  ets:delete(Ets,RegistrationId),
  gen_server:reply(From,ok),
  {ok,State};

handle_message_from_router({invocation,RequestId,RegistrationId,Details},State) ->
  handle_message_from_router({invocation,RequestId,RegistrationId,Details,undefined,undefined},State);
handle_message_from_router({invocation,RequestId,RegistrationId,Details,Arguments},State) ->
  handle_message_from_router({invocation,RequestId,RegistrationId,Details,Arguments,undefined},State);
handle_message_from_router({invocation,RequestId,RegistrationId,Details,Arguments,ArgumentsKw}=Msg,#state{ets=Ets}=State) ->
  [#registration{
                id = RegistrationId,
                mfa = Mfa,
                pid=Pid}] = ets:lookup(Ets,RegistrationId),
  NewState = case Mfa of
               undefined ->
                 % send it to the user process
                 Pid ! {awre,Msg},
                 State;
               {M,F,S}  ->
                 try erlang:apply(M,F,[Details,Arguments,ArgumentsKw,S]) of
                   {ok,Options,ResA,ResAKw} ->
                     {ok,NState} = send_to_router({yield,RequestId,Options,ResA,ResAKw},State),
                     NState;
                   {error,Details,Uri,Arguments,ArgumentsKw} ->
                     {ok,NState} = send_to_router({error,invocation,RequestId,Details,Uri,Arguments,ArgumentsKw},State),
                     NState;
                   Other ->
                     {ok,NState} = send_to_router({error,invocation,RequestId,#{<<"result">> => Other },invalid_argument,undefined,undefined},State),
                     NState
                 catch
                   Error:Reason ->
                     {ok,NState} = send_to_router({error,invocation,RequestId,#{<<"reason">> => io_lib:format("~p:~p",[Error,Reason])},invalid_argument,undefined,undefined},State),
                     NState
                 end
             end,
  {ok,NewState};

handle_message_from_router({error,call,RequestId,Details,Error},State) ->
  handle_message_from_router({error,call,RequestId,Details,Error,undefined,undefined},State);
handle_message_from_router({error,call,RequestId,Details,Error,Arguments},State) ->
  handle_message_from_router({error,call,RequestId,Details,Error,Arguments,undefined},State);
handle_message_from_router({error,call,RequestId,Details,Error,Arguments,ArgumentsKw},State) ->
  {From,_} = get_ref(RequestId,call,State),
  gen_server:reply(From,{error,Details,Error,Arguments,ArgumentsKw}),
  {ok,State};

handle_message_from_router(Msg,State) ->
  io:format("unhandled message ~p~n",[Msg]),
  {ok,State}.

%
% Session Scope IDs
%
%     ERROR.Request
%     PUBLISH.Request
%     PUBLISHED.Request
%     SUBSCRIBE.Request
%     SUBSCRIBED.Request
%     UNSUBSCRIBE.Request
%     UNSUBSCRIBED.Request
%     CALL.Request
%     CANCEL.Request
%     RESULT.Request
%     REGISTER.Request
%     REGISTERED.Request
%     UNREGISTER.Request
%     UNREGISTERED.Request
%     INVOCATION.Request
%     INTERRUPT.Request
%     YIELD.Request
%
% IDs in the session scope SHOULD be incremented by 1 beginning with 1
% (for each direction - Client-to-Router and Router-to-Client)
%

send_and_ref(Msg,From,Args,State) ->
  {Message,NewState} = create_ref_for_message(Msg,From,Args,State),
  send_to_router(Message,NewState).

send_to_router(Msg,#state{transport={TMod,TState}} = State) ->
  {ok,NewTState} = TMod:send_to_router(Msg,TState),
  {ok,State#state{transport={TMod,NewTState}}}.

create_ref_for_message(Msg,From,Args,#state{ets=Ets}=State)  ->
  Method = case element(1,Msg) of
             connect -> hello;
             El -> El
           end,
  {RequestId,NewState} = case Method of
                           hello ->
                             {hello,State};
                           subscribe ->
                             Id = State#state.subscribe_id,
                             {Id,State#state{subscribe_id = Id+1}};
                           unsubscribe ->
                             Id = State#state.unsubscribe_id,
                             {Id,State#state{unsubscribe_id = Id+1}};
                           publish ->
                             Id = State#state.publish_id,
                             {Id,State#state{publish_id = Id+1}};
                           register ->
                             Id = State#state.register_id,
                             {Id,State#state{register_id = Id+1}};
                           unregister ->
                             Id = State#state.unregister_id,
                             {Id,State#state{unregister_id = Id+1}};
                           call ->
                             Id = State#state.call_id,
                             {Id,State#state{call_id = Id+1}}
                         end,
  true = ets:insert_new(Ets,#ref{key={Method,RequestId},ref=From,args=Args}),
  case is_integer(RequestId) of
    true ->
      {setelement(2,Msg,RequestId),NewState};
    false ->
      {Msg,NewState}
  end.



get_ref(ReqId,Method,#state{ets=Ets}) ->
  Key = {Method,ReqId},
  [#ref{ref=From,args=Args}] = ets:lookup(Ets,Key),
  ets:delete(Ets,Key),
  {From,Args}.

close_connection() ->
  gen_server:cast(self(),terminate).
