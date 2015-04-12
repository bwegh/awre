# awre
[![passing or failing?](https://travis-ci.org/bwegh/awre.svg?branch=master)](https://travis-ci.org/bwegh/awre/)

A wamp.ws client written in erlang


Table of Contents
=================

* [Description](#description)
* [Features](#features)
* [Peer](#peer)
* [Examples](#examples)
* [License](#license)

Description
===========
Awre is the implementation of the WAMP client/peer in Erlang.

Pull Requests, Bug Reports, Comments and any other kind of feedback is welcome.


[Back to TOC](#table-of-contents)

Features
========

The peer support all four kind of roles:
 - caller
 - callee
 - publisher
 - subscriber
At the moment only raw tcp connections are supported, not yet websocket.


[Back to TOC](#table-of-contents)

Peer
====
Awre implements the basic profile for all four roles of a peer, caller and callee as well as
publisher and subscriber.
The connection to the router can either be remote to another host/port or local to a router
running within the same VM.

The connections are implemented as gen_server and can be shared between different processes.
The Idea is to have just one connection to a router and share it with all needing processes.
*A lookup of connections to certain routers and realms is not yet implemented, yet under consideration*

To connect to a realm you need to follow a few simple steps:
```Erlang
%% first start a connection
{ok,Con} = awre:start_client(),
%% then connect to either a local or remote router
%% local would be
{ok,SessionId,RouterDetails} = awre:connect(Con,Realm),
%% the following is a remote router
{ok,SessionId,RouterDetails} = awre:connect(Con,Host,Port,Realm,Encoding),


%% now the connection 'Con' is connected to the router and handles everything for you
%% sending an event to a certain topic is just as easy:
%% the two parameters Arguments and ArgumentsKW are optional.
ok = awre:publish(Con,Options,EventTopicUrl,Arguments,ArgumentsKW),

%% invoking a remote procedure is easy, too:
%% as with publishing are the Arguments and the ArgumentsKW optional.
%% ResA and ResAKw may be undefined in case they did not include any data.
{ok,Details,ResA,ResAKw} = awre:call(Con,Options,ProcedureUri,Arguments,ArgumentsKW),

%% for subscription to a topic or registering a function for remote calls
%% there exist two different ways:
%% one is to use a tuple of module, function and one argument that will be called by awre.
%% this is the easiest way, yet has the drawback that you can only forward one argument
%% to your function and not return any state change or similar.
%% the following line will invoke Module:Function(Details,Arguments,ArgumentsKW,OneArgument)
%% on the occurance of an event.
{ok,SubscriptionId} = awre:subscribe(Con,Options,EventUrl,{Module,Function,OneArgument}),

%% the same works for registering a remote procedure.
%% the function that will be called here is also:
%% Module:Function(Details,Arguments,ArgumentsKW,OneArgument)
{ok,RegistrationId} = awre:register(Con,Options,ProcedureUri,{Module,Function,OneArgument}),


%% the other possibility is to subscribe or register without giving an mfa.
%% In this case the process will receive the
%% {awre,{event,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw}}
%% message for an event and the
%% {awre,{invocation,RequestId,RegistrationId,Details,Arguments,ArgumentsKw}}
%% message for an invocation.
%% you MUST reply to an invocation by using awre:yield/3,awre:yield/4 or awre:yield/5.
%%
{ok,SubscriptionId} = awre:subscribe(Con,Options,EventUri),
{ok,RegistrationId} = awre:register(Con,Options,ProcedureUri),
```

The crossbar_client example includes registration and subscriptions using mfa.
Also have a look at the simple_client example for a client implementation using gen_server
and subscribe and register without mfa.

[Back to TOC](#table-of-contents)


Examples
========

In the exampes directory you can find three different examples:
 * crossbar_client: This is the template used in crossbar to demonstrate the usage of an erlang client with crossbar.io.
 * simple_client: A simple client that shows how a client can be implemented using a gen_server.

[Back to TOC](#table-of-contents)

License
========
The MIT License (MIT)

Copyright (c) 2014 Bas Wegh

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
[Back to TOC](#table-of-contents)
