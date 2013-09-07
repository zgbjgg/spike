%%
%% Copyright (c) 2012-2013, Jorge Garrido <zgbjgg@gmail.com>
%% All rights reserved.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(spike).

-author('zgbjgg@gmail.com').

-behaviour(gen_server).

-include("spike.hrl").

%% API
-export([start_link/0, get_device/0, stop/0, get_devices/0, 
	 process_by/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { devices }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get devices connected.
%%
%% @spec get_devices() -> {ok, Devices :: list()}
%% @end
%%--------------------------------------------------------------------
-spec get_devices() -> {ok, Devices :: list()}.
get_devices() ->
    gen_server:call(?MODULE, getting_devices).

%%--------------------------------------------------------------------
%% @doc
%% Get device connected using load balancer.
%%
%% @spec get_device() -> {ok, Device :: atom()}
%% @end
%%--------------------------------------------------------------------
-spec get_device() -> {ok, Device :: atom()}.
get_device() ->
    gen_server:call(?MODULE, getting_device).

%%--------------------------------------------------------------------
%% @doc
%% Process Mod:Func(Args) on selected device.
%%
%% @spec process_by(Device :: atom(), { Mod :: atom(), 
%%				        Func :: atom(), 
%%					Args :: list()}) -> term()
%% @end
%%--------------------------------------------------------------------
-spec process_by(Device :: atom(), { Mod :: atom(), 
                                      Func :: atom(), 
                                      Args :: list()}) -> term().
process_by(Device, { _Mod, _Func, _Args } = Options ) ->
    gen_server:call(?MODULE, {processing_by, Device, Options}).


%%--------------------------------------------------------------------
%% @doc
%% Stops the gen server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    ok = net_kernel:monitor_nodes(true), 
    {ok, #state{devices=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(getting_devices, _From, State=#state{devices=Devices})           ->
    {reply, {ok, Devices}, State};
handle_call(getting_device, _From, State=#state{devices=[]}) 		     ->
    {reply, {error, no_alive_devices}, State};
handle_call(getting_device, _From, #state{devices=[Device | Devices]})       ->
    {reply, {ok, Device}, #state{devices = Devices ++ [Device]}};
handle_call({processing_by, Device, {Mod, Func, Args}}, _From, State)        ->
    Res = rpc:call(Device, Mod, Func, Args),
    {reply, Res, State};
handle_call(stop, _From, State) 				             ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({nodedown, Device}, #state{devices=Devices}) ->
    {noreply, #state{devices=[ D || D <- Devices, D =/= Device ]}};
handle_info({nodeup, Device}, #state{devices=Devices})   ->
    {noreply, #state{devices=[ Device | Devices ]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
