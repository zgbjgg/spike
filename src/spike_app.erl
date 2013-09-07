%%
%% Copyright (c) 2012-2013, Jorge Garrido <zgbjgg@gmail.com>
%% All rights reserved.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(spike_app).

-author('zgbjgg@gmail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% FOR DEBUG PURPOSES DISABLE AND COMPILE AGAIN
    %% application:start(sasl),
    spike_sup:start_link().

stop(_State) ->
    ok.
