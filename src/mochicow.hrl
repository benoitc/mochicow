%M% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of mochicow released under the MIT license.
%%% See the NOTICE for more information.


-record(hstate, {
        socket :: inet:socket(),
        transport :: module(),
        loop :: {module(), any(), any()}}).
