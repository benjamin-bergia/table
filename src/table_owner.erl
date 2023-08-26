-module(table_owner).
-behaviour(gen_statem).

%% API.
-export([specs/1]).
-export([start_link/1]).
-export([update_heir/1]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([empty/3]).
-export([owning_table/3]).
-export([terminate/3]).
-export([code_change/4]).


-type init_args() :: ets:tab().
%% API.

-spec specs(init_args()) -> supervisor:child_spec().
specs(Args) ->
  #{id => ?MODULE,
    start => {?MODULE, start_link, [Args]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [?MODULE]
  }.

-spec start_link(init_args()) -> gen_statem:start_ret().
start_link(Args) ->
  gen_statem:start_link(?MODULE, Args, []).

-spec update_heir(ets:tab()) -> ok.
update_heir(Table) ->
  Owner = ets:info(Table, owner),
  gen_statem:call(Owner, update_heir).


%% gen_statem.

callback_mode() ->
  state_functions.

init(TableName) ->
  table_heir:get_table(TableName),
  {ok, empty, #{table => TableName}}.

empty(info, {'ETS-TRANSFER', _Table, _FromPid, _GiftData}, StateData) ->
  {next_state, owning_table, StateData};
empty(info, {'DOWN', _Ref, process, _Pid, _Reason}, #{table := Table}) ->
  table_heir:get_table(Table),
  keep_state_and_data;
empty(_EventType, _EventData, _StateData) ->
  keep_state_and_data.

owning_table(info, {'DOWN', _Ref, process, _Pid, _Reason}, _StateData) ->
  keep_state_and_data;
owning_table({call, {Pid, _Ref} = From}, update_heir, #{table := Table}) ->
  erlang:monitor(process, Pid),
  ets:setopts(Table, {heir, Pid, []}),
  {keep_state_and_data, [{reply, From, ok}]};
owning_table(_EventType, _EventData, _StateData) ->
  keep_state_and_data.

terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.
