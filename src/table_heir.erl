-module(table_heir).
-behaviour(gen_statem).

%% API.
-export([specs/1]).
-export([start_link/1]).
-export([get_table/1]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([waiting_owner/3]).
-export([empty/3]).
-export([terminate/3]).
-export([code_change/4]).

-type init_args() :: list().


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

-spec get_table(ets:tab()) -> ok.
get_table(Table) ->
  Heir = ets:info(Table, heir),
  erlang:monitor(process, Heir),
  gen_statem:call(Heir, get_table).



%% gen_statem.

callback_mode() ->
  state_functions.

init([TableName, TableOpts]) ->
  case ets:whereis(TableName) of
    undefined ->
      Table = ets:new(TableName, [named_table|[{heir, self(), []}|TableOpts]]),
      {ok, waiting_owner, #{table => Table}};
    Table ->
      table_owner:update_heir(Table),
      {ok, empty, #{table => Table}}
  end.

waiting_owner({call, {Pid, _Ref} = From}, get_table, #{table := Table} = StateData) ->
  ets:give_away(Table, Pid, []),
  {next_state, empty, StateData, [{reply, From, ok}]};
waiting_owner(_EventType, _EventData, _StateData) ->
  keep_state_and_data.

empty(info, {'ETS-TRANSFER', _Table, _From, _HeirData}, StateData) ->
  {next_state, waiting_owner, StateData};
empty(_EventType, _EventData, _StateData) ->
  keep_state_and_data.


terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.
