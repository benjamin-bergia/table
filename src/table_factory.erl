-module(table_factory).
-behaviour(supervisor).

-export([specs/0]).
-export([start_link/1]).
-export([init/1]).


-type init_args() :: table_heir:init_args().

-spec specs() -> supervisor:child_spec().
specs() ->
  #{id => ?MODULE,
    start => {?MODULE, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [?MODULE]
  }.

-spec start_link(init_args()) -> supervisor:startlink_ret().
start_link(Args) ->
  supervisor:start_link(?MODULE, Args).

-spec init(init_args()) -> {ok, {supervisor:sup_flags(), list(supervisor:child_spec())}}.
init([Table, Options]) ->
  Procs = [
    table_heir:specs([Table, Options]),
    table_owner:specs(Table)
  ],
  {ok, {{one_for_one, 1, 5}, Procs}}.
