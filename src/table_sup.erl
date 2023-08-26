-module(table_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([new_table/1]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec new_table(table_heir:init_args()) -> supervisor:startchild_ret().
new_table(Args) ->
  supervisor:start_child(?MODULE, [Args]).

-spec init([]) -> {ok, {supervisor:sup_flags(), list(supervisor:child_spec())}}.
init([]) ->
  Procs = [
    table_factory:specs()
  ],
  {ok, {{simple_one_for_one, 1, 5}, Procs}}.
