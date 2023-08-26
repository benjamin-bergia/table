-module(table).

-export([new/1]).
-export([new/2]).


-spec new(ets:tab()) -> supervisor:startchild_ret().
new(Name) ->
  table_sup:new_table([Name, []]).

-spec new(ets:tab(), list()) -> supervisor:startchild_ret().
new(Name, Options) ->
  table_sup:new_table([Name, Options]).
