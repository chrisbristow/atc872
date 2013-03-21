-module(atc872).
-export([start/1, add_row/4, fetch_rows/4]).



start([ Port, Node, Nodes ]) ->
  error_logger:info_msg("Port:~s Node:~s Nodes:~s~n", [ Port, Node, Nodes ]),
  mnesia:start(),
  misultin:start_link([ { port, list_to_integer(atom_to_list(Port)) }, { loop, fun(Req) -> handle_http(Req) end } ]),
  register(main, self()),
  loop(list_to_integer(atom_to_list(Node)), list_to_integer(atom_to_list(Nodes))).



loop(Node, Nodes) ->
  receive
    { nodeinfo, Requestor } -> Requestor ! { Node, Nodes };
    _ -> error
  after 5000 ->
    case filelib:is_file("atc872.stop") of
      true ->
        timer:sleep(1000),
        init:stop()
        ;
      _ ->
        ok
    end
  end,
  loop(Node, Nodes).



handle_http(Req) ->
  handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).



handle('GET', [], Req) ->
  Req:file("web/atc872.html")
  ;
handle('POST', ["addrow"], Req) ->
  [{ "channel", Channel }, { "user", User }, { "text", Text }] = Req:parse_post(),
  { _, Nodes } = get_node_info(),
  case add_row(Nodes, Channel, User, Text) of
    ok ->
      Req:ok("{ \"status\": \"ok\" }")
      ;
    error ->
      Req:ok("{ \"status\": \"error\" }")
  end
  ;
handle('POST', ["fetchrows"], Req) ->
  [{ "channel", Channel }, { "from", LastRow }, { "back", RowsBack }] = Req:parse_post(),
  { Node, _ } = get_node_info(),
  case fetch_rows(Node, Channel, list_to_integer(LastRow), list_to_integer(RowsBack)) of
    { LatestRow, no_rows } ->
      Req:ok("{ \"status\": \"no_rows\"," ++
             "  \"latest\": " ++integer_to_list(LatestRow) ++ "}")
      ;
    { LatestRow, RowList } ->
      Rows = lists:foldl(fun({ Now, User, Text }, A) ->
                         { { Yr, Mo, Dy }, { Hr, Mi, _ } } = calendar:now_to_local_time(Now),
                         Pfx = case length(A) of
                           0 -> "";
                           _ -> "," ++[10]
                         end,
                         A ++ Pfx ++ "    { \"time\": \"" ++ pad(integer_to_list(Hr)) ++ ":" ++ pad(integer_to_list(Mi)) ++ " " ++ pad(integer_to_list(Dy)) ++ "-" ++ pad(integer_to_list(Mo)) ++ "-" ++ integer_to_list(Yr) ++ "\", \"user\": \"" ++User++ "\", \"text\": \"" ++Text++ "\" }"
                         end, [], lists:reverse(RowList)),
      Req:ok("{ \"status\": \"ok\"," ++ [10] ++
             "  \"latest\": " ++integer_to_list(LatestRow) ++ "," ++[10]++
             "  \"rows\": [" ++[10]++
             Rows ++[10]++
             "  ]" ++[10]++
             "}"
      )
      ;
    no_such_channel ->
      Req:ok("{ \"status\": \"no_such_channel\" }")
      ;
    error ->
      Req:ok("{ \"status\": \"error\" }")
  end
  ;
handle('GET', [Static], Req) ->
  Req:file("web/"++Static).



pad(Str) ->
  case length(Str) of
    1 -> "0" ++ Str;
    _ -> Str
  end.




get_node_info() ->
  main ! { nodeinfo, self() },
  receive
    { Node, Nodes } -> { Node, Nodes };
    _ -> error
  end.




add_row(Nodes, Channel, User, Text) ->
  Transaction=fun() ->
    lists:foreach(fun(I) ->
      case mnesia:read({ rows, { last, Channel, I } }) of
        [ { _, _, L } ] ->
          mnesia:write({ rows, { last, Channel, I }, L + 1 }),
          mnesia:write({ rows, { I, Channel, L + 1 }, { now(), User, Text } })
          ;
        [] ->
          mnesia:write({ rows, { first, Channel, I }, 0 }),
          mnesia:write({ rows, { last, Channel, I }, 0 }),
          mnesia:write({ rows, { I, Channel, 0 }, { now(), User, Text } })
      end,
      case mnesia:read({ rows, { updated, I } }) of
        [ { _, _, UpdateList } ] ->
          mnesia:write({ rows, { updated, I }, lists:keystore(Channel, 1, UpdateList, { Channel, now() }) })
          ;
        [] ->
          mnesia:write({ rows, { updated, I }, lists:keystore(Channel, 1, [], { Channel, now() }) })
      end
    end,lists:seq(0, Nodes - 1))
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Add row failed: ~p~n", [Reason]),
      error
      ;
    { atomic, _ } ->
      ok
  end.




fetch_rows(Node, Channel, LastRow, RowsBack) ->
  Transaction=fun() ->
    case mnesia:read({ rows, { last, Channel, Node } }) of
      [ { _, _, Last } ] ->
        if
          Last > LastRow ->
            Rows = lists:foldl(fun(E, A) ->
              [ { _, _, R } ] = mnesia:read({ rows, { Node, Channel, E } }),
              [ R ] ++ A
            end, [], lists:sublist(lists:seq(Last, LastRow + 1, -1), RowsBack)),
            { Last, Rows }
            ;
          true -> { Last, no_rows }
        end
        ;
      [] -> no_such_channel
    end
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Fetch rows failed: ~p~n", [Reason]),
      error
      ;
    { atomic, Rval } ->
      Rval
  end.
