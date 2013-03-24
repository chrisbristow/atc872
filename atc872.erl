-module(atc872).
-export([start/1, add_row/7, fetch_rows/5, search_rows/4]).



start([ Port, Node, Nodes ]) ->
  error_logger:info_msg("Starting node: ~s (total nodes: ~s)~n", [ Node, Nodes ]),
  error_logger:info_msg("Initialising Mnesia~n"),

  mnesia:start(),

  case mnesia:wait_for_tables([ rows ], 300000) of
    ok ->
      ok
      ;
    _ ->
      error_logger:warning_msg("Error: Mnesia has failed to start~n"),
      init:stop()
  end,

  error_logger:info_msg("Starting the web server on port: ~s~n", [ Port ]),
  misultin:start_link([ { port, list_to_integer(atom_to_list(Port)) }, { loop, fun(Req) -> handle_http(Req) end } ]),

  error_logger:info_msg("Ready~n"),

  register(main, self()),
  loop(list_to_integer(atom_to_list(Node)), list_to_integer(atom_to_list(Nodes))).




loop(Node, Nodes) ->
  receive
    { nodeinfo, Requestor } ->
      Requestor ! { Node, Nodes },
      loop(Node, Nodes)
      ;
    _ ->
      loop(Node, Nodes)
  end.



handle_http(Req) ->
  handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).




row_renderer(RowData, Req) ->
% error_logger:info_msg("ROW_R DEBUG: ~p~n", [ RowData ]),
  case RowData of
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
                         A ++ Pfx ++ "    { \"time\": \"" ++ pad(integer_to_list(Hr)) ++ ":" ++ pad(integer_to_list(Mi)) ++ " " ++ pad(integer_to_list(Dy)) ++ "-" ++ pad(integer_to_list(Mo)) ++ "-" ++ integer_to_list(Yr) ++ "\", \"user\": \"" ++quote_handler(User)++ "\", \"text\": \"" ++quote_handler(Text)++ "\" }"
                         end, [], RowList),
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
  end.



quote_handler(Str) ->
  Filter1 = re:replace(Str, "[\\x00-\\x1f\\x5c]+", "", [{ return, list }, global ]),
  re:replace(Filter1, "\"", "\\\\\"", [{ return, list }, global ]).




handle('GET', [], Req) ->
  Req:file("web/atc872.html")
  ;
handle('POST', ["addrow"], Req) ->
  [{ "channel", Channel }, { "user", User }, { "text", Text }, { "from", LastRow }, { "back", RowsBack }] = Req:parse_post(),
  { Node, Nodes } = get_node_info(),
  RowData = add_row(Nodes, Channel, User, Text, Node, list_to_integer(LastRow), list_to_integer(RowsBack)),
  row_renderer(RowData, Req)
  ;
handle('POST', ["fetchrows"], Req) ->
  [{ "channel", Channel }, { "user", User }, { "from", LastRow }, { "back", RowsBack }] = Req:parse_post(),
  { Node, _ } = get_node_info(),
  RowData = fetch_rows(Node, Channel, User, list_to_integer(LastRow), list_to_integer(RowsBack)),
  row_renderer(RowData, Req)
  ;
handle('POST', ["searchrows"], Req) ->
  [{ "channel", Channel }, { "back", RowsBack }, { "pattern", Pattern }] = Req:parse_post(),
  { Node, _ } = get_node_info(),
  RowData = search_rows(Node, Channel, list_to_integer(RowsBack), Pattern),
  row_renderer(RowData, Req)
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




add_row(Nodes, Channel, User, Text, Node, LastRow, RowsBack) ->
  error_logger:info_msg("Adding row to ~p from ~p: ~p (node: ~p, nodes: ~p, from: ~p, back: ~p)~n", [ Channel, User, Text, Node, Nodes, LastRow, RowsBack ]),
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
    end,lists:seq(0, Nodes - 1)),
    get_rows(Node, Channel, LastRow, RowsBack)
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Add row failed: ~p~n", [Reason]),
      error
      ;
    { atomic, Rval } ->
      Rval
  end.




get_rows(Node, Channel, LastRow, RowsBack) ->
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
  end.




get_search_results(Node, Channel, RowsBack, SearchString) ->
  case mnesia:read({ rows, { last, Channel, Node } }) of
    [ { _, _, Last } ] ->
      ResultSet = lists:foldl(fun(E, A) ->
        [ { _, _, { Now, User, Text } } ] = mnesia:read({ rows, { Node, Channel, E } }),
        M1 = case re:run(User, SearchString, [{ capture, none }, caseless]) of
          match ->
            [{ Now, User, Text }]
            ;
          _ ->
            case re:run(Text, SearchString, [{ capture, none }, caseless]) of
              match ->
                [{ Now, User, Text }]
                ;
              _ -> []
            end
        end,
        M1 ++ A
      end, [], lists:sublist(lists:seq(Last, 0, -1), RowsBack)),
      { -1, ResultSet }
      ;
    [] -> no_such_channel
  end.




fetch_rows(Node, Channel, User, LastRow, RowsBack) ->
  error_logger:info_msg("Fetching rows for ~p from ~p (node: ~p, from: ~p, back: ~p)~n", [ User, Channel, Node, LastRow, RowsBack ]),
  Transaction=fun() ->
    case mnesia:read({ rows, users }) of
      [{ _, _, CurrentUserList }] ->
        mnesia:write({ rows, users, lists:keystore(User, 1, CurrentUserList, { User, now() }) })
        ;
      [] ->
        mnesia:write({ rows, users, [{ User, now() }] })
    end,
    get_rows(Node, Channel, LastRow, RowsBack)
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Fetch rows failed: ~p~n", [Reason]),
      error
      ;
    { atomic, Rval } ->
      Rval
  end.



search_rows(Node, Channel, RowsBack, SearchString) ->
  error_logger:info_msg("Searching for ~p from ~p (node: ~p, back: ~p)~n", [ SearchString, Channel, Node, RowsBack ]),
  Transaction=fun() ->
    get_search_results(Node, Channel, RowsBack, SearchString)
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Search rows failed: ~p~n", [Reason]),
      error
      ;
    { atomic, Rval } ->
      Rval
  end.
