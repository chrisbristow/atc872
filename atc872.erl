% atc872.erl
%
% Server for the ATC872 low-maintenance online chat forum service.

% Copyright (c) 2013, Chris Bristow
% All rights reserved.
% 
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met: 
% 
% 1. Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer. 
% 2. Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution. 
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
% 
% The views and conclusions contained in the software and documentation are those
% of the authors and should not be interpreted as representing official policies, 
% either expressed or implied, of the FreeBSD Project.




-module(atc872).
-export([start/1, add_row/7, add_row/8, fetch_rows/5, search_rows/4, archiver/2]).




% Start hook.  Takes arguments:
% - TCP port for the web server.
% - Node ID number.
% - Number of nodes in the cluster.
% - Number of rows to cache before the archiver writes them to disk.
% Initialises Mnesia and starts the Misultin web server.

start([ Port, Node, Nodes, CachedRows ]) ->
  process_flag(trap_exit, true),

  error_logger:info_msg("Starting node: ~s (total nodes: ~s)~n", [ Node, Nodes ]),
  error_logger:info_msg("Caching ~s rows~n", [ CachedRows ]),
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

  error_logger:info_msg("Starting the archiver~n"),
  spawn_link(?MODULE, archiver, [ list_to_integer(atom_to_list(Node)), list_to_integer(atom_to_list(CachedRows)) ]),

  error_logger:info_msg("Ready~n"),

  register(main, self()),
  loop(list_to_integer(atom_to_list(Node)), list_to_integer(atom_to_list(Nodes))).




% Daemon process that returns the current node ID and number of
% nodes in the cluster on receipt of a message.

loop(Node, Nodes) ->
  receive
    { nodeinfo, Requestor } ->
      Requestor ! { Node, Nodes },
      loop(Node, Nodes)
      ;
    _ ->
      loop(Node, Nodes)
  end.




% Directs Misultin HTTP requests to a group of "handle()" functions which
% provide a REST-ful interface to the server.

handle_http(Req) ->
  handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).




% Convert a now() to a HH:MM dd-mm-yyyy.

now_to_string(Now) ->
  { { Yr, Mo, Dy }, { Hr, Mi, _ } } = calendar:now_to_local_time(Now),
  pad(integer_to_list(Hr)) ++ ":" ++ pad(integer_to_list(Mi)) ++ " " ++ pad(integer_to_list(Dy)) ++ "-" ++ pad(integer_to_list(Mo)) ++ "-" ++ integer_to_list(Yr).



% Takes a list of rows and a Misultin request reference, then renders the
% rows as a JSON document.

row_renderer(RowData, Req) ->
  case RowData of
    { LatestRow, no_rows } ->
      Req:ok("{ \"status\": \"no_rows\"," ++
             "  \"latest\": " ++integer_to_list(LatestRow) ++ "}")
      ;
    { LatestRow, RowList } ->
      Rows = lists:foldl(fun({ Now, User, Text }, A) ->
                         Pfx = case length(A) of
                           0 -> "";
                           _ -> "," ++[10]
                         end,
                         A ++ Pfx ++ "    { \"time\": \"" ++ now_to_string(Now) ++ "\", \"user\": \"" ++quote_handler(User)++ "\", \"text\": \"" ++quote_handler(Text)++ "\" }"
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
      ;
    Else ->
      error_logger:warning_msg("Warning: Row data returned: ~p~n", [ Else ])
  end.




% Removes un-printable characters from a string.

quote_handler(Str) ->
  Filter1 = re:replace(Str, "[\\x00-\\x1f\\x5c]+", "", [{ return, list }, global ]),
  re:replace(Filter1, "\"", "\\\\\"", [{ return, list }, global ]).




% Log RT time.

log_rt(Pfx, Then) ->
  { Ms1, Sc1, Us1 } = Then,
  { Ms2, Sc2, Us2 } = now(),
  error_logger:info_msg("~s took: ~p us~n", [Pfx, ((Ms2 * 1000000000000) + (Sc2 * 1000000) + Us2) - ((Ms1 * 1000000000000) + (Sc1 * 1000000) + Us1)]).



% REST-ful interface to the web server.

handle('GET', [], Req) ->
  Req:file("web/atc872.html")
  ;
handle('POST', ["addrow"], Req) ->
  Ctime = now(),
  [{ "channel", Channel }, { "user", User }, { "text", Text }, { "from", LastRow }, { "back", RowsBack }] = Req:parse_post(),
  { Node, Nodes } = get_node_info(),
  RowData = add_row(Nodes, Channel, User, Text, Node, list_to_integer(LastRow), list_to_integer(RowsBack)),
  row_renderer(RowData, Req),
  log_rt("Add row (user:" ++ User ++ " channel:" ++ Channel ++ " text:" ++ Text ++ " from:" ++ LastRow ++ " back:" ++ RowsBack ++ ")", Ctime)
  ;
handle('POST', ["fetchrows"], Req) ->
  Ctime = now(),
  [{ "channel", Channel }, { "user", User }, { "from", LastRow }, { "back", RowsBack }] = Req:parse_post(),
  { Node, _ } = get_node_info(),
  RowData = fetch_rows(Node, Channel, User, list_to_integer(LastRow), list_to_integer(RowsBack)),
  row_renderer(RowData, Req),
  log_rt("Fetch rows (user:" ++ User ++ " channel:" ++ Channel ++ " from:" ++ LastRow ++ " back:" ++ RowsBack ++ ")", Ctime)
  ;
handle('POST', ["searchrows"], Req) ->
  Ctime = now(),
  [{ "channel", Channel }, { "back", RowsBack }, { "pattern", Pattern }] = Req:parse_post(),
  { Node, _ } = get_node_info(),
  RowData = search_rows(Node, Channel, list_to_integer(RowsBack), Pattern),
  row_renderer(RowData, Req),
  log_rt("Search (channel:" ++ Channel ++ " back:" ++ RowsBack ++ " pattern:" ++ Pattern ++ ")", Ctime)
  ;
handle('GET', [Static], Req) ->
  Req:file("web/"++Static).




% Prefixes a single digit with a "0", ie. "1" becomes "01" and "11" is left as-is.

pad(Str) ->
  case length(Str) of
    1 -> "0" ++ Str;
    _ -> Str
  end.



% Get the node ID and number of nodes from the "main" loop daemon.

get_node_info() ->
  main ! { nodeinfo, self() },
  receive
    { Node, Nodes } -> { Node, Nodes };
    _ -> error
  end.




% Add a row to the database and return all new rows (including the row added).
% The function containing the argument "Now" allows the row to be given a specific date, otherwise
% the current date/time is used.

add_row(Nodes, Channel, User, Text, Node, LastRow, RowsBack) ->
  add_row(Nodes, Channel, User, Text, Node, LastRow, RowsBack, now()).

add_row(Nodes, Channel, User, Text, Node, LastRow, RowsBack, Now) ->
  Transaction = fun() ->
    lists:foreach(fun(I) ->
      case mnesia:read({ rows, { last, Channel, I } }) of
        [ { _, _, L } ] ->
          mnesia:write({ rows, { last, Channel, I }, L + 1 }),
          mnesia:write({ rows, { I, Channel, L + 1 }, { Now, User, Text } })
          ;
        [] ->
          mnesia:write({ rows, { first, Channel, I }, 0 }),
          mnesia:write({ rows, { last, Channel, I }, 0 }),
          mnesia:write({ rows, { created, Channel, I }, { Now, User } }),
          mnesia:write({ rows, { I, Channel, 0 }, { Now, User, Text } })
      end,
      case mnesia:read({ rows, { updated, I } }) of
        [ { _, _, UpdateList } ] ->
          mnesia:write({ rows, { updated, I }, lists:keystore(Channel, 1, UpdateList, { Channel, Now }) })
          ;
        [] ->
          mnesia:write({ rows, { updated, I }, lists:keystore(Channel, 1, [], { Channel, Now }) })
      end
    end,lists:seq(0, Nodes - 1)),
    get_rows(Node, Channel, LastRow, RowsBack)
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Add row failed: ~p~n", [Reason]),
      error
      ;
    { atomic, Rval } ->
      Rval
  end.




% Return all rows added after the given row ID ("LastRow").

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





% Recursive search function.  Searches the cache in reverse until:
% - RowsBack rows have been found.
% - The start of the cache ("first") is reached.

search_row(Node, Channel, RowsBack, SearchString, Row, MatchCount, MatchList) ->
  if
    MatchCount > RowsBack ->
      MatchList
      ;
    true ->
      case mnesia:read({ rows, { Node, Channel, Row } }) of
        [ { _, _, { Now, User, Text } } ] ->
          case re:run(User, SearchString, [{ capture, none }, caseless]) of
            match ->
              search_row(Node, Channel, RowsBack, SearchString, Row - 1, MatchCount + 1, MatchList ++ [{ Now, User, Text }])
              ;
            _ ->
              case re:run(Text, SearchString, [{ capture, none }, caseless]) of
                match ->
                  search_row(Node, Channel, RowsBack, SearchString, Row - 1, MatchCount + 1, MatchList ++ [{ Now, User, Text }])
                  ;
                _ ->
                  case re:run(now_to_string(Now), SearchString, [{ capture, none }, caseless]) of
                    match ->
                      search_row(Node, Channel, RowsBack, SearchString, Row - 1, MatchCount + 1, MatchList ++ [{ Now, User, Text }])
                      ;
                    _ ->
                      search_row(Node, Channel, RowsBack, SearchString, Row - 1, MatchCount, MatchList)
                  end
              end
          end
          ;
        _ -> MatchList
    end
  end.





% Search for a specific string in the Text and User parts the last N rows
% of a given channel.

get_search_results(Node, Channel, RowsBack, SearchString) ->
  case mnesia:read({ rows, { last, Channel, Node } }) of
    [ { _, _, Last } ] ->
      { -1, lists:reverse(search_row(Node, Channel, RowsBack, SearchString, Last, 0, [])) }
      ;
    [] -> no_such_channel
  end.




% Return the last N rows after the specified row ID on a given channel.

fetch_rows(Node, Channel, User, LastRow, RowsBack) ->
  Transaction = fun() ->
    case mnesia:read({ rows, users }) of
      [{ _, _, CurrentUserList }] ->
        mnesia:write({ rows, { users, Channel }, lists:keystore(User, 1, CurrentUserList, { User, now() }) })
        ;
      [] ->
        mnesia:write({ rows, { users, Channel }, [{ User, now() }] })
    end,
    get_rows(Node, Channel, LastRow, RowsBack)
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Fetch rows failed: ~p~n", [Reason]),
      error
      ;
    { atomic, Rval } ->
      Rval
  end.




% Transactional wrapper for conducting a channel search.

search_rows(Node, Channel, RowsBack, SearchString) ->
  Transaction = fun() ->
    get_search_results(Node, Channel, RowsBack, SearchString)
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Search rows failed: ~p~n", [Reason]),
      error
      ;
    { atomic, Rval } ->
      Rval
  end.




% Archiver main loop.

archiver(Node, CachedRows) ->
  Transaction = fun() ->
    case mnesia:read({ rows, { updated, Node } }) of
      [] ->
        []
        ;
      [{ _, _, [] }] ->
        []
        ;
      [{ _, _, UpdateList }] ->
        mnesia:delete({ rows, { updated, Node } }),
        UpdateList
%     [{ _, _, [{ Channel, _Now } | Rest ] }] ->
%       mnesia:write({ rows, { updated, Node }, Rest }),
%       [{ _, _, First }] = mnesia:read({ rows, { first, Channel, Node } }),
%       [{ _, _, Last }] = mnesia:read({ rows, { last, Channel, Node } }),
%
%       if
%         (Last - First) > CachedRows ->
%           { archive_this, Channel, First, ((Last - First) - CachedRows) }
%           ;
%         true ->
%           []
%       end
    end
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:info_warning("Error: Archiver error: ~p~n", [ Reason ]),
      timer:sleep(10000),
      archiver(Node, CachedRows)
      ;
    { atomic, [] } ->
      timer:sleep(5000),
      archiver(Node, CachedRows)
      ;
%   { atomic, { archive_this, Channel, Oldest, Newest } } ->
%     error_logger:info_msg("Archiving: ~s, rows ~p to ~p~n", [ Channel, Oldest, Newest ]),
%     do_archive(Node, Channel, Oldest, Newest),
%     archiver(Node, CachedRows)
%     ;
    { atomic, UpdateList } ->
      lists:foreach(fun({ Channel, _ }) ->
%       error_logger:info_msg("DEBUG: do_archive(): node:~p channel:~s~n", [ Node, Channel ]),
        do_archive(Node, Channel, CachedRows, 0)
      end, UpdateList),
      archiver(Node, CachedRows)
      ;
    Else ->
      error_logger:info_warning("Error: Archiver error: ~p~n", [ Else ]),
      archiver(Node, CachedRows)
  end.




% Start archiving.

do_archive(Node, Channel, CachedRows, N) ->
  Transaction = fun() ->
    [{ _, _, First }] = mnesia:read({ rows, { first, Channel, Node } }),
    [{ _, _, Last }] = mnesia:read({ rows, { last, Channel, Node } }),

    if
      (Last - First) > CachedRows ->
        [{ _, _, { Now, User, Text } }] = mnesia:read({ rows, { Node, Channel, First } }),
        { archive_this, First, Now, User, Text }
        ;
      true ->
        []
    end
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:info_warning("Error: Archiver error: ~p~n", [ Reason ])
      ;
    { atomic, { archive_this, First, Now, User, Text } } ->
      case write_row_to_file(Channel, Now, User, Text) of
        ok ->
          remove_archived_row(Node, Channel, First),
          timer:sleep(100),
          do_archive(Node, Channel, CachedRows, N + 1)
          ;
        { error, Reason } ->
          error_logger:info_warning("Error: Archiver error - write_row_to_file failed: ~p~n", [ Reason ])
      end
      ;
    { atomic, [] } ->
      if
        N > 0 ->
          error_logger:info_msg("Archived ~p row(s) from ~s~n", [ N, Channel ])
          ;
        true ->
          ok
      end
  end.




% do_archive(Node, Channel, ThisRow, Newest) when ThisRow < Newest ->
%   Transaction = fun() ->
%     [{ _, _, { Now, User, Text } }] = mnesia:read({ rows, { Node, Channel, ThisRow } }),
%     { Now, User, Text }
%   end,
%   case mnesia:transaction(Transaction) of
%     { aborted, Reason } ->
%       error_logger:info_warning("Error: Archiver error: ~p~n", [ Reason ])
%       ;
%     { atomic, { Now, User, Text } } ->
%       case write_row_to_file(Now, User, Text) of
%         ok ->
%           remove_archived_row(Node, Channel, ThisRow),
%           do_archive(Node, Channel, ThisRow + 1, Newest)
%           ;
%         _ ->
%           error
%       end
%   end
%   ;
% do_archive(_, _, _, _) ->
%   ok.




% Append a row to an archive file.

write_row_to_file(Channel, Now, User, Text) ->
  { { Yr, Mo, Dy }, _ } = calendar:now_to_local_time(Now),
  Filename = "archive/" ++ integer_to_list(Yr) ++ pad(integer_to_list(Mo)) ++ pad(integer_to_list(Dy)) ++ "/" ++ Channel,
  case filelib:ensure_dir(Filename) of
    ok ->
      case file:open(Filename, [ append ]) of
        { ok, Iod } ->
          try io:format(Iod, "~1024000p.~n", [ { Now, User, Text } ]) of
            ok ->
              file:close(Iod),
              ok
          catch
            _:_ ->
              { error, "Write to archive has failed" }
          end
          ;
        { error, Reason } ->
          { error, Reason }
      end
      ;
    { error, Reason } ->
      { error, Reason }
  end.




% Remove an archived row from the cache.

remove_archived_row(Node, Channel, ThisRow) ->
  Transaction = fun() ->
    [{ _, _, First }] = mnesia:read({ rows, { first, Channel, Node } }),
    if
      First == ThisRow ->
        mnesia:delete({ rows, { Node, Channel, ThisRow } }),
        mnesia:write({ rows, { first, Channel, Node }, ThisRow + 1 })
        ;
      true ->
        mnesia:abort(row_isnt_first)
    end
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:info_warning("Error: Archiver error - remove_archived_row failed: ~p~n", [ Reason ])
      ;
    { atomic, _ } ->
      ok
  end.
