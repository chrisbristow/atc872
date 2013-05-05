% atc872.erl
%
% Version: 0.41
%
% Server for the ATC872 online forum service.
%
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
-export([ start/1, add_row/5, add_row/6, fetch_rows/4, search_rows/3, archiver/1, full_archive/1, get_channel_list/0, channel_renderer/1, delete_channel/1 ]).




% Start hook.  Takes arguments:
% - TCP port for the web server.
% - Number of rows to cache before the archiver writes them to disk.
% Initialises Mnesia and starts the Misultin web server.

start([ Port, CachedRows ]) ->
  process_flag(trap_exit, true),

  error_logger:info_msg("Starting node: ~p~n", [ node() ]),
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
  archiver(list_to_integer(atom_to_list(CachedRows))).




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
    { LatestRow, no_rows, UserList } ->
      Req:ok("{ \"status\": \"no_rows\"," ++
             "  \"latest\": " ++ integer_to_list(LatestRow) ++ "," ++
             "  \"users\": \"" ++ UserList ++ "\"}")
      ;
    { LatestRow, RowList, UserList } ->
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
             "  ]," ++[10]++
             "  \"users\": \"" ++ UserList ++ "\"" ++[10]++
             "}" ++[10]
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



% Render a JSON object containing a list of all channels.

channel_renderer(Req) ->
  case get_channel_list() of
    [] ->
      Req:ok("{ \"status\": \"no_channels\" }")
      ;
    Clist ->
      Formatted = lists:map(fun({ Channel, Time, User }) ->
        "{ \"channel\": \"" ++ Channel ++ "\", \"time\": \"" ++ now_to_string(Time) ++ "\", \"user\": \"" ++quote_handler(User) ++ "\" }"
      end, Clist),

      Channels = string:join(Formatted, ","),

      Req:ok("{ \"status\": \"ok\"," ++ [10] ++
             "  \"channels\": [" ++[10]++
             Channels ++[10]++
             "  ]" ++[10]++
             "}")
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
  RowData = add_row(Channel, User, Text, list_to_integer(LastRow), list_to_integer(RowsBack)),
  row_renderer(RowData, Req),
  log_rt("Add row (user:" ++ User ++ " channel:" ++ Channel ++ " text:" ++ Text ++ " from:" ++ LastRow ++ " back:" ++ RowsBack ++ ")", Ctime)
  ;
handle('POST', ["fetchrows"], Req) ->
  Ctime = now(),
  [{ "channel", Channel }, { "user", User }, { "from", LastRow }, { "back", RowsBack }] = Req:parse_post(),
  RowData = fetch_rows(Channel, User, list_to_integer(LastRow), list_to_integer(RowsBack)),
  row_renderer(RowData, Req),
  log_rt("Fetch rows (user:" ++ User ++ " channel:" ++ Channel ++ " from:" ++ LastRow ++ " back:" ++ RowsBack ++ ")", Ctime)
  ;
handle('POST', ["searchrows"], Req) ->
  Ctime = now(),
  [{ "channel", Channel }, { "back", RowsBack }, { "pattern", Pattern }] = Req:parse_post(),
  RowData = search_rows(Channel, list_to_integer(RowsBack), Pattern),
  row_renderer(RowData, Req),
  log_rt("Search (channel:" ++ Channel ++ " back:" ++ RowsBack ++ " pattern:" ++ Pattern ++ ")", Ctime)
  ;
handle('POST', ["listchannels"], Req) ->
  Ctime = now(),
  channel_renderer(Req),
  log_rt("List channels", Ctime)
  ;
handle('GET', [Static], Req) ->
  Req:file("web/"++Static).




% Prefixes a single digit with a "0", ie. "1" becomes "01" and "11" is left as-is.

pad(Str) ->
  case length(Str) of
    1 -> "0" ++ Str;
    _ -> Str
  end.




% Add a row to the database and return all new rows (including the row added).
% The function containing the argument "Now" allows the row to be given a specific date, otherwise
% the current date/time is used.

add_row(Channel, User, Text, LastRow, RowsBack) ->
  add_row(Channel, User, Text, LastRow, RowsBack, now()).

add_row(Channel, User, Text, LastRow, RowsBack, Now) ->
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
    end, mnesia:table_info(rows, disc_copies)),

    case mnesia:read({ rows, channels }) of
      [ { _, _, UpdateList } ] ->
        mnesia:write({ rows, channels, lists:keystore(Channel, 1, UpdateList, { Channel, Now, User }) })
        ;
      [] ->
        mnesia:write({ rows, channels, lists:keystore(Channel, 1, [], { Channel, Now, User }) })
    end,

    get_rows(Channel, LastRow, RowsBack)
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

get_rows(Channel, LastRow, RowsBack) ->
  case mnesia:read({ rows, { last, Channel, node() } }) of
    [ { _, _, Last } ] ->
      Before = subtract_time(now(), 30000000),

      ListOfUsers = case mnesia:read({ rows, { users, Channel } }) of
        [ { _, _, Ulist } ] ->
          ListOfUserTuples = lists:filter(fun({ _, Time }) ->
            if
              Time < Before -> false;
              true -> true
            end
          end, Ulist),
          string:join(lists:map(fun({ User, _ }) -> User end, ListOfUserTuples), ", ")
          ;
        _ ->
          ""
      end,

      if
        Last > LastRow ->
          { Last, get_a_row([], Channel, Last, LastRow, RowsBack), ListOfUsers }
          ;
        true -> { Last, no_rows, ListOfUsers }
      end
      ;
    [] ->
      no_such_channel
  end.



% Recursively fetch as many rows as possible, until:
% - RowsBack is reached, or
% - The start of the cache is reached.

get_a_row(Result, Channel, CurrentRow, FirstRow, RowsBack) ->
  if
    CurrentRow == FirstRow ->
      Result
      ;
    length(Result) >= RowsBack ->
      Result
      ;
    true ->
      case mnesia:read({ rows, { node(), Channel, CurrentRow } }) of
        [ { _, _, R } ] ->
          get_a_row([ R ] ++ Result, Channel, CurrentRow - 1, FirstRow, RowsBack)
          ;
        _ ->
          Result
      end
  end.




% Recursive search function.  Searches the cache in reverse until:
% - RowsBack rows have been found.
% - The start of the cache ("first") is reached.

search_row(Channel, RowsBack, SearchString, Row, MatchCount, MatchList) ->
  if
    MatchCount > RowsBack ->
      MatchList
      ;
    true ->
      case mnesia:read({ rows, { node(), Channel, Row } }) of
        [ { _, _, { Now, User, Text } } ] ->
          case re:run(Text, SearchString, [{ capture, none }, caseless]) of
            match ->
              search_row(Channel, RowsBack, SearchString, Row - 1, MatchCount + 1, MatchList ++ [{ Now, User, Text }])
              ;
            _ ->
              case re:run(User, SearchString, [{ capture, none }, caseless]) of
                match ->
                  search_row(Channel, RowsBack, SearchString, Row - 1, MatchCount + 1, MatchList ++ [{ Now, User, Text }])
                  ;
                _ ->
                  case re:run(now_to_string(Now), SearchString, [{ capture, none }, caseless]) of
                    match ->
                      search_row(Channel, RowsBack, SearchString, Row - 1, MatchCount + 1, MatchList ++ [{ Now, User, Text }])
                      ;
                    _ ->
                      search_row(Channel, RowsBack, SearchString, Row - 1, MatchCount, MatchList)
                  end
              end
          end
          ;
        _ -> MatchList
    end
  end.





% Search for a specific string in the Text and User parts the last N rows
% of a given channel.

get_search_results(Channel, RowsBack, SearchString) ->
  case mnesia:read({ rows, { last, Channel, node() } }) of
    [ { _, _, Last } ] ->
      Results = lists:reverse(search_row(Channel, RowsBack, SearchString, Last, 0, [])),

      if
        length(Results) < RowsBack ->
          [ { _, _, Fst } ] = mnesia:read({ rows, { first, Channel, node() } }),
          [ { _, _, { First, _, _ } } ] = mnesia:read({ rows, { node(), Channel, Fst } }),
          [ { _, _, { Created, _ } } ] = mnesia:read({ rows, { created, Channel, node() } }),
          { Results, First, Created }
          ;
        true ->
          Results
      end
      ;
    [] ->
      no_such_channel
  end.




% Return the last N rows after the specified row ID on a given channel.

fetch_rows(Channel, User, LastRow, RowsBack) ->
  Transaction = fun() ->
    case mnesia:read({ rows, { users, Channel } }) of
      [{ _, _, CurrentUserList }] ->
        mnesia:write({ rows, { users, Channel }, lists:keystore(User, 1, CurrentUserList, { User, now() }) })
        ;
      [] ->
        mnesia:write({ rows, { users, Channel }, [{ User, now() }] })
    end,
    get_rows(Channel, LastRow, RowsBack)
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

search_rows(Channel, RowsBack, SearchString) ->
  Transaction = fun() ->
    get_search_results(Channel, RowsBack, SearchString)
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Search rows failed: ~p~n", [Reason]),
      error
      ;
    { atomic, no_such_channel } ->
      no_such_channel
      ;
    { atomic, { ResultList, First, Created } } ->
      { -1, search_archive_files(ResultList, First, subtract_time(Created, 86400000000), Channel, RowsBack, SearchString), "" }
      ;
    { atomic, Rval } ->
      { -1, Rval, "" }
  end.




% Archiver main loop.  As well as periodically calling do_archive(), this
% function also checks to see if the file "atc872.stop" has been created.  If
% this file is present, then it will be deleted and the server will close itself
% down cleanly.

archiver(CachedRows) ->
  Transaction = fun() ->
    case mnesia:read({ rows, { updated, node() } }) of
      [] ->
        []
        ;
      [{ _, _, [] }] ->
        []
        ;
      [{ _, _, UpdateList }] ->
        mnesia:delete({ rows, { updated, node() } }),
        UpdateList
    end
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Archiver error: ~p~n", [ Reason ]),
      timer:sleep(10000),
      archiver(CachedRows)
      ;
    { atomic, [] } ->
      case filelib:is_file("atc872.stop") of
        true ->
          error_logger:info_msg("Warning: File atc872.stop has been found - closing down~n"),
          file:delete("atc872.stop"),
          init:stop()
          ;
        _ ->
          timer:sleep(5000),
          archiver(CachedRows)
      end
      ;
    { atomic, UpdateList } ->
      lists:foreach(fun({ Channel, _ }) ->
        do_archive(Channel, CachedRows, 0)
      end, UpdateList),
      archiver(CachedRows)
      ;
    Else ->
      error_logger:warning_msg("Error: Archiver error: ~p~n", [ Else ]),
      archiver(CachedRows)
  end.




% Start archiving.  Will leave "CachedRows" in Mnesia.

do_archive(Channel, CachedRows, N) ->
  Transaction = fun() ->
    case mnesia:read({ rows, { first, Channel, node() } }) of
      [{ _, _, First }] ->
        [{ _, _, Last }] = mnesia:read({ rows, { last, Channel, node() } }),

        if
          (Last - First) > CachedRows ->
            [{ _, _, { Now, User, Text } }] = mnesia:read({ rows, { node(), Channel, First } }),
            { archive_this, First, Now, User, Text }
            ;
          true ->
            []
        end
        ;
      [] ->
        []
    end
  end,

  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Archiver error: ~p~n", [ Reason ])
      ;
    { atomic, { archive_this, First, Now, User, Text } } ->
      case write_row_to_file(Channel, Now, User, Text) of
        ok ->
          remove_archived_row(Channel, First),
          timer:sleep(100),
          do_archive(Channel, CachedRows, N + 1)
          ;
        { error, Reason } ->
          error_logger:warning_msg("Error: Archiver error - write_row_to_file failed: ~p~n", [ Reason ])
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




% Generate an archive file name from a now().

get_archive_file_name(Channel, Now) ->
  { { Yr, Mo, Dy }, _ } = calendar:now_to_local_time(Now),
  "atc872." ++ atom_to_list(node()) ++ "/" ++ integer_to_list(Yr) ++ pad(integer_to_list(Mo)) ++ pad(integer_to_list(Dy)) ++ "/" ++ Channel.




% Append a row to an archive file.

write_row_to_file(Channel, Now, User, Text) ->
  Filename = get_archive_file_name(Channel, Now),
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




% Remove an archived row from the cache.  Called after a row has been successfully
% appended to an archive disk file.

remove_archived_row(Channel, ThisRow) ->
  Transaction = fun() ->
    [{ _, _, First }] = mnesia:read({ rows, { first, Channel, node() } }),
    if
      First == ThisRow ->
        mnesia:delete({ rows, { node(), Channel, ThisRow } }),
        mnesia:write({ rows, { first, Channel, node() }, ThisRow + 1 })
        ;
      true ->
        mnesia:abort(row_isnt_first)
    end
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Archiver error - remove_archived_row failed: ~p~n", [ Reason ])
      ;
    { atomic, _ } ->
      ok
  end.



% Continue searching through the archived files for a channel.
% This is a recursive function which steps backwards one archive file at a time
% until it reaches a point just before the "created" time for a channel.
% If an archive file doesn't exist for any particular day, then that day
% is simply skipped.  If an archive file is found for a particular day, then
% the file is opened and it's file descriptor is passed to the read_a_line()
% function which recursively searches all lines in that file.

search_archive_files(ResultList, First, Created, Channel, RowsBack, SearchString) ->
  if
    length(ResultList) >= RowsBack ->
      ResultList
      ;
    true ->
      ArchiveName = get_archive_file_name(Channel, First),

      FileResults = case file:open(ArchiveName, read) of
        { ok, Iod } ->
          FileLines = read_a_line(Iod, [], length(ResultList), RowsBack, SearchString),
          file:close(Iod),
          lists:reverse(FileLines)
          ;
        { error, _Reason } ->
          []
      end,

      NewFst = subtract_time(First, 86400000000),

      if
        NewFst < Created ->
          FileResults ++ ResultList
          ;
        true ->
          search_archive_files(FileResults ++ ResultList, NewFst, Created, Channel, RowsBack, SearchString)
      end
  end.




% Subtract a microsecond time from a now(), returning a new now().

subtract_time(Now, UsTime) ->
  { Ms, Sc, Us } = Now,
  NewVal = ((Ms * 1000000000000) + (Sc * 1000000) + Us) - UsTime,
  NewMs = NewVal div 1000000000000,
  NewSc = (NewVal - (NewMs * 1000000000000)) div 1000000,
  NewUs = NewVal - (NewMs * 1000000000000) - (NewSc * 1000000),
  { NewMs, NewSc, NewUs }.





% Find a matching line in an archive file.  Like the cache search, row "text"
% is searched first, if no match then row "user", if no match then row "date".
% This is a recursive function which checks for matches within
% a given file.  Matches are added to an accumulator list until either:
% - The end of the file is reached.
% - Or, the total number of matches equals "RowsBack".  (Total number of matches
%   so far, for cache search matches plus file matches is provided in "PrevLen".)
% - Or, a failure occurs when trying to read from the file descriptor.
% Known Issue: Files are searched oldest -> newest (as that is the order that
% they are written).  Whereas the cache is searched newest -> oldest.  This
% means that if RowsBack is reached whilst searching a file, then it is
% possible that newer rows within that file will be omitted from the results,
% yet older ones (up to RowsBack) will be included.

read_a_line(Iod, Acc, PrevLen, RowsBack, SearchString) ->
  if
    PrevLen >= RowsBack ->
      Acc
      ;
    true ->
      case file:read_line(Iod) of
        { ok, Data } -> 
          case erl_scan:string(Data) of
            { ok, Tokens, _ } ->
              case erl_parse:parse_term(Tokens) of
                { ok, { Now, User, Text } } ->
                  case re:run(Text, SearchString, [{ capture, none }, caseless]) of
                    match ->
                      read_a_line(Iod, [{ Now, User, Text }] ++ Acc, PrevLen + 1, RowsBack, SearchString)
                      ;
                    _ ->
                      case re:run(User, SearchString, [{ capture, none }, caseless]) of
                        match ->
                          read_a_line(Iod, [{ Now, User, Text }] ++ Acc, PrevLen + 1, RowsBack, SearchString)
                          ;
                        _ ->
                          case re:run(now_to_string(Now), SearchString, [{ capture, none }, caseless]) of
                            match ->
                              read_a_line(Iod, [{ Now, User, Text }] ++ Acc, PrevLen + 1, RowsBack, SearchString)
                              ;
                            _ ->
                              read_a_line(Iod, Acc, PrevLen, RowsBack, SearchString)
                          end
                      end
                  end
                  ;
                _ ->
                  read_a_line(Iod, Acc, PrevLen, RowsBack, SearchString)
              end
              ;
            _ ->
              read_a_line(Iod, Acc, PrevLen, RowsBack, SearchString)
          end
          ;
        eof ->
          Acc
          ;
        { error, Reason } ->
          error_logger:warning_msg("Error: Read_line failed: ~p~n", [ Reason ]),
          Acc
      end
  end.



% Archive an entire channel to disk.  This is generally called from an
% external shell to archive channels which are no longer in use.  Their first, last and created
% records remain in Mnesia meaning that they are still searchable.

full_archive(Channel) ->
  add_row(Channel, "Archiver", "This channel has been archived.", -1, 1),
  do_archive(Channel, 0, 0).




% Delete a channel.  To be called from an external shell.  This will
% remove all traces of a channel in Mnesia from all nodes.  Note - will
% not remove any archive files from disk.

delete_channel(Channel) ->
  Transaction = fun() ->
    lists:foreach(fun(I) ->
      case mnesia:read({ rows, { last, Channel, I } }) of
        [ { _, _, L } ] ->
          [ { _, _, F } ] = mnesia:read({ rows, { first, Channel, I } }),

          lists:foreach(fun(N) ->
            mnesia:delete({ rows, { I, Channel, N } })
          end, lists:seq(F, L, 1)),

          mnesia:delete({ rows, { created, Channel, I } }),
          mnesia:delete({ rows, { first, Channel, I } }),
          mnesia:delete({ rows, { last, Channel, I } })
          ;
        [] ->
          no_such_channel
      end,

      case mnesia:read({ rows, { updated, I } }) of
        [ { _, _, UpdateList } ] ->
          mnesia:write({ rows, { updated, I }, lists:keydelete(Channel, 1, UpdateList) })
          ;
        [] ->
          ok
      end
    end, mnesia:table_info(rows, disc_copies)),

    mnesia:delete({ rows, { users, Channel } }),

    case mnesia:read({ rows, channels }) of
      [ { _, _, UpdateList } ] ->
        mnesia:write({ rows, channels, lists:keydelete(Channel, 1, UpdateList) }),
        ok
        ;
      [] ->
        ok
    end
  end,

  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Delete channel failed: ~p~n", [Reason]),
      error
      ;
    { atomic, Rval } ->
      Rval
  end.



% Fetch a sorted list of all channels.

get_channel_list() ->
  Transaction = fun() ->
    case mnesia:read({ rows, channels }) of
      [{ _, _, List }] ->
        List
        ;
      [] ->
        []
    end
  end,
  case mnesia:transaction(Transaction) of
    { aborted, Reason } ->
      error_logger:warning_msg("Error: Channel fetch failed: ~p~n", [ Reason ]),
      []
      ;
    { atomic, [] } ->
      []
      ;
    { atomic, Clist } ->
      lists:reverse(lists:keysort(2, Clist))
  end.
