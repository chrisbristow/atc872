ATC872
======
Overview
--------
TBC

TBC
---
* Mnesia set-up:
mnesia:create_schema(['a1@Doofus']).
mnesia:start().
mnesia:create_table(rows, [{ attributes, [ id, content ]}, { disc_copies, ['a1@Doofus'] }]).

* Testing the REST interfaces:
curl -d "channel=ch1&user=nank&from=-1&back=200" http://localhost:8686/fetchrows
curl -d "channel=The%20Rebooting%20Of%20The%20Things&back=200&pattern=what" http://localhost:8686/searchrows
curl -d "channel=ch1&user=nank&text=XXXX&from=-1&back=200" http://localhost:8686/addrow

{ {1365,245875,170506}, {"stuffage"}, {"pw1"} }.
{ {1365,245913,892208}, {"pilot"}, {"woo"} }.

{ ok, Iod } = file:open("/tmp/d.txt").
{ ok, Data } = file:read_line(Iod).
{ ok, Tokens, _ } = erl_scan:string(Data).
{ ok, Term } = erl_parse:parse_term(Tokens).
file:close(Iod).
