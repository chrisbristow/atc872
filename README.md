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
