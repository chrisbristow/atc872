ATC872
======
Overview
--------
ATC872 is a multi-channel chat server.  Users access ATC872 via a web-browser in order to follow,
contribute to, and search in conversational channels.

ATC872 is built using Erlang (http://www.erlang.org/) and is designed to run across multiple hosts for
resilience.  The web server component is implemented using Misultin (https://github.com/ostinelli/misultin).
It's web-based front end uses MooTools (http://mootools.net/) in order to provide a slick, consistent,
cross-browser experience.

Quick Start
-----------
1) Download Erlang from http://www.erlang.org/ and install.

2) Download ATC872 from https://github.com/chrisbristow/atc872 and place atc872.erl and web (directory)
   in the same directory.

3) Download Misultin from https://github.com/ostinelli/misultin, compile, and place in a directory
   alongside atc872.erl and web.

4) Download mootools.js and place in the web directory.  Make sure it is named "mootools.js".

5) Compile atc872.erl:
   - erlc atc872.erl

6) Set up Erlang's Mnesia database:
   - erl -sname atc -setcookie atc872
   - mnesia:create_schema([node()]).
   - mnesia:start().
   - mnesia:create_table(rows, [{ attributes, [ id, content ]}, { disc_copies, [node()] }]).
   - q().

7) Start ATC872:
   - erl -sname atc -setcookie atc872 -noshell -pa ostinelli-misultin-59a72fd/ebin -s atc872 start 8686 250
     - 8686 is the port the web server will listen on.
     - 250 is the number of records per channel to hold in Mnesia - older records are archived to disk.

8) Point your web browser at the server - URL:
   http://[hostname]:8686/atc872.html?channel=ChannelOne

9) Chat !!

In order to run a cluster of ATC872 server, step 6, above should be changed to:

On all hosts in the cluster, run:

- erl -sname atc -setcookie atc872

Then, on one node within the cluster, run:

- mnesia:create_schema([node(),'OTHER_NODE']).

Eg. "mnesia:create_schema([ node(), 'atc872@host2' ])."

Then, on all hosts in the cluster, run:

- mnesia:start().

Then, again on one node within the cluster, run:

- mnesia:create_table(rows, [{ attributes, [ id, content ]}, { disc_copies, [node(),'OTHER_NODE'] }]).

Eg. "mnesia:create_table(rows, [{ attributes, [ id, content ]}, { disc_copies, [ node(), 'atc872@host2' ] }])."

Finally, exit the Erlang shell on all hosts, and start ATC872 on all hosts, as per step 7:

- erl -sname atc -setcookie atc872 -noshell -pa ostinelli-misultin-59a72fd/ebin -s atc872 start 8686 250

Browsers can be pointed at any instance in the cluster.

Closing the Server Down
-----------------------
The ATC872 server periodically checks for a file called "atc872.stop".  If this file is
present, the server will delete the file and then perform a clean shutdown.

Testing the REST interfaces
---------------------------
The following curl commands can be used for testing the various web interfaces available from ATC872:

- curl -d "channel=ch1&user=nank&from=-1&back=200" http://localhost:8686/fetchrows
- curl -d "channel=The%20Channel&back=200&pattern=what" http://localhost:8686/searchrows
- curl -d "channel=ch1&user=nank&text=XXXX&from=-1&back=200" http://localhost:8686/addrow
- curl -d "" http://localhost:8686/listchannels
