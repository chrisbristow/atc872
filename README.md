ATC872
======
Overview
--------
ATC872 allows teams to share information.  It provides a web browser-based, collaborative, live-chat
and simple blogging service, with keyword searching.  Users contribute and converse within "channels" which
are specified within the URL used.

It is build using Erlang (http://www.erlang.org/) and is designed to run across multiple hosts for
resilience.  The web server component is implemented using Misultin (https://github.com/ostinelli/misultin).
It's web-based front end uses MooTools (http://mootools.net/) in order to provide a slick, consistent,
cross-browser experience.

Quick Start
-----------
1) Download Erlang from http://www.erlang.org/ and install.

2) Download ATC872 from xxxx and place atc872.erl and web (directory) in the same directory.

3) Download Misultin from https://github.com/ostinelli/misultin, compile, and place in a directory
   alongside atc872.erl and web.

4) Download mootools.js and place in the web directory.  Make sure it is named "mootools.js".

5) Compile atc872.erl:
   - erlc atc872.erl

6) Set up Erlang's Mnesia database:
   - erl -sname atc -setcookie atc872
   - mnesia:create_schema(['-- INSERT ERLANG NODE NAME HERE --']).
   - mnesia:start().
   - mnesia:create_table(rows, [{ attributes, [ id, content ]}, { disc_copies, ['-- INSERT ERLANG NODE NAME HERE --'] }]).
   - q().

7) Start ATC872:
   - erl -sname atc -setcookie atc872 -noshell -pa ostinelli-misultin-59a72fd/ebin -s atc872 start 8686 0 1 250

   - 8686 is the port the web server will listen on.
   - 0 is the ID of this ATC872 node.
   - 1 is the number of ATC872 nodes in the cluster.
   - 250 is the number of records per channel to hold in Mnesia - older records are archived to disk.

8) Point your web browser at the server - URL:
   http://[hostname]:8686/atc872.html?channel=Channel%20One

9) Chat !!

Testing the REST interfaces
---------------------------
curl -d "channel=ch1&user=nank&from=-1&back=200" http://localhost:8686/fetchrows
curl -d "channel=The%20Rebooting%20Of%20The%20Things&back=200&pattern=what" http://localhost:8686/searchrows
curl -d "channel=ch1&user=nank&text=XXXX&from=-1&back=200" http://localhost:8686/addrow
