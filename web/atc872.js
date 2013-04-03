// atc872.js - Part of the ATC872 project which delivers a low-maintenance online
//             chat forum service.
//
// Copyright (c) 2013, Chris Bristow
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met: 
// 
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer. 
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution. 
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// 
// The views and conclusions contained in the software and documentation are those
// of the authors and should not be interpreted as representing official policies, 
// either expressed or implied, of the FreeBSD Project.




// Globals.

var max_extent = 200;
var user = "";
var channel = "";
var latest_row = -1;
var poll_timer = null;
var short_poll_interval = 2000;
var long_poll_interval = 10000;
var poll_interval = long_poll_interval;
var poll_acc = 0;
var poll_acc_ticks = 30;
var flash_timer = null;
var flash_interval = 500;
var flash_ticks = 0;
var flash_wait = 10000;
var last_entry = 0;





// This runs when the page has fully loaded.  The channel is collected from
// the URL and control passed to the initialisation function.

window.addEvent('domready', function()
{
  channel = decodeURIComponent((location.search).replace("?channel=", ""));
  initialise();
});





// If there is a "user" cookie set, jump straight into the channel, otherwise prompt
// for a username.  Also, displays an error if the page is loaded without a channel
// specified.

function initialise()
{
  clearTimeout(poll_timer);

  if(channel.length == 0)
  {
    $("area51").innerHTML = "Error: No channel specified !!";
  }
  else
  {
    user = Cookie.read("user");

    if(user == null || user.length == 0)
    {
      $("area51").innerHTML  = "<div class=\"rowdatecss\">Enter your name here:</div>";
      $("area51").innerHTML += "<div><input class=\"usertextinputcss\" type=\"text\" id=\"auser\" onkeyup=\"enter_userid(event);\"></div>";
    }
    else
    {
      init_channel();
    }
  }
}




// Verify that the user name entered conforms, then jump to the channel view.

function enter_userid(event)
{
  if(event.keyCode == 13 && $("auser").value.length > 1 && $("auser").value.length < 100 && /\S+/.test($("auser").value))
  {
    user = $("auser").value;
    Cookie.write('user', user, { duration: 365 });
    init_channel();
  }
}




// Verify that an entered search term is valid, then post a search request to the server.

function enter_search(event)
{
  if(event.keyCode == 13 && $("asearch").value.length > 1 && /\S+/.test($("asearch").value))
  {
    $("textlist").innerHTML = "";

    var ajax=new Request({ url: "searchrows", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
    ajax.post("channel="+channel+"&back="+max_extent+"&pattern="+$("asearch").value);
  }
}




// Verify that a text line is valid, post it to the server and re-start polling (at the fast rate).

function enter_usertext(event)
{
  if(event.keyCode == 13 && $("usertextinput").value.length > 0 && /\S+/.test($("usertextinput").value))
  {
    var ajax=new Request({ url: "addrow", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
    ajax.post("channel="+channel+"&user="+user+"&text="+encodeURIComponent($("usertextinput").value)+"&from="+latest_row+"&back="+max_extent);

    $("usertextinput").value = "";
    poll_interval = short_poll_interval;
    poll_acc = poll_acc_ticks;
    last_entry = (new Date()).getTime();
    clearTimeout(poll_timer);
    poll_timer = setTimeout("do_poll()", poll_interval);
  }
}




// Post check-for-update request to the server.  Switch to the slow polling rate if there has been
// no user input recently.

function do_poll()
{
  clearTimeout(poll_timer);

  var ajax=new Request({ url: "fetchrows", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
  ajax.post("channel="+channel+"&user="+user+"&from="+latest_row+"&back="+max_extent);

  if(poll_acc > 0)
  {
    poll_interval = short_poll_interval;
    poll_acc --;
  }
  else
  {
    poll_interval = long_poll_interval;
  }

  poll_timer = setTimeout("do_poll()", poll_interval);
}



// Flash the title bar to indicate that an update has arrived.

function do_flash()
{
  clearTimeout(flash_timer);

  if(flash_ticks > 0)
  {
    document.title = document.title == channel ? "**********" : channel;
    flash_timer = setTimeout("do_flash()", flash_interval);
    flash_ticks --;
  }
  else
  {
    document.title == channel;
  }
}




// Render the channel view page structure and initiate polling.

function init_channel()
{
  latest_row = -1;
  document.title = channel;

  $("area51").innerHTML  = "<div class=\"channelcss\">"+channel+"</div><br>";
  $("area51").innerHTML += "<div class=\"rowdatecss\">"+user+" says:</div>";
  $("area51").innerHTML += "<div><input class=\"usertextinputcss\" type=\"text\" id=\"usertextinput\" onkeyup=\"enter_usertext(event);\"></div>";
  $("area51").innerHTML += "<div><a class=\"rowdatecss\" href=\"#\" onmouseup=\"change_user();\">switch user</a> - <a class=\"rowdatecss\" href=\"#\" onmouseup=\"go_to_search();\">search</a></div>";
  $("area51").innerHTML += "<div id=\"textlist\"></div>";

  do_poll();
}




// Render the search page and prompt for a search term.

function go_to_search()
{
  clearTimeout(poll_timer);

  $("area51").innerHTML  = "<div class=\"rowdatecss\">Search in \""+channel+"\" for:</div>";
  $("area51").innerHTML += "<div><input class=\"usertextinputcss\" type=\"text\" id=\"asearch\" onkeyup=\"enter_search(event);\"></div>";
  $("area51").innerHTML += "<div><a class=\"rowdatecss\" href=\"#\" onmouseup=\"init_channel();\">return to channel</a></div>";
  $("area51").innerHTML += "<div id=\"textlist\"></div>";
}




// Update the "user" cookie and re-initialise with a new user name after switching user.

function change_user()
{
  user = "";
  latest_row = -1;
  Cookie.write('user', user, { duration: 365 });
  initialise();
}




// Render the channel text event list, or search result list.

function fetched_rows(responseText)
{
  var obj = JSON.decode(responseText);

  if(obj.status == "ok")
  {
    var d = new Date();

    if(latest_row != obj.latest && latest_row > -1 && d.getTime() > last_entry + flash_wait)
    {
      flash_ticks = 16;
      do_flash();
    }

    latest_row = obj.latest;

    for(var i = 0; i < obj.rows.length; i ++)
    {
      var tm = obj.rows[i].time;
      var us = obj.rows[i].user;
      var tx = obj.rows[i].text;

      $("textlist").innerHTML = "<br><div class=\"rowdatecss\">"+tm+" "+us+":</div><div class=\"rowtextcss\">"+tx+"</div>" + $("textlist").innerHTML;
    }
  }
}
