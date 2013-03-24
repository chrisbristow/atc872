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





var user = "";
var channel = "";
var latest_row = -1;
var poll_timer = null;
var poll_interval = 2000;
var max_extent = 200;




window.addEvent('domready',function()
{
  channel = decodeURIComponent((location.search).replace("?channel=",""));
  initialise();
});



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
      $("area51").innerHTML  = "<p class=\"rowdatecss\">Enter your name here:</p>";
      $("area51").innerHTML += "<p><input class=\"usertextinputcss\" type=\"text\" id=\"auser\" onkeyup=\"enter_userid(event);\"></p>";
    }
    else
    {
      init_channel();
    }
  }
}




function enter_userid(event)
{
  if(event.keyCode == 13 && $("auser").value.length > 1)
  {
    user = $("auser").value;
    Cookie.write('user', user, { duration: 365 });
    init_channel();
  }
}



function enter_search(event)
{
  if(event.keyCode == 13 && $("asearch").value.length > 1)
  {
    var ajax=new Request({ url: "searchrows", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
    ajax.post("channel="+channel+"&back="+max_extent+"&pattern="+$("asearch").value);
  }
}




function enter_usertext(event)
{
  if(event.keyCode == 13 && $("usertextinput").value.length > 0)
  {
    var ajax=new Request({ url: "addrow", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
    ajax.post("channel="+channel+"&user="+user+"&text="+encodeURIComponent($("usertextinput").value)+"&from="+latest_row+"&back="+max_extent);
    $("usertextinput").value = "";
    clearTimeout(poll_timer);
    poll_timer = setTimeout("do_poll()", poll_interval);
  }
}




function do_poll()
{
  clearTimeout(poll_timer);
  var ajax=new Request({ url: "fetchrows", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
  ajax.post("channel="+channel+"&user="+user+"&from="+latest_row+"&back="+max_extent);
  poll_timer = setTimeout("do_poll()", poll_interval);
}




function init_channel()
{
  latest_row = -1;

  $("area51").innerHTML  = "<p class=\"rowdatecss\">"+user+" - <a class=\"rowdatecss\" href=\"#\" onmouseup=\"change_user();\">switch user</a> - <a class=\"rowdatecss\" href=\"#\" onmouseup=\"go_to_search();\">search</a></p>";
  $("area51").innerHTML += "<p class=\"channelcss\">"+channel+"</p>";
  $("area51").innerHTML += "<p><input class=\"usertextinputcss\" type=\"text\" id=\"usertextinput\" onkeyup=\"enter_usertext(event);\"></p>";
  $("area51").innerHTML += "<p id=\"textlist\"></p>";

  do_poll();
}



function go_to_search()
{
  clearTimeout(poll_timer);

  $("area51").innerHTML  = "<p class=\"rowdatecss\">Search in \""+channel+"\" for:</p>";
  $("area51").innerHTML += "<p><input class=\"usertextinputcss\" type=\"text\" id=\"asearch\" onkeyup=\"enter_search(event);\"></p>";
  $("area51").innerHTML += "<p><a class=\"rowdatecss\" href=\"#\" onmouseup=\"init_channel();\">return to channel</a></p>";
  $("area51").innerHTML += "<p id=\"textlist\"></p>";
}



function change_user()
{
  user = "";
  latest_row = -1;
  Cookie.write('user', user, { duration: 365 });
  initialise();
}




function fetched_rows(responseText)
{
  var obj = JSON.decode(responseText);

  if(obj.status == "ok")
  {
    latest_row = obj.latest;

    for(var i = 0; i < obj.rows.length; i ++)
    {
      var tm = obj.rows[i].time;
      var us = obj.rows[i].user;
      var tx = obj.rows[i].text;

      $("textlist").innerHTML = "<p class=\"rowdatecss\">"+tm+" "+us+" :</p><p class=\"rowtextcss\">"+tx+"</p>" + $("textlist").innerHTML;
    }
  }
}
