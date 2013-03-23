var user = "";
var channel = "";
var latest_row = -1;
var poll_timer = null;




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
      $("area51").innerHTML = "<p class=\"rowtextcss\">Enter your name here: <input class=\"usertextinputcss\" type=\"text\" id=\"auser\" onkeyup=\"enter_userid(event);\"></p>";
    }
    else
    {
      init_channel();
    }
  }
}




function enter_userid(event)
{
  if(event.keyCode == 13)
  {
    user = $("auser").value;
    Cookie.write('user', user, { duration: 365 });
    init_channel();
  }
}




function enter_usertext(event)
{
  if(event.keyCode == 13)
  {
    var ajax=new Request({ url: "addrow", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
    ajax.post("channel="+channel+"&user="+user+"&text="+encodeURIComponent($("usertextinput").value)+"&from="+latest_row+"&back=200");
    $("usertextinput").value = "";
    clearTimeout(poll_timer);
    poll_timer = setTimeout("do_poll()", 2000);
  }
}




function do_poll()
{
  clearTimeout(poll_timer);
  var ajax=new Request({ url: "fetchrows", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
  ajax.post("channel="+channel+"&from="+latest_row+"&back=200");
  poll_timer = setTimeout("do_poll()", 10000);
}




function init_channel()
{
  $("area51").innerHTML  = "<p class=\"channelcss\">"+channel+" (<a href=\"#\" onmouseup=\"change_user();\">"+user+"</a>)</p>";
  $("area51").innerHTML += "<p><input class=\"usertextinputcss\" type=\"text\" id=\"usertextinput\" onkeyup=\"enter_usertext(event);\"></p>";
  $("area51").innerHTML += "<p id=\"textlist\"></p>";

  do_poll();
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
