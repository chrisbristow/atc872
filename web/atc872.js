var user = "";
var channel = "";
var latest_row = 0;

window.addEvent('domready',function()
{
  channel = (location.search).replace("?channel=","");

  if(channel.length == 0)
  {
    $("area51").innerHTML = "Error: No channel specified !!";
  }
  else
  {
    user = Cookie.read("user");

    if(user == null)
    {
      $("area51").innerHTML = "Name: <input type=\"text\" id=\"auser\" onkeyup=\"enter_userid(event);\">";
    }
    else
    {
      init_channel();
    }
  }
});

function enter_userid(event)
{
  if(event.keyCode == 13)
  {
    user = $("auser").value;
    Cookie.write('user', user, { duration: 365 });
    init_channel();
  }
}

function init_channel()
{
  var ajax=new Request({ url: "fetchrows", onSuccess: function(responseText, responseXML) { fetched_rows(responseText) } });
  ajax.post("channel="+channel+"&from=-1&back=20");
}

function fetched_rows(responseText)
{
  var obj = JSON.decode(responseText);

  if(obj.status == "ok")
  {
    var render = "";

    latest_row = obj.latest;

    render += "("+latest_row+")<br>";

    for(var i = 0; i < obj.rows.length; i ++)
    {
      var tm = obj.rows[i].time;
      var us = obj.rows[i].user;
      var tx = obj.rows[i].text;

      render += tm+" "+us+" "+tx+"<br>";
    }

    $("area51").innerHTML = render;
  }
  else
  {
    alert("Error: Rows could not be fetched");
  }
}






function go_home()
{
  $('pagename').value="home";
  fetch_internal();
}

function go_link(s)
{
  $('pagename').value=s;
  fetch_internal();
}

function fetch_internal()
{
  var url="loadpage/"+$('pagename').value;
  var ajax=new Request.HTML({ url: url,update: $('mainarea') }).get({ 'session': $random(1000,1000000) });
}

function edit_this_page()
{
  var url="loadsrc/"+$('pagename').value;
  var ajax=new Request.HTML({ url: url,update: $('pagesrc'),onSuccess: function(responseText,responseXML) { $('edittext').value=$('pagesrc').innerHTML; }}).get({ 'session': $random(1000,1000000) });
  $('editarea').setStyle("display","block");
  $('errorbox').setStyle("display","none");
  $('searchbox').setStyle("display","none");
}

function save_page()
{
  var ajax=new Request.HTML({ url: "savepage",update: $('mainarea') });
  ajax.post("pagename="+$('pagename').value+"&content="+encodeURIComponent($('edittext').value));
  $('editarea').setStyle("display","none");
  $('errorbox').setStyle("display","none");
  $('searchbox').setStyle("display","none");
}

function cancel_page()
{
  $('editarea').setStyle("display","none");
  $('errorbox').setStyle("display","none");
  $('searchbox').setStyle("display","none");
}

function display_error(s)
{
  $('errormessage').innerHTML=s;
  $('editarea').setStyle("display","none");
  $('errorbox').setStyle("display","block");
  $('searchbox').setStyle("display","none");
}

function open_search_box()
{
  $('searchbox').setStyle("display","block");
  $('editarea').setStyle("display","none");
  $('errorbox').setStyle("display","none");
}

function get_page_list()
{
  var ajax=new Request.HTML({ url: "listpages",update: $('mainarea') }).get({ 'session': $random(1000,1000000) });
}

function do_search()
{
  if(($('searchtext').value).length>0)
  {
    var ajax=new Request.HTML({ url: "searchfor",update: $('mainarea') }).get({ 'searchfor': $('searchtext').value,'session': $random(1000,1000000) });
    $('editarea').setStyle("display","none");
    $('errorbox').setStyle("display","none");
    $('searchbox').setStyle("display","none");
  }
}
