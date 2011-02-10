# Erlang HandlerSocket (erl\_hs)

erl\_hs is a small application that will allow Erlang to chatter with the underlying storage in MySQL. If you don't know
about HandlerSocket, please visit [HandlerSocket on github](https://github.com/ahiguti/HandlerSocket-Plugin-for-MySQL "ahiguti HandlerSocket") and [Yoshinori Matsunobu's blog](http://yoshinorimatsunobu.blogspot.com/2010/10/using-mysql-as-nosql-story-for.html "Yoshinori Matsunobu's blog").

As of this commit, it is not recommended to use this code for anything. Unless you like to watch your computer melt, then by all means, melt it. I just threw this together quickly as a proof of concept. It will however, become something useful in time.

## Usage

    > make
	> erl -pa ebin
	
	1> application:start(hs_app).
	ok
	2> Pid = hs_pool_manager:get_connection().
	<0.57.0>
	3> hs:open_index(Pid, "1", "db", "users", "PRIMARY", "id,nickname"). 
	ok
	4> hs:select(Pid, "1", "=", "2").
	{ok,[{<<"2">>,<<"mJXz0HlgmR97AXeH5TEIZoi">>}]}
	5> hs:select(Pid, "1", "<", "10", "4").
	{ok,[{<<"9">>,<<"hLvs9gL55oj2R5D5M0trO">>},
	     {<<"8">>,<<"qUW1443">>},
	     {<<"7">>,<<"1x6x5TRWXo3bQb6kugCN8d316">>},
	     {<<"6">>,<<"qa80h2AlzCj25SE2r6z57XBR">>}]}
	
The only two functions working are open_index and select.

## Config

Look at the .app file. Most things aren't implemented. Oops.

## Author

Joseph Lambert 

Current pooling idea from this Gist: https://gist.github.com/790556 by djnym

This will change later, I just needed quick, simple pooling code.

