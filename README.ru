Пока что это проверка концепций. Но я стараюсь, чтобы в репозитории был реально
работающий код. Так что достаточно склонировать репозиторий и начать работать.

С места в карьер
================

1. Установите exmpp, https://support.process-one.net/doc/display/EXMPP/Installation+guide
   Или просто выполните
      ./configure && make && sudo make install
   в директории deps/exmpp

2. Запустите make в директории exb

3. Переименуйте config.in в config. Установите требуемые опции

4. Запустите
      erl -pz ebin

5. Запустите
      application:start(exb).

6. Теперь с ботом можно "пообщаться" (он будет присылать обратно то, что вы ему 
   напишите)
   
   То еcть, если настройки для бота br@server.org, то достаточно добавить этот
   контакт в список контактов и начать с ним общаться.
   
    
Детально
============

Запуск
-------
На данный момент exb является самостоятельным ботом. То есть вам не надо устанвливать
сервер для работы. В будущем планируется дать возможность использовать exb как 
компонент сервера.

Config
------
Опции конфигурации сами говорят сами за себя. Если вы не знаете, что такое resource,
можно закомментировать эту строчку.

Если вы хотите, чтобы бот вел себя по разному, в зависимости от собеседника, можно
заставить его использовать разные наборы плагинов. В файле конфиг:
  [plugins,
  	%% plugin1, plugin2, plugin3 используются для всех
  	{default, [plugin1, plugin2, plugin3]},
  	
  	%% plugin2, plugin4 используются в разговоре с "user@server"
  	{"user@server", [plugin2, plugin4]},

  	%% plugin5 используется в разговоре с "user2@server2/resource"
  	{"user2@server2/resource", [plugin5]},
  ]

Reloader
--------
exb включает великолепный сервер reloader из проекта mochiweb's. Этот сервер следит
за изменениями в директории ebin. Таким образом можно заменять код налету. Доста-
точно выполнить команду make:all() в консоли Эрланга или make в директории exb,
и reloader перезагрузит измененные файлы. Даже не надо завершать работу приложения.

Плагины
-------
Плагины exb на данный момент реализованы как middleware-компоненты в веб-проектах
Python wsgi и Ruby Rack. Это означает, что можно связать в одну цепочку сколько 
угодно плагинов для обработки запросов и ответов.

Все это работает так:

Удаленный пользователь
       |
       |
       V
       plugin 1  (может изменить входящий запрос)
              |
              |
              V
              plugin 2  (может изменить входящий запрос)
                     |
                     |
                     V
                     ... (остальные плагины в цепочке)
                     |
                     |
                     V
              plugin 2  (может изменить ответ, полученный из цепочки выше)
              |
              |
              V
       plugin 1  (может изменить ответ, полученный из plugin 2)
       |
       |
       V
Удаленный пользователь

exb включает несколько плагинов, можете изучить их код, чтобы понять, как это 

Разработка собственного плагина
-------------------------------
exb автоматически оборачивает все плагины в поведение gen_server (а в будущем
планируется и автоматическое распределение нагрузки для плагинов). Разработчику
плагинов можно об том не думать, а просто реализовывать логику плагина.

Структура директории:
exb/
   |
   priv
      |
      plugins
            |
            PluginName1
            |         |
            |         config (опционально)
            |         exb_plugin_PluginName1.erl
            |
            PluginName2
                      |
                      config (опционально)
                      exb_plugin_PluginName2.erl
                      
Каждый плагин должен экспортировать функцию run/5, которая определена следующим
образом:
  run(Packet, Session, Args, PluginChain, PluginConfig)
  
По умолчанию Args = PluginConfig = [].

Packet — это фходящий XMPP-пакте, который может выглядеть таким образом:

	{xmlel,'jabber:client',[],message,
	    [{xmlattr,undefined,from,<<"user@jabber.orgg/undefined">>},
	     {xmlattr,undefined,to,<<"me@myserver.com/AdiumEC145AB1">>},
	     {xmlattr,undefined,type,<<"chat">>}],
	    [{xmlel,'http://jabber.org/protocol/chatstates',
	         [{'http://jabber.org/protocol/chatstates',none}],
	         active,[],[]},
	     {xmlel,'jabber:client',[],body,[],[{xmlcdata,<<"hello">>}]},
	     {xmlel,'http://jabber.org/protocol/xhtml-im',
	         [{'http://jabber.org/protocol/xhtml-im',none}],
	         html,[],
	         [{xmlel,'http://www.w3.org/1999/xhtml',
	              [{'http://www.w3.org/1999/xhtml',none}],
	              body,[],
	              [{xmlel,'http://www.w3.org/1999/xhtml',[],"span",
	                   [{xmlattr,undefined,"style",
	                        <<"font-family: Helvetica; font-size: medium; background: #ffffff;">>}],
	                   [{xmlcdata,<<"hello">>}]}]}]},
	     {xmlel,"google:nosave",
	         [{"google:nosave","nos"}],
	         x,
	         [{xmlattr,undefined,value,<<"disabled">>}],
	         []},
	     {xmlel,"http://jabber.org/protocol/archive",
	         [{"http://jabber.org/protocol/archive","arc"}],
	         "record",
	         [{xmlattr,undefined,otr,<<"false">>}],
	         []}]}

Больше информации по пакетам можно найти в документации по XMPP и exmpp.

Входящий пакет можно изменять, как угодно. После модификации этот пакет следует 
передать дальше в цепочку (PluginChain). Полностью эо выглядит так:

	run(Packet, Session, Args, PluginChain, PluginConfig) ->
	    NewPacket1 = опционально_меняем_входящий_пакет(Packet),
	    NewPacket2 = PluginChain(Packet),
	    NewPacket3 = опционально_меняем_ответ(NewPacket2),
	    NewPacket3. %% возвращаем ответ
	

*ПРИМЕЧАНИЕ*: Интерфейс плагинов может измениться (скорее всего изменится порядок 
аргументов, передаваемых в run/5).