Пока что это проверка концепций

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