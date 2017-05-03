# Egg

## Разработка

Перед началом работы, необходимо необходимо подготовить систему - нам
понадобятся git и Erlang-19+:

```bash
    $ wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
    $ sudo dpkg -i erlang-solutions_1.0_all.deb
    $ sudo apt-get update
    $ sudo apt-get install erlang git
```

Теперь система готова к работе, клонируем egg:

```bash
    $ git clone https://github.com/kimoffegg/egg.git
```

Запускаем сервер:

```bash
    $ cd egg && rebar3 run
```

Для остановки сервера

```bash
    $ q().
```

## Использование

Деплой на продакшн производится следующим образом:
Собираем релиз:

```bash
    $ ./rebar3 as prod tar
```

В конце сборки получаем сообщение следующего вида:

```bash
    ===> tarball $PATH_TO_RELEASE.tar.gz successfully created!
```

Заливаем тарбол на сервер, например так:

```bash
    $ scp -i "$PATH_TO_KEY.pem" $PATH_TO_RELEASE.tar.gz user@remote_host:/home/user/egg
```

Заходим на удалённый сервер (свежая система, устанавливать ничего не нужно) разархивируем тарбол и запускаем его:

```bash
    $ cd egg && tar -xf egg-0.0.1.tar.gz
    $ ./bin/egg start
```

Чтобы проверить, что сервер работает, можно написать:

```bash
    $ ./bin/egg ping
```

должен вернуть "pong"

## Конфигурирование

## Хостинг статических файлов

В файле egg/apps/server/src/server.app.src в поле static_dir
указывается раположение статических файлов, доступ к которым будет
предоставлять эта машина по пути myhost.com:8080/static/*

## Регистрация

### version 1

В свежей системе нет пользователей, поэтому нужно создать первого администратора из консоли сервера командой:

```erlang
    %% new(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AccessLevel)
    users:new(8800000000, <<"my_password">>, <<"Joe">>, <<"Armstrong">>, 70, true, administrators, 0).
```

Таким же образом можно создавать и обычных пользователей, поменяв группу на users, а можно послать POST с JSON с полями msisdn, group, pwd_hash, fname, lname, age, is_male, access_level на адресс ../v1/register, либо зайдя по этому адрессу и заполнив форму. Регистрировать новых пользователей через api можно будет только с группой users (сейчас с любой группой и уровнем доступа). Пример создания того-же пользователя через API:

```bash
    $ curl -H "Content-Type: application/json" -X POST -d '{"msisdn":8800000000, "group":"administrators", "pwd_hash":"A865A7E0DDBF35FA6F6A232E0893BEA4", "fname":"Joe", "lname":"Armstrong", "age":70, "is_male":true, "access_level":0}' http://localhost:8080/v1/register
```

Коды ответа:

* 201: пользователь успешно создан
* 400: нет необходимых полей
* 403: нет прав на создание такого пользователя
* 409: если такой пользователь уже есть

### version 2

Версия протокола 2 отличается необходимостью ввода смс кода. Чтобы отправить смс, отправляем JSON POST на ../v1/sms с единственным полем msisdn с указанием номера получателя смс, пример отправки:

```bash
    $ curl -H "Content-Type: application/json" -X POST -d '{"msisdn": 8800000000}' http://localhost:8080/v1/sms
```

Новые коды ответа:

* 403: неверный смс код
* 428: смс-код для этого номера не отправлялся, либо просрочен

>       На данный момент сервер находится в режиме тестирования и не будет отправлять реальные смс, а будет использовать специальное апи для разработчиков смс-сервера, чтобы не тратить баланс на время отладки приложений. После отправки смс используйте код 6666, в режиме тестирования он всегда будет подходить.

## Сессии, авторизация

### version 1

Авторизация производится отправкой JSON по адрессу ../v1/auth с полями msisdn и password, где поле password хранит md5 хэш пароля, либо через форму, по этому-же адресу. В случае успешной авторизации вы получаете session_id, который и будет в дальнейшем давать вам доступ, ставя обращение к нему префиксом к запросу, требующему авторизации. Пример:

Неавторизованный доступ к модулю регистрации:

>       localhost:8080/v1/register

Авторизованный доступ к модулю регистрации:

>       localhost:8080/v1/session/$SESSION_ID/register

Пример получения токена:

```bash
    $ curl -H "Content-Type: application/json" -X POST -d '{"msisdn":8800000000, "password":"A865A7E0DDBF35FA6F6A232E0893BEA4"}' http://localhost:8080/v1/auth
```

Коды ответа:

* 200: авторизация прошла успешно
* 400: нет необходимых полей
* 401: неправильная пара логин\пароль

### version 2

Отличается лишь новым полем в запросе - code, куда вводится смс-код. Получение смс-кода описано в разделе регистрации второй версии, а так-же новыми кодами ответа сервера, так же описанными в разделе регистрации. Таким образом, получение токена будет выглядеть следующим образом:

```bash
    $ curl -H "Content-Type: application/json" -X POST -d '{"msisdn":8800000000, "password":"A865A7E0DDBF35FA6F6A232E0893BEA4", "code": 6666}' http://localhost:8080/v2/auth
```

## Аплоад и загрузка файлов

## WebSocket

Вебсокет как и REST оформлен модульно - строка запроса будет распарсена, опознанна сессия, модуль протокола, у модуля будут запрошены уровни и группы, имеющие доступ к нему, и если пользователь имеет достаточно прав - открыт вебсокет на одном из транспортов, обозначенных пользователем и сервером как поддерживаемые (сейчас это "json" и "msgpack", значение передаётся в хэдере 'sec-websocket-protocol' запроса открытия вебсокета). Формат адресной строки открытия вебсокета:

>       localhost:8080/ws/$VERSION/$PROTOCOL

Либо, для авторизованного доступа:

>       localhost:8080/session/$SESSION_ID/ws/$VERSION/$PROTOCOL

Коды ответа:

* 403: нет прав на открытие сокета с данным модулем\протоколом
* 404: обращение к несуществующему модулю\протоколу
* 501: в хэдере 'sec-websocket-protocol' указан неподдерживаемый транспорт

На данный момент существуют протоколы default (для тестов и отладки) и chat.

Устройство модуля-протокола:
Каждый модуль-протокол должен реализовать 6 функций:
* access_level - возвращает уровень доступа к модулю
* allowed_groups - возвращает список групп, имеющих доступ к модулю
* wrap_msg - преобразовывает ответ сервера в бинарный формат согласно оговоренному транспорту
* do_action - описывает действия, основанные на полученном от клиента сообщении и стэйте пользователя
* unwrap_msg - преобразовывает сообщение клиента из бинарного формата в типизированный внутренний формат сервера
* default_user_state - конструктор начального стэйта пользователя на основании данных о сессии

## ws\chat protocol v1

Это основной протокол данного форка, реализующий поведение чата. После открытия вебсокета пользователю доступны следующие команды (список может меняться по мере разработки):

Сообщения Клиент -> Сервер
----------------------

| Имя сообщения                 | msg_type      | Поля                                  | Возможные ответы              | Описание                                      |
--------------------------------|---------------|---------------------------------------|-------------------------------|-----------------------------------------------|
| c2s_chat_get_list             | 1             | --                                    | s2c_chat_list, s2c_error      | Получить список своих чатов                   |
| c2s_chat_get_info             | 2             | chat_id                               | s2c_chat_info, s2c_error      | Получить информацию о чате                    |
| c2s_chat_create               | 3             | name, users                           | s2c_chat_create_result        | Создать чат                                   |
| c2s_chat_leave                | 4             | chat_id                               | нет или s2c_error             | Покинуть чат                                  |
| c2s_chat_delete               | 5             | chat_id                               | нет или s2c_error             | Удалить чат                                   |
| c2s_chat_invite_user          | 6             | chat_id, user_msisdn                  | нет или s2c_error             | Пригласить в чат                              |
| c2s_chat_mute                 | 7             | chat_id                               | нет или s2c_error             | Пометить чат как приглушенный                 |
| c2s_chat_unmute               | 8             | chat_id                               | нет или s2c_error             | Убрать пометку с чата                         |
| c2s_chat_typing               | 9             | chat_id                               | нет                           | Сигнализировать, что ты печатаешь             |
| c2s_chat_accept_invatation    | 29            | chat_id                               | нет или s2c_error             | Принять приглашение в чат                     |
| c2s_chat_reject_invatation    | 30            | chat_id                               | нет или s2c_error             | Отклонить приглашение в чат                   |
| c2s_message_send              | 10            | chat_id, msg_body                     | нет или s2c_error             | Отправить сообщение                           |
| c2s_message_get_list          | 11            | chat_id, msg_id                       | s2c_message_list, s2c_error   | Получить все сообщения с msg_id больше данного|
| c2s_message_update            | 12            | chat_id, msg_id, msg_body             | нет или s2c_error             | Исправить сообщение                           |
| c2s_message_update_status     | 13            | chat_id, msg_id                       | нет или s2c_error             | Обновить статус (ожидает->получено->прочитано)|
| c2s_system_logout             | 14            | --                                    | нет                           | Удалить сессию, разорвать соединение          |
| c2s_user_get_info             | 15            | user_msisdn                           | s2c_user_info, s2c_error      | Получить информацию о пользователе            |
| c2s_user_get_status           | 16            | user_msisdn                           | s2c_user_status, s2c_error    | Онлайн ли пользователь?                       |
| c2s_user_set_info             | 17            | user_msisdn,fname,lname,age,is_male   | нет или s2c_error             | Указать информацию о себе                     |
| c2s_user_search               | 18            | fname, lname                          | s2c_user_search_result        | Поиск пользователя по имени и фамилии         |
| c2s_room_rename               | 19            | %TODO                                 | %TODO                         | %TODO                                         |
| c2s_room_add_user             | 20            |                                       |                               |                                               |
| c2s_room_del_user             | 21            |                                       |                               |                                               |
| c2s_room_add_subroom          | 22            |                                       |                               |                                               |
| c2s_room_create               | 23            |                                       |                               |                                               |
| c2s_room_delete               | 24            |                                       |                               |                                               |
| c2s_room_get_tree             | 27            |                                       |                               |                                               |
| c2s_room_get_info             | 28            |                                       |                               |                                               |
| c2s_room_enter_to_chat        | 25            |                                       |                               |                                               |
| c2s_room_send_message         | 26            |                                       |                               |                                               |

Сообщения Сервер -> Клиент
----------------------

| Имя сообщения                 | msg_type      | Поля                                                  | Описание                                      |
--------------------------------|---------------|-------------------------------------------------------|-----------------------------------------------|
| s2c_chat_list                 | 101           | chat_id                                               | Список чатов, в которых ты есть               |
| s2c_chat_info                 | 102           | chat_id,name,users,is_muted,chat_owner,access_group   | Информация о чате                             |
| s2c_chat_create_result        | 103           | chat_id                                               | Возвращает id созданного чата                 |
| s2c_error                     | 104           | code                                                  | Если что-то пошло не так, коды как в HTTP     |
| s2c_chat_invatation           | 105           | chat_id                                               | Тебя пригласили в чат                         |
| s2c_chat_typing               | 106           | chat_id, user_msisdn                                  | Кто-то начал печатать в одном из чатов        |
| s2c_message                   | 107           | chat_id, msg_body, timestamp, status, msg_id, from    | Сообщение в одном из чатов                    |
| s2c_message_update            | 108           | chat_id, msg_body, msg_id                             | Сообщение исправили                           |
| s2c_message_update_status     | 109           | chat_id, msg_id                                       | Сообщение получили\прочли                     |
| s2c_user_info                 | 110           | user_msisdn, fname, lname, age, is_male               | Информация о пользователе                     |
| s2c_user_status               | 111           | user_msisdn, is_online, last_visit_timestamp          | Онлайн ли пользователь                        |
| s2c_user_search_result        | 112           | users                                                 | Список телефоно найденных пользователей       |
| s2c_room_list                 | 113           | room_id                                               | %TODO                                         |
| s2c_room_tree                 | 114           | room_id                                               |                                               |
| s2c_room_info                 | 115           | room_id, subrooms, users, chats                       |                                               |
| s2c_room_create_result        | 116           | room_id                                               |                                               |
| s2c_message_list              | 117           | chat_id, messages                                     |                                               |

Наиболее актуальную информацию, а так же типы значений полей можно посмотреть в файле [объявления типов протокола](https://github.com/kimoffegg/egg/blob/egg_adaptation/apps/server/include/ws_chat_protocol_v1_messages.hrl) .


## Администрирование

## Бэкапы и восстановление
