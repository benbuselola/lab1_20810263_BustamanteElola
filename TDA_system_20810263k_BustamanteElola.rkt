#lang racket
(provide (all-defined-out))
(require "TDA_option_20810263k_BustamanteElola.rkt")
(require "TDA_flow_20810263k_BustamanteElola.rkt")
(require "TDA_chatbot_20810263k_BustamanteElola.rkt")

;CONSTRUCTOR
#|
Nombre de la función: system.
Dominio: name x InitialChatbotCodeLink x chatbot.
Recorrido: system.
Tipo de recursión: Recursión de cola.
Descripción: Función la cual crea el system, que tiene como parametros de entrada el nombre, el InitialChatbotCodeLink y 0 o más chatbots. Con estos
argumentos se crea una lista representando un system.|#
(define system
  (lambda (name InitialChatbotCodeLink . chatbot)
    (define chatbot-dup
  (lambda (chatbot aux)
    (if (eq? chatbot null)
        aux
        (if (boolean?(member (chatbot-chatbotid (car chatbot)) (map chatbot-chatbotid (cdr chatbot))))
        (chatbot-dup (cdr chatbot) (append aux (list (car chatbot))))
        (chatbot-dup (cdr chatbot) aux)))))

    (cond
      [(null? chatbot) (list name InitialChatbotCodeLink (list ) (list ) (list ) (list ))]
      [(= (length chatbot) 1) (list name InitialChatbotCodeLink (list ) (list ) (list ) chatbot)]
      [else (list name InitialChatbotCodeLink (list ) (list ) (list ) (chatbot-dup chatbot (list )))])))
#|
Nombre de la función: chatbot-dup.
Dominio: chatbot x aux.
Recorrido: aux.
Tipo de recursión: Recursión de cola.
Descripción: Función auxiliar la cual, mediante la verificación del id del chatbot, revisa que no existan repeticiones en los chatbots mediante
el uso de un auxilar y con recursión de cola.|#
;SELECTORES
#|
Nombre de la función: system-name.
Dominio: system.
Recorrido: name del system.
Descripción: Función que toma un system y entrega su nombre asociado.|#
(define system-name
  (lambda (system)
  (car system)))
#|
Nombre de la función: system-InitialChatbotCodeLink.
Dominio: system.
Recorrido: InitialChatbotCodeLink del system.
Descripción: Función que toma un system y entrega su InitialChatbotCodeLink asociado.|#
(define system-InitialChatbotCodeLink
  (lambda (system)
    (cadr system)))
#|
Nombre de la función: system-chatHistory.
Dominio: system.
Recorrido: chatHistory del system.
Descripción: Función que toma un system y entrega su chatHistory asociado.|#
(define system-chatHistory
  (lambda (system)
    (caddr system)))
#|
Nombre de la función: system-userList.
Dominio: system.
Recorrido: userList del system.
Descripción: Función que toma un system y entrega su userList asociado.|#
(define system-userList
  (lambda (system)
    (cadddr system)))
#|
Nombre de la función: system-loginList.
Dominio: system.
Recorrido: loginList del system.
Descripción: Función que toma un system y entrega su loginList asociado.|#
(define system-loginList
  (lambda (system)
    (car(cddddr system))))
#|
Nombre de la función: system-chatbot.
Dominio: system.
Recorrido: chatbot del system.
Descripción: Función que toma un system y entrega los chatbots asociados.|#
(define system-chatbot
  (lambda (system)
    (car(cdr(cddddr system)))))

;MODIFICADORES

#|
Nombre de la función: system-add-chatbot.
Dominio: system x chatbot.
Recorrido: system.
Descripción: Funcion la cual agrega un chatbot al system. Para verificar la no repetición de chatbots,
se tiene que verificar por la id del chatbot a ingresar y ver que no se repita. Si se repite el id,
se retorna el system sin modificar. Caso contrario, se añade el chatbot a la lista de chatbots que posee el system.|#
(define system-add-chatbot
  (lambda (system chatbot)
    (if (boolean?(member (chatbot-chatbotid chatbot) (map chatbot-chatbotid (system-chatbot system))))
        (list (system-name system)(system-InitialChatbotCodeLink system)(append (system-chatbot system) (list chatbot)))
        system)))


#|
Nombre de la función: system-add-user.
Dominio: system x user.
Recorrido: system.
Descripción: Función la cual añade un usuario al sistema, es decir, ingresa los ingresa a la lista userList que posee el system.
Tambien, verifica que no se repitan los usuarios registrados.|#
(define system-add-user
  (lambda (system user)
    (if (boolean? (member user (system-userList system)))
        (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system) (append (system-userList system)(list user)) (system-loginList system)(system-chatbot system))
        system)))
#|
Nombre de la función: system-login.
Dominio: system x user.
Recorrido: system.
Descripción: Esta función realiza el login de un usuario al system. Este usuario tiene que estar previamente registrado, es decir,
se tiene que encontrar en el userList y no tiene que existir un usuario antes logeado. Si no, no se añade a la lista loginList|#
(define system-login
  (lambda (system user)
    (if (= (length(system-loginList system)) 0)
        (if (boolean? (member user (system-userList system)))
            system
            (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(append (system-loginList system)(list user))(system-chatbot system)))
        system)))
#|
Nombre de la función: system-logout.
Dominio: system.
Recorrido: system.
Descripción: Esta funcion realizar el deslogeo del usuario activo, es decir, deja la lista de login vacia. Se necesita que tenga un usuario
en la lista de logins, si no, se entrega el system.|#
(define system-logout
  (lambda (system)
    (if (= (length (system-loginList system)) 1)
        (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(list)(system-chatbot system))
        system)))

#|
Nombre de la función: system-talk-rec.
Dominio: system x message (string).
Recorrido: system.
Tipo de recursión: Recursión
Descripción: Funcion la cual permite interactuar con el chatbot. Tiene que existir un usuario registrado previamente y entrega el flujo entre
chatbots que recorre el usuario a partir del mensaje ingresado|#

(define system-talk-rec
  (lambda (system message)
    (if (null? (system-loginList system))
        system
        (list (system-name system)(system-InitialChatbotCodeLink system)(cons (buscar-chatbot (system-chatbot system) (system-InitialChatbotCodeLink system) )(system-chatHistory system))(system-userList system)(list)(system-chatbot system)))))


;FUNCIONES AUXILIARES
#|
Nombre de la función: buscar-flow
Dominio: chatbot x flow x code x message
Recorrido: flow
Descripción: Esta función busca un flow en un chatbot específico en base al code y el message proporcionados. 
Si encuentra el flujo, devuelve el flujo correspondiente, de lo contrario,lo sigue buscando.
|#
(define buscar-flow
  (lambda (chatbot flow code message)
    (if (null? flow)
        "hola"
        "a")))

#|
Nombre de la función: buscar-chatbot
Dominio: lista-de-chatbots x mensaje
Recorrido: chatbot
Tipo de recursión: Recursión natural
Descripción: Esta función busca un chatbot en una lista de chatbots basándose en un mensaje específico. 
Si encuentra el chatbot, devuelve el flujo inicial del chatbot.
|#
(define buscar-chatbot 
  (lambda (list-chatbot message)
  (if (equal? (caar list-chatbot) message)
      (list-ref(car list-chatbot) 4)
      (buscar-chatbot (cdr list-chatbot) message))))
