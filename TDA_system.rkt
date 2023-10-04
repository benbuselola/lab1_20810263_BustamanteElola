#lang racket
(provide (all-defined-out))
(require "TDA_option.rkt")
(require "TDA_flow.rkt")
(require "TDA_chatbot.rkt")

#|
Nombre de la función: system.
Dominio: name x InitialChatbotCodeLink x chatbot.
Recorrido: system.
Descripción: Función la cual crea el system, que tiene como parametros de entrada el nombre, el InitialChatbotCodeLink y 0 o más chatbots. Con estos
argumentos se crea una lista representando un system.|#
(define system
  (lambda (name InitialChatbotCodeLink . chatbot)
    (cond
      [(null? chatbot) (list name InitialChatbotCodeLink (list ) (list ) (list ) (list ))]
      [(= (length chatbot) 1) (list name InitialChatbotCodeLink (list ) (list ) (list ) chatbot)]
      [else (list name InitialChatbotCodeLink (list ) (list ) (list ) (chatbot-dup chatbot (list )))])))

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


(define chatbot-dup
  (lambda (chatbot aux)
    (if (eq? chatbot null)
        aux
        (if (boolean?(member (chatbot-chatbotid (car chatbot)) (map chatbot-chatbotid (cdr chatbot))))
        (chatbot-dup (cdr chatbot) (append aux (list (car chatbot))))
        (chatbot-dup (cdr chatbot) aux)))))

#|
Nombre de la función: system-add-user.
Dominio: system x user.
Recorrido: system.
Descripción: |#
(define system-add-user
  (lambda (system user)
    (if (boolean? (member user (system-userList system)))
        (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system) (append (system-userList system)(list user)) (system-loginList system)(system-chatbot system))
        system)))

(define system-login
  (lambda (system user)
    (if (= (length(system-loginList system)) 0)
        (if (boolean? (member user (system-userList system)))
            system
            (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(append (system-loginList system)(list user))(system-chatbot system)))
        system)))

(define system-logout
  (lambda (system)
    (if (= (length (system-loginList system)) 1)
        (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(list)(system-chatbot system))
        system)))

(define system-name
  (lambda (system)
  (car system)))
(define system-InitialChatbotCodeLink
  (lambda (system)
    (cadr system)))

(define system-chatHistory
  (lambda (system)
    (caddr system)))

(define system-userList
  (lambda (system)
    (cadddr system)))
(define system-loginList
  (lambda (system)
    (car(cddddr system))))
(define system-chatbot
  (lambda (system)
    (car(cdr(cddddr system)))))