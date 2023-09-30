#lang racket

(define id-option
  (lambda (option)
    (car option)))

(define message-option
  (lambda (option)
    (cadr option)))

(define ChatbotCodeLink-option
  (lambda (option)
    (caddr option)))

(define FlowCodeLink-option
  (lambda (option)
    (cadddr option)))

(define Keyword-option
  (lambda (option)
    (car(cddddr option))))
#|
Nombre de la función: option
Dominio: code x message x ChatbotLink x FlowCodeLink x Keyword
Recorrido: option
Descripción: Esta función permite crear una representación de una opcion, la cual recibe un codigo, mensaje , codigo del chatbot , un codigo del flow y cero
o más palabras clave|#
(define option
  (lambda(code message ChatbotCodeLink FlowCodeLink . Keyword)
    (list code message ChatbotCodeLink FlowCodeLink Keyword)))
#|
Nombre de la función: flow 
Dominio: id x name x option
Recorrido: flow
Descripción: Función la cual permite crear un flow que toma como parametros un id, su nombre y 0 o más opciones. Acá , se verifica
que no se entreguen opciones repetidas y se realiza dicha verificación en base a la id que posee la opcion llamando a una función recursiva de
tipo natural|#
(define flow
  (lambda(id name . option)
    (cond
      [(null? option) (list id name (list ))]
      [(= (length option) 1) (list id name option)]
      [else (list id name (option-dup(remove-duplicates option) (list )))])))

(define flow-id
  (lambda (flow)
    (car flow)))

(define flow-name
  (lambda (flow)
    (cadr flow)))

(define flow-option
  (lambda (flow)
    (caddr flow)))

(define option-dup
  (lambda (option aux)
    (if (eq? option null)
        aux
        (if (boolean?(member (caar option) (map id-option (cdr option))))
        (option-dup (cdr option) (append aux (list (car option))))
        (option-dup (cdr option) aux)))))

(define flow-add-option
  (lambda (flow option)
    (if (boolean?(member (id-option option) (map id-option (flow-option flow))))
        (list (flow-id flow)(flow-name flow)(append (flow-option flow) (list option)))
        flow)))

(define chatbot
  (lambda (chatbotid name welcomeMessage startFlowId . flows)
    (cond
      [(null? flows) (list chatbotid name welcomeMessage startFlowId (list))]
      [(= (length flows) 1) (list chatbotid name welcomeMessage startFlowId flows)]
      [else (list chatbotid name welcomeMessage startFlowId (flow-dup(remove-duplicates flows) (list )))])))

(define flow-dup
  (lambda (flows aux)
    (if (eq? flows null)
        aux
        (if (boolean? (member (caar flows ) (map flow-id (cdr flows))))
            (flow-dup (cdr flows) (append aux  (car flows)))
            (flow-dup (cdr flows) aux)
            )) ))

(define chatbot-add-flow
  (lambda (chatbot flow)
    (list (chatbot-chatbotid chatbot) (chatbot-name chatbot) (chatbot-welcomeMessage chatbot) (chatbot-startFlowId chatbot) (flow-dup-cb flow (chatbot-flows chatbot) (list )))))

(define flow-dup-cb
  (lambda (new-flow flows aux)
    (if (eq? flows null)
        (append aux (list new-flow))
        (if (boolean? (member (flow-id new-flow) (map flow-id flows)))
            (flow-dup-cb  new-flow (cdr flows) (append aux  (car flows)))
            flows
            )) ))
(define chatbot-chatbotid
  (lambda (chatbot)
    (car chatbot)))

(define chatbot-name
  (lambda (chatbot)
    (cadr chatbot)))

(define chatbot-welcomeMessage
  (lambda (chatbot)
    (caddr chatbot)))
(define chatbot-startFlowId
  (lambda (chatbot)
    (cadddr chatbot)))
(define chatbot-flows
  (lambda (chatbot)
    (car(cddddr chatbot))))

(define system
  (lambda (name InitialChatbotCodeLink . chatbot)
    (cond
      [(null? chatbot) (list name InitialChatbotCodeLink (list ) (list ) (list ) (list ))]
      [(= (length chatbot) 1) (list name InitialChatbotCodeLink (list ) (list ) (list ) chatbot)]
      [else (list name InitialChatbotCodeLink (list ) (list ) (list ) (chatbot-dup chatbot (list )))])))


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
(define user
  (lambda (name)
    name))
(define system-add-user
  (lambda (system user)
    (if (boolean? (member user (system-userList system)))
        (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(append (system-userList system)(list user))(system-loginList system)(system-chatbot system))
        (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(system-loginList system)(system-chatbot system)))))
(define system-login
  (lambda (system user)
    (if (= (length(system-loginList system)) 0)
        (if (boolean? (member user (system-userList system)))
            (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(system-loginList system)(system-chatbot system))
            (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(append (system-loginList system)(list user))(system-chatbot system)))
        (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(system-loginList system)(system-chatbot system)))))
(define system-logout
  (lambda (system)
    (if (= (length (system-loginList system)) 1)
        (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(list)(system-chatbot system))
         (list (system-name system)(system-InitialChatbotCodeLink system)(system-chatHistory system)(system-userList system)(system-loginList system)(system-chatbot system)))))
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

(define op1 (option 1 "Viajar" 2 4 "viajar" "turistear" "conocer"))
(define op2 (option 2 "vuelo" 2 4 "viajar" "turistear" "conocer"))
(define op3 (option 3 "Taxi" 2 4 "viajar" "turistear" "conocer"))
(define op4 (option 5 "Taxi" 2 4 "viajar" "turistear" "conocer"))

(define f9 (flow 1 "Flujox"))
(define f10 (flow 1 "Flujo1" op1 op2 op2 op1 op3))
(define f12 (flow 1 "Flujo2" op1 ))
(define f11 (flow-add-option f10 op4))
(define f13 (flow 2 "Flujo3" op3))
(define chatbot1 (chatbot 1 "chatbot1" "Hola" f10 f12))
(define cb10 (chatbot 0 "“Asistente”" "“Bienvenido\n¿Qué te gustaría hacer?”" 2))
(define cb11 (chatbot 1 "“Asistente”" "“Bienvenido\n¿Qué te gustaría hacer?”" 1 f12))
(define s0 (system "“NewSystem”" 0))
(define s1 (system" “NewSystem”" 0 cb11 cb10))
(define s2 (system-add-user s1 "“user0”"))
(define s3 (system-add-user s2 "“user1”"))
(define s4 (system-login s3 "“user1”"))
(define s5 (system-login s4 "“user0”"))
