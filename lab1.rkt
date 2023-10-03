#lang racket
#|
Nombre de la función: id-option.
Dominio: option.
Recorrido: id del option.
Descripción: Funcion la cual toma la option y retorna la id asociada.|#
(define id-option
  (lambda (option)
    (car option)))
#|
Nombre de la función: message-option.
Dominio: option.
Recorrido: menssage del option.
Descripción: Esta función permite retornar el message asociado a la option ingresada.|#
(define message-option
  (lambda (option)
    (cadr option)))
#|
Nombre de la función: ChatbotCodeLink-option.
Dominio: option.
Recorrido: ChatbotCodeLink del option.
Descripción: La función permite devolver el ChatbotCodeLink de una option que se le ingrese.|#
(define ChatbotCodeLink-option
  (lambda (option)
    (caddr option)))
#|
Nombre de la función: FlowCodeLink-option.
Dominio: option.
Recorrido: FlowCodeLink del option.
Descripción: La función toma una option como argumento y devuelve el FlowCodeLink asociado a esa opción.|#
(define FlowCodeLink-option
  (lambda (option)
    (cadddr option)))
#|
Nombre de la función: Keyword-option.
Dominio: option.
Recorrido: Keyword del option.
Descripción: Esta funcion toma como arguemento una option y entrega las palabras clave o "Keywords" asociadas.|#
(define Keyword-option
  (lambda (option)
    (car(cddddr option))))
#|
Nombre de la función: option.
Dominio: code x message x ChatbotLink x FlowCodeLink x Keyword.
Recorrido: option.
Descripción: Esta función permite crear una representación de una opcion, la cual recibe un codigo, mensaje , codigo del chatbot , un codigo del flow y cero
o más palabras clave.|#
(define option
  (lambda(code message ChatbotCodeLink FlowCodeLink . Keyword)
    (list code message ChatbotCodeLink FlowCodeLink Keyword)))
#|
Nombre de la función: flow.
Dominio: id x name x option.
Recorrido: flow.
Descripción: Función la cual permite crear un flow que toma como parametros un id, su nombre y 0 o más opciones. Acá , se verifica
que no se entreguen opciones repetidas y se realiza dicha verificación en base a la id que posee la opcion llamando a una función recursiva de
tipo natural.|#
(define flow
  (lambda(id name . option)
    (cond
      [(null? option) (list id name (list ))]
      [(= (length option) 1) (list id name option)]
      [else (list id name (option-dup(remove-duplicates option) (list )))])))
#|
Nombre de la función: flow-id.
Dominio: flow.
Recorrido: id del flow.
Descripción: Funcion la cual , en base a la entrega de un flow, retorna su id.|#
(define flow-id
  (lambda (flow)
    (car flow)))
#|
Nombre de la función: flow-name
Dominio: flow
Recorrido: name del flow
Descripción: La funcion toma como argumento un flow y retorna el nombre de dicho parametro de entrada.|#
(define flow-name
  (lambda (flow)
    (cadr flow)))
#|
Nombre de la función: flow-option
Dominio: flow
Recorrido: option del flow
Descripción: Esta función toma un flow como argumento y retorna las options que poseea el flow.|#
(define flow-option
  (lambda (flow)
    (caddr flow)))
#|
Nombre de la función: option-dup
Dominio: option x aux. 
Recorrido: aux.
Tipo de recursión: Recursión de cola.
Descripción: Esta función auxiliar toma como valores de entrada las options que provienen de la función flow y un aux que nos va a servir para guardar
las opciones.El caso base de esta función, es cuando la lista de las opciones sea null y se va a devolver aux
Para poder verificar la no repetición, se realiza un map de la funcion id-option (la cual devuelve las id asociada a las opciones) y revisa
si la id de la funcion que se quiera agregar no sea igual. En el caso que sean iguales, se hace llamada recursiva con la lista que queda de opciones y
con aux intacto. Caso contrario, se hace la llamda recusiva con el resto de la lista de opciones y el auxiliar va a contener la opcion no repetida.|#

(define option-dup
  (lambda (option aux)
    (if (eq? option null)
        aux
        (if (boolean?(member (caar option) (map id-option (cdr option))))
        (option-dup (cdr option) (append aux (list (car option))))
        (option-dup (cdr option) aux)))))
#|
Nombre de la función: flow-add-option.
Dominio: flow x option.
Recorrido: flow.
Descripción: En esta función, que toma como entrada un flow y una option, añade la option de entrada al flow entregado. Al igual que en el rf3,
se tiene que verificar que las optionss no se repetian por el id con la diferencia que tiene que ser de forma no recursiva.|#
(define flow-add-option
  (lambda (flow option)
    (if (boolean?(member (id-option option) (map id-option (flow-option flow))))
        (list (flow-id flow)(flow-name flow)(append (flow-option flow) (list option)))
        flow)))
#|
Nombre de la función: chatbot.
Dominio: chatbotid x name x welcomeMessage x startFlowId x flows.
Recorrido: chatbot .
Descripción: Función la cual crea un chatbot (Representado como lista) con los valores de entrada definidos en el dominio. Acá,
se tiene que verificar que los flows que se añadan no se repitan en base a su id. Para lograr lo anterior, se llama a una función recursiva
que realize la acción de no duplicación.|#
(define chatbot
  (lambda (chatbotid name welcomeMessage startFlowId . flows)
    (cond
      [(null? flows) (list chatbotid name welcomeMessage startFlowId (list))]
      [(= (length flows) 1) (list chatbotid name welcomeMessage startFlowId flows)]
      [else (list chatbotid name welcomeMessage startFlowId (flow-dup(remove-duplicates flows) (list )))])))
#|
Nombre de la función: flow-dup
Dominio: flow x aux. 
Recorrido: aux.
Tipo de recursión: Recursión de cola.
Descripción: En esta función, se toma como parametros de entrada flows y un aux que va a servir para emplear la recursión de cola.
Se tiene como caso base, que los flows sea una lista vacía y retorne aux. Para verficiar la no repetición de flows, se realiza
una consulta que ve si existen ids iguales entre el primer elemento de flows y los demas. Para el caso que no se repita, se realiza un
append agreando el valor a aux y se hace la llamada con el resto de la lista de flows y el aux modificado. Caso contrario, llamada recursiva
con el resto de la lista de flow y el aux sin modificar. |#
(define flow-dup
  (lambda (flows aux)
    (if (eq? flows null)
        aux
        (if (boolean? (member (caar flows ) (map flow-id (cdr flows))))
            (flow-dup (cdr flows) (append aux  (car flows)))
            (flow-dup (cdr flows) aux)
            )) ))
#|
Nombre de la función: chatbot-add-flow .
Dominio: chatbot x flows.
Recorrido: chatbot.
Descripción: La función tiene como finalidad agregar un flow al chatbot. Al igual que las funciones anteriores relacionadas con el chatbot,
se tiene que ver la no repetición de los flows en base al id y se hace uso de una función externa para dicha tarea|#
(define chatbot-add-flow
  (lambda (chatbot flow)
    (list (chatbot-chatbotid chatbot) (chatbot-name chatbot) (chatbot-welcomeMessage chatbot) (chatbot-startFlowId chatbot) (flow-dup-cb flow (chatbot-flows chatbot) (list )))))
#|Nombre de la función: flow-dup-cb.
Dominio: new-flow x flow x aux. 
Recorrido: aux.
Tipo de recursión: Recursión de cola.
Descripción:. Esta función verifica que el new-flow que se desea ingresar a la lista original de flows asociada al chatbot, no tenga una id
similar. El caso base es cuando la lista de flows es null y retorna el aux con el new-flow agregado. Para poder verificar la no repetición, hay que ver que la id del
new-flow no se encuentre repetida en la lista de flows. Si se encuentra repetida, se devuelve la lista de flows. Caso contrario, se
llama a la función con el new-flow, el resto de la lista de flows y el aux con el primer flow de la lista proveniente del chatbot.|#
(define flow-dup-cb
  (lambda (new-flow flows aux)
    (if (eq? flows null)
        (append aux (list new-flow))
        (if (boolean? (member (flow-id new-flow) (map flow-id flows)))
            (flow-dup-cb  new-flow (cdr flows) (append aux  (car flows)))
            flows
            )) ))
#|
Nombre de la función: chatbot-chatbotid.
Dominio: chatbot.
Recorrido: id del chatbot.
Descripción: Funcion la cual toma el chatbot y retorna la id asociada.|#
(define chatbot-chatbotid
  (lambda (chatbot)
    (car chatbot)))
#|
Nombre de la función: chatbot-name.
Dominio: chatbot.
Recorrido: name del chatbot.
Descripción: La funcion toma un chatbot y entrega el nombre asociado.|#
(define chatbot-name
  (lambda (chatbot)
    (cadr chatbot)))
#|
Nombre de la función: chatbot-welcomeMessage.
Dominio: chatbot.
Recorrido: message del chatbot.
Descripción: Funcion que recive un chatbot y entrega su mensaje de bienvenida.|#
(define chatbot-welcomeMessage
  (lambda (chatbot)
    (caddr chatbot)))
#|
Nombre de la función: chatbot-startFlowId.
Dominio: chatbot.
Recorrido: startFlowId del chatbot.
Descripción: Esta función toma un chatbot y entrega el startFlowId asociado al chatbot que fue ingresado.|#
(define chatbot-startFlowId
  (lambda (chatbot)
    (cadddr chatbot)))
#|
Nombre de la función: chatbot-flows.
Dominio: chatbot.
Recorrido: flows asociados al chatbot.
Descripción: La función toma el chatbot y entrega la lista de flows asociada.|#
(define chatbot-flows
  (lambda (chatbot)
    (car(cddddr chatbot))))
#|
Nombre de la función: chatbot-flows.
Dominio: chatbot.
Recorrido: flows asociados al chatbot.
Descripción: La función toma el chatbot y entrega la lista de flows asociada.|#
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
