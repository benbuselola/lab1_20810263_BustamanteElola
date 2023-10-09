#lang racket
(provide (all-defined-out))
(require "TDA_option.rkt")
(require "TDA_flow.rkt")

;CONSTRUCTOR

#|
Nombre de la función: chatbot.
Dominio: chatbotid x name x welcomeMessage x startFlowId x flows.
Recorrido: chatbot .
Tipo de recursión: Recursión de cola.
Descripción: Función la cual crea un chatbot (Representado como lista) con los valores de entrada definidos en el dominio. Acá,
se tiene que verificar que los flows que se añadan no se repitan en base a su id. Para lograr lo anterior, se llama a una función recursiva
que realize la acción de no duplicación.|#
(define chatbot
  (lambda (chatbotid name welcomeMessage startFlowId . flows)
    (define flow-dup
      (lambda (flows aux)
        (if (eq? flows null)
            aux
            (if (boolean? (member (caar flows ) (map flow-id (cdr flows))))
                (flow-dup (cdr flows) (append aux  (car flows)))
                (flow-dup (cdr flows) aux)
                )) ))
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

;SELECTORES

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

;MODIFICADORES

#|
Nombre de la función: chatbot-add-flow .
Dominio: chatbot x flows.
Recorrido: chatbot.
Tipo de recursión: Recursión de cola.
Descripción: La función tiene como finalidad agregar un flow al chatbot. Al igual que las funciones anteriores relacionadas con el chatbot,
se tiene que ver la no repetición de los flows en base al id y se hace uso de una función externa para dicha tarea|#

(define chatbot-add-flow
  (lambda (chatbot flow)
  (define flow-dup-cb
    (lambda (new-flow flows aux)
      (if (eq? flows null)
          (append aux (list new-flow))
          (if (boolean? (member (flow-id new-flow) (map flow-id flows)))
              (flow-dup-cb  new-flow (cdr flows) (append aux  (car flows)))
              flows
              )) ))
     (lambda (chatbot flow)
    (list (chatbot-chatbotid chatbot) (chatbot-name chatbot) (chatbot-welcomeMessage chatbot) (chatbot-startFlowId chatbot) (flow-dup-cb flow (chatbot-flows chatbot) (list ))))))
#|Nombre de la función: flow-dup-cb.
Dominio: new-flow x flow x aux. 
Recorrido: aux.
Tipo de recursión: Recursión de cola.
Descripción:. Esta función verifica que el new-flow que se desea ingresar a la lista original de flows asociada al chatbot, no tenga una id
similar. El caso base es cuando la lista de flows es null y retorna el aux con el new-flow agregado. Para poder verificar la no repetición, hay que ver que la id del
new-flow no se encuentre repetida en la lista de flows. Si se encuentra repetida, se devuelve la lista de flows. Caso contrario, se
llama a la función con el new-flow, el resto de la lista de flows y el aux con el primer flow de la lista proveniente del chatbot.|#
