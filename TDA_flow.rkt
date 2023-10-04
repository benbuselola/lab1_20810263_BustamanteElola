#lang racket
(provide (all-defined-out))
(require "TDA_option.rkt")
;CONSTRUCTOR
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