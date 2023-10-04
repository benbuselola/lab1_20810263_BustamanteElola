#lang racket
(provide (all-defined-out))
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