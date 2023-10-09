#lang racket
(provide (all-defined-out))
;CONSTRUCTOR

#|
Nombre de la función: user.
Dominio: name.
Recorrido: user.
Descripción: Función la cual permite la representación de un user. Lo devuelve como una lista.|#
(define user
  (lambda(name)
    (list name)))

;SELECTORES
#|
Nombre de la función: user-name.
Dominio: user.
Recorrido: Nombre del user.
Descripción: Función la cual toma un user y entrega su nombre asociado.|#
(define user-name
  (lambda (user)
    (car user)))
