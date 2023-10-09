#lang racket
(provide (all-defined-out))
(require "TDA_user_20810263k_BustamanteElola.rkt")
;CONSTRUCTOR

#|
Nombre de la función: chatHistory.
Dominio: user x historial.
Recorrido: user.
Descripción: Función la cual permite la representación de un user. Lo devuelve como una lista.|#
(define chatHistory
  (lambda(user historial)
    (list user historial)))

;SELECTORES
#|
Nombre de la función: chatHistory-user.
Dominio: chatHistory.
Recorrido: user del chatHistory. 
Descripción: Función la cual toma un chatHistory y entrega su user asociado.|#
(define chatHistory-user
  (lambda (chatHistory)
    (car chatHistory)))
#|
Nombre de la función: chatHistory-historial.
Dominio: chatHistory.
Recorrido: historial del chatHistory. 
Descripción: Función la cual toma un chatHistory y entrega su historial asociado.|#
(define chatHistory-historial
  (lambda (chatHistory)
    (cadr chatHistory)))