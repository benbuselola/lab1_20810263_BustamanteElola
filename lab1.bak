#lang racket

#|(define chatbot
  (lambda( id mensaje . opciones)
    (list ' id mensaje opciones)))

(define sistema
  (lambda(nombre . chatbots)
    (list ' sistema nombre chatbots)))
|#
(define option
  (lambda(code message ChatbotCodeLink FlowCodeLink . Keyword)
    (list 'option code message ChatbotCodeLink FlowCodeLink Keyword)))

(define flow
  (lambda(id name . option)
    (list 'flow id name option)))

(define flow-add-option
  (lambda(flow option)
    (append option (flow))))

(define listaver
  (lambda (lista num)
    (if (boolean? (member num lista))
        "No existe el numero" "Existe el numero")))


