#lang racket

#|(define chatbot
  (lambda( id mensaje . opciones)
    (list ' id mensaje opciones)))

(define sistema
  (lambda(nombre . chatbots)
    (list ' sistema nombre chatbots)))
|#
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

(define option
  (lambda(code message ChatbotCodeLink FlowCodeLink . Keyword)
    (list code message ChatbotCodeLink FlowCodeLink Keyword)))

(define flow
  (lambda(id name . option)
    (cond
      [(null? option) (list id name)]
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
      [(null? flows) (list chatbotid name welcomeMessage startFlowId)]
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
        (append new-flow flows)
        (if (boolean? (member (car new-flow ) (map flow-id (cdr flows))))
            flows
            (flow-dup  new-flow (cdr flows) (append aux  (car flows)))
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

(define op1 (option 1 "Viajar" 2 4 "viajar" "turistear" "conocer"))
(define op2 (option 2 "vuelo" 2 4 "viajar" "turistear" "conocer"))
(define op3 (option 3 "Taxi" 2 4 "viajar" "turistear" "conocer"))
(define op4 (option 5 "Taxi" 2 4 "viajar" "turistear" "conocer"))


(define f10 (flow 1 "Flujo1" op1 op2 op2 op1 op3))
(define f12 (flow 1 "Flujo2" op1 ))
(define f11 (flow-add-option f10 op4))
(define f13 (flow 2 "Flujo3" op3))
(define chatbot1 (chatbot 1 "chatbot1" "Hola" f10 f12))
(define cb10 (chatbot 0 "“Asistente”" "“Bienvenido\n¿Qué te gustaría hacer?”" 2))
(define cb11 (chatbot 0 "“Asistente”" "“Bienvenido\n¿Qué te gustaría hacer?”" 1 f12))

