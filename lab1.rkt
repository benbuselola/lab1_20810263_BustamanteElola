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


(define op1 (option 1 "Viajar" 2 4 "viajar" "turistear" "conocer"))
(define op2 (option 2 "vuelo" 2 4 "viajar" "turistear" "conocer"))
(define op3 (option 3 "Taxi" 2 4 "viajar" "turistear" "conocer"))
(define op4 (option 5 "Taxi" 2 4 "viajar" "turistear" "conocer"))
(define f10 (flow 1 "Flujo1" op1 op2 op2 op1 op3))
(define f11 (flow-add-option f10 op4))