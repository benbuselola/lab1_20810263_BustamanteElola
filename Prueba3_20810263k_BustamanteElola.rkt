#lang racket
(require "TDA_option_20810263k_BustamanteElola.rkt")
(require "TDA_flow_20810263k_BustamanteElola.rkt")
(require "TDA_chatbot_20810263k_BustamanteElola.rkt")
(require "TDA_system_20810263k_BustamanteElola.rkt")
(require "TDA_user_20810263k_BustamanteElola.rkt")
(require "TDA_chatHistory_20810263k_BustamanteElola.rkt")



;Scipt de prueba N°3 creado por mi

;Se crean opciones
(define op1 (option 1 "1) Jugar juegos RPG" 1 1 "RPG" "JRPG" "ROL")) 
(define op2 (option 2 "2) Jugar juegos MMO" 2 1 "MMO" "Mundo abierto" "Clasicos" )) 
(define op3 (option 3 "3) Jugar juegos FPS" 1 1 "First Person Shooter" "Disparos" "Guerra"))
(define op4 (option 1 "1) Persona 5 Royale" 1 2 "JRPG" "Japones" "PC/XBOX/PS4")) ; se crea la opcion4
(define op5 (option 2 "2) Final Fantasy VI" 1 1 "Japones" "SNES")) 
(define op6 (option 3 "3) Volver" 0 1 "Salir" "Atras"))
(define op7 (option 1 "1) World of Warcraft" 2 1 "WoW")) ; se crea la opcion8
(define op8 (option 2 "2) Path of exile" 2 1 "PC" "Micropagos")) ; se crea la opcion9
(define op9 (option 3 "3) Volver" 0 1 "Salir" "Atras")) ; se crea la opcion10
;Se crea un flow y se le intentan agregar opciones
(define f1 (flow 1 "Flujo Principal Juegos\nBienvenido\n¿Qué tipo de juegos le gustaria jugar?" op1 op2 op3))
;Estas opciones no se deberian agregar debido a los mismos ids
(define f2 (flow-add-option f1 op1)) 
(define f3 (flow-add-option f1 op2)) 
(define f4 (flow-add-option f1 op3))
(define f5 (flow 2 "Flujo 1 Chatbot RPG\n¿Cuál te llama más la atención?" op4 op5 op6 op6))
(define f6 (flow 3 "Flujo 1 Chatbot MMO\n¿Qué juego te llama la atención?" op7 op8 op9)) ; se crea un flow en base a las opciones
;Se crea un chatbot a partir de lo anterior
(define cb0 (chatbot 0 "Chatbot Inicial" "Bienvenido\n¿Qué tipo de juegos le gustaria jugar el día de hoy?" 1 f3 f2 f2)) 
(define cb1 (chatbot 1 "Juegos RPG" "Bienvenido\n¿Cuál te llama más la atención?" 1 f3 f5)) 
(define cb2 (chatbot 2 "Juegos MMO" "Bienvenido\n¿Cuál te llama más la atención?" 1 f6))
; Se intentan añadir flujos que se repiten en el chabot 
(define cb3 (chatbot-add-flow cb1 f1)) 
(define cb4 (chatbot-add-flow cb3 f2)) 
(define cb5 (chatbot-add-flow cb0 f3))
;Se proceda a crear el sistema
(define s0 (system "Video-Juegos" 0 cb0 cb1 cb2 cb2))
; se intenta agregar un chatbot que se repite
(define s1 (system-add-chatbot s0 cb0)) 
(define s100 (system-add-chatbot s0 cb2))
(define s1001(system-add-chatbot s0 cb1)) 

(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2"user2")) 
(define s4 (system-add-user s3 "user1")) ;Ya está añadido, por lo tanto, no se ingresa nuevamente
(define s5 (system-login s4 "user1")) 
(define s6 (system-login s5 "user2")) ;No deja logear con otro usuario ya registrado 
(define s7 (system-logout s6)) ;Se deslogea un user
(define s8 (system-login s7 "user2")) ;Ahora deja el ingreso
(define s9 (system-logout s8)) ;Se deslogea un user
(define s10 (system-login s9 "user3")) ;Se logea un user no registrado que no existe, no deja logearlo
(define s11 (system-login s10 "user1")) ;Logea un user
;Interacciones con el usuario
(define s12 (system-talk-rec s11 "Chao")) 
(define s13 (system-talk-rec s12 "2"))
(define s14 (system-talk-rec s13 "1")) 
