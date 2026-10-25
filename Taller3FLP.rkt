#lang eopl

;; ============================================================
;; Taller 3 - Fundamentos de Interpretación y Compilación / FLP
;; Integrantes:
;;   - Andrés Felipe Muñoz Moreno - 2438908
;;   - Juan Manuel Muñoz Moreno - 2436424
;;   - Juan Camilo Ramos Hoyos - 2438212
;;   - Emmanuel Paez Hurtado - 2419847
;; Repositorio: https://github.com/juancamilor1707/Taller3_FLP.git
   ;; DECLARACIÓN DE USO DE IA: se usó Claude (Anthropic) como tutor para
   ;; explicar conceptos (arbno, ambientes) y revisar errores de sintaxis.
   ;; El código fue escrito por los integrantes del grupo.
;; ============================================================

;; ============================================================
;; PUNTO 1 - Especificación léxica y gramática
;; ============================================================

;; Especificación léxica:
;; - espacios en blanco se ignoran
;; - texto: palabra que inicia con letra (o "_") seguida de letras, dígitos,
;;   "_" o ":". Se define sin comillas; las comillas se escapan en la gramática.
;;   (":" y "_" inicial son necesarios para los ejercicios 9e y 9f:
;;   "Hola:" y "_ProfesoresFLP")
;; - identificador: siempre inicia con @ (ej: @x, @z, @suma_1)
;; - numero: enteros y decimales, positivos y negativos
(define scanner-spec-simple-interpreter
  '((white-sp (whitespace) skip)
    (texto ((or letter "_") (arbno (or letter digit "_" ":"))) string)
    (identificador ("@" letter (arbno (or letter digit "_"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;; Gramática del lenguaje (notación infija para primitivas binarias).
;; Los miembros 2, 3 y 4 agregan sus producciones donde se indica.
(define grammar-simple-interpreter
  '((programa (expresion) un-programa)

    (expresion (numero) numero-lit)
    (expresion ("\"" texto "\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
      
    ;; --- PUNTO 4 (miembro 2): condicional-exp ---
    (expresion ("Si" expresion "{" expresion "}" "sino" "{" expresion "}") condicional-exp)
    (expresion ("declarar" "(" (arbno identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "{" expresion "}") procedimiento-ex)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")app-exp)
    ;; --- PUNTO 8 (miembro 4): declaración de procedimiento recursivo ---
    (expresion
     ("declarar-rec" identificador "(" (arbno identificador) ")" "=" "{" expresion "}" "en" "{" expresion "}")
     declararRec-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)

    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)))

;; Crea automáticamente los datatypes (programa, expresion, primitiva-binaria,
;; primitiva-unaria) a partir de la gramática.
(sllgen:make-define-datatypes scanner-spec-simple-interpreter
                              grammar-simple-interpreter)

;; Muestra los datatypes generados (útil para depurar la gramática).
(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-simple-interpreter
                                  grammar-simple-interpreter)))

;; Scanner + parser: recibe un string con un programa y devuelve su árbol
;; de sintaxis abstracta.
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter
                             grammar-simple-interpreter))

;; Scanner solo: devuelve la lista de tokens (útil para depurar el léxico).
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter
                              grammar-simple-interpreter))

;; ============================================================
;; PUNTO 2 - Ambientes
;; ============================================================

;; Datatype del ambiente: vacío, o extendido con una lista de ids, una lista
;; de valores y el ambiente anterior. Los miembros 3 y 4 pueden añadir aquí
;; la variante recursiva (punto 8).
(define-datatype ambiente ambiente?
  (vacio)
  (extendido (ids (list-of symbol?))
             (vals list?)
             (amb ambiente?))
  (extendido-rec (proc-id symbol?)
                 (ids (list-of symbol?))
                 (cuerpo expresion?)
                 (amb-anterior ambiente?)))

;; Ambiente inicial con @a..@e = 1 2 3 "hola" "FLP".
(define ambiente-inicial
  (extendido '(@a @b @c @d @e)
             '(1 2 3 "hola" "FLP")
             (vacio)))

;; Mensaje que se devuelve cuando una variable no está en el ambiente.
(define mensaje-variable-inexistente "Error, la variable no existe")

;; buscar-variable: recibe un símbolo (identificador) y un ambiente.
;; Retorna el valor asociado si lo encuentra; en caso contrario retorna el
;; string "Error, la variable no existe". Se usa en la evaluación de var-exp.
(define buscar-variable
  (lambda (id amb)
    (cases ambiente amb
      (vacio () mensaje-variable-inexistente)
      (extendido (ids vals amb-anterior)
                 (let ((pos (buscar-posicion id ids)))
                   (if pos
                       (list-ref vals pos)
                       (buscar-variable id amb-anterior))))
      (extendido-rec (proc-id ids cuerpo amb-anterior)
                     (if (eqv? id proc-id)
                         (cerradura ids cuerpo (extendido-rec proc-id ids cuerpo amb-anterior))
                         (buscar-variable id amb-anterior))))))

;; buscar-posicion: recibe un símbolo y una lista de símbolos. Retorna la
;; posición (desde 0) de la primera aparición o #f si no está. Es auxiliar
;; de buscar-variable.
(define buscar-posicion
  (lambda (id lista)
    (let loop ((l lista) (i 0))
      (cond ((null? l) #f)
            ((eqv? id (car l)) i)
            (else (loop (cdr l) (+ i 1)))))))

;; ============================================================
;; PUNTO 3 - Booleanos (0 es falso, cualquier otro valor es verdadero)
;; ============================================================

;; valor-verdad?: recibe un valor numérico y retorna #t si es verdadero
;; (distinto de 0) o #f si es falso (igual a 0). Se usa en neg y será usada
;; por el condicional (punto 4).
(define valor-verdad?
  (lambda (valor)
    (if (number? valor)
        (not (zero? valor))
        (eopl:error 'valor-verdad? "Se esperaba un número, se recibió: ~s" valor))))

;; bool->num: convierte un booleano de Racket a la representación del
;; lenguaje (1 verdadero, 0 falso). Las comparaciones lo usan para su resultado.
(define bool->num
  (lambda (b) (if b 1 0)))

;; ============================================================
;; Evaluación
;; ============================================================

;; evaluar-programa: recibe un programa (árbol de sintaxis abstracta) y lo
;; evalúa en el ambiente inicial.
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                   (evaluar-expresion body ambiente-inicial)))))

;; evaluar-expresion: recibe una expresión y un ambiente y retorna el valor
;; de la expresión. Es el núcleo del interpretador.
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable id amb))
      (primapp-bin-exp (exp1 prim exp2)
                       (let ((arg1 (evaluar-expresion exp1 amb))
                             (arg2 (evaluar-expresion exp2 amb)))
                         (aplicar-primitiva-binaria prim arg1 arg2)))
      (primapp-un-exp (prim exp1)
                      (let ((arg (evaluar-expresion exp1 amb)))
                        (aplicar-primitiva-unaria prim arg)))
      ;; --- PUNTOS 4 a 8: los demás miembros agregan sus casos aquí ---
      ;; condicional-exp: evalúa la condición; si es verdadera (distinta de 0)
      ;; evalúa la rama del Si, y si no, la rama del sino.
      (condicional-exp (test-exp true-exp false-exp)
                       (let ((valor-test (evaluar-expresion test-exp amb)))
                         (if (valor-verdad? valor-test)
                             (evaluar-expresion true-exp amb)
                             (evaluar-expresion false-exp amb)
                             )))
      ;; variableLocal-exp: evalúa cada expresión de exps en el ambiente actual,
      ;; extiende el ambiente con ids y esos valores, y evalúa cuerpo en ese
      ;; ambiente extendido.
      (variableLocal-exp (ids exps cuerpo)
                         (let ((vals (map (lambda (e) (evaluar-expresion e amb)) exps)))
                           (evaluar-expresion cuerpo (extendido ids vals amb))))

      (procedimiento-ex (ids cuerpo)
                  (cerradura ids cuerpo amb))

      (app-exp (rator rands)
               (let ((proc (evaluar-expresion rator amb))
                     (args (map (lambda (e) (evaluar-expresion e amb)) rands)))
                 (aplicar-procedimiento proc args)))
      (declararRec-exp (id ids cuerpo-proc cuerpo-principal)
                       (evaluar-expresion cuerpo-principal
                                          (extendido-rec id ids cuerpo-proc amb)))
      )))

;; verificar-numeros: recibe el nombre de la primitiva y dos valores; lanza
;; error si alguno no es número. Es auxiliar de las primitivas numéricas.
(define verificar-numeros
  (lambda (nombre a b)
    (if (and (number? a) (number? b))
        #t
        (eopl:error nombre "Se esperaban dos números, se recibió: ~s y ~s" a b))))

;; verificar-textos: igual que verificar-numeros pero para strings (concat).
(define verificar-textos
  (lambda (nombre a b)
    (if (and (string? a) (string? b))
        #t
        (eopl:error nombre "Se esperaban dos textos, se recibió: ~s y ~s" a b))))

;; dividir: divide a entre b. Lanza error si b es 0 y entrega un decimal
;; (en vez de una fracción exacta de Racket) cuando la división no es entera.
(define dividir
  (lambda (a b)
    (if (zero? b)
        (eopl:error 'primitiva-div "División por cero")
        (let ((r (/ a b)))
          (if (and (exact? r) (not (integer? r)))
              (exact->inexact r)
              r)))))

;; iguales?: compara dos valores; usa = para números (así 2 y 2.0 son
;; iguales) y equal? para el resto (textos).
(define iguales?
  (lambda (a b)
    (if (and (number? a) (number? b))
        (= a b)
        (equal? a b))))

;; aplicar-primitiva-binaria: recibe una primitiva binaria y sus dos
;; argumentos ya evaluados y retorna el resultado. Las comparaciones retornan
;; 1 (verdadero) o 0 (falso).
(define aplicar-primitiva-binaria
  (lambda (prim a b)
    (cases primitiva-binaria prim
      (primitiva-suma () (begin (verificar-numeros '+ a b) (+ a b)))
      (primitiva-resta () (begin (verificar-numeros '~ a b) (- a b)))
      (primitiva-div () (begin (verificar-numeros '/ a b) (dividir a b)))
      (primitiva-multi () (begin (verificar-numeros '* a b) (* a b)))
      (primitiva-concat () (begin (verificar-textos 'concat a b) (string-append a b)))
      (primitiva-mayor () (begin (verificar-numeros '> a b) (bool->num (> a b))))
      (primitiva-menor () (begin (verificar-numeros '< a b) (bool->num (< a b))))
      (primitiva-mayor-igual () (begin (verificar-numeros '>= a b) (bool->num (>= a b))))
      (primitiva-menor-igual () (begin (verificar-numeros '<= a b) (bool->num (<= a b))))
      (primitiva-diferente () (bool->num (not (iguales? a b))))
      (primitiva-comparador-igual () (bool->num (iguales? a b))))))

;; aplicar-primitiva-unaria: recibe una primitiva unaria y su argumento ya
;; evaluado y retorna el resultado.
(define aplicar-primitiva-unaria
  (lambda (prim a)
    (cases primitiva-unaria prim
      (primitiva-longitud ()
                          (if (string? a)
                              (string-length a)
                              (eopl:error 'longitud "Se esperaba un texto, se recibió: ~s" a)))
      (primitiva-add1 ()
                      (if (number? a)
                          (+ a 1)
                          (eopl:error 'add1 "Se esperaba un número, se recibió: ~s" a)))
      (primitiva-sub1 ()
                      (if (number? a)
                          (- a 1)
                          (eopl:error 'sub1 "Se esperaba un número, se recibió: ~s" a)))
      (primitiva-negacion-booleana ()
                                   (bool->num (not (valor-verdad? a)))))))

;; ============================================================
;; Interpretador (REPL) y utilidades de prueba
;; ============================================================

;; interpretador: ciclo leer-evaluar-imprimir con el prompt "-->".
;; Se ejecuta con (interpretador).
(define interpretador
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (evaluar-programa pgm))
                        (sllgen:make-stream-parser
                         scanner-spec-simple-interpreter
                         grammar-simple-interpreter)))

;; ejecutar: recibe un string con un programa, lo parsea y lo evalúa. Sirve
;; para probar sin usar el REPL, p. ej. (ejecutar "(@a + @b)").
(define ejecutar
  (lambda (texto-programa)
    (evaluar-programa (scan&parse texto-programa))))



;; ============================================================
;; PUNTO 6 y 7 - Creacion y Evaluacion de procedimientos
;; ============================================================

;; Datatype de los procedimientos: cerradura con una lista de parámetros, el
;; cuerpo sin evaluar y el ambiente donde se creó (alcance léxico)
(define-datatype procval procval?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)
   ))


;; aplicar-procedimiento: recibe una cerradura y los argumentos ya evaluados;
;; liga parámetros y argumentos sobre el ambiente de la cerradura y retorna
;; el valor de evaluar el cuerpo en ese ambiente. Es auxiliar de app-exp.
(define aplicar-procedimiento
  (lambda(proc args)
    (cases procval proc
      (cerradura (ids cuerpo amb)
                 (if (= (length ids) (length args))
                     (evaluar-expresion cuerpo (extendido ids args amb))
                     (eopl:error 'aplicar-procedimiento "Se esperaban ~s argumentos, se recibieron ~s" (length ids) (length args)))))))
  


;; -------------------------------
;; Pruebas de los puntos 1, 2 y 3 
;; -------------------------------
(ejecutar "@a")                       ; 1
(ejecutar "@b")                       ; 2
(ejecutar "@e")                       ; "FLP"
(ejecutar "@z")                       ; "Error, la variable no existe"
(ejecutar "(5 > 9)")                  ; 0
(ejecutar "(6 <= 6)")                 ; 1
(ejecutar "(4 ~ 5)")                  ; -1
(ejecutar "(4 ~ -5)")                 ; 9
;; -------------------------------
;; Pruebas del punto 4
;; -------------------------------
(ejecutar "Si (2+3) { 2 } sino { 3 }")                 ; 2
(ejecutar "Si (longitud(@d) ~ 4) { 2 } sino { 3 }")    ; 3

;; -------------------------------
;; Pruebas del punto 5
;; -------------------------------
(ejecutar "declarar (@x=2;@y=3;@a=7;){ (@a+(@x~@y)) }")   ; 6
(ejecutar "declarar (@x=2;@y=3;@a=7;){ (@a+@b) }")        ; 9


;; -------------------------------
;; Pruebas del punto 6
;; -------------------------------
(ejecutar "procedimiento (@x,@y,@z) { ((@x+@y)+@z) }")
(ejecutar "procedimiento (@x) { @x }")

;; -------------------------------
;; Pruebas del punto 7
;; -------------------------------

(ejecutar "declarar (@x=2;@y=3;@a=procedimiento (@x,@y,@z) { ((@x+@y)+@z) };) { evaluar @a (1,2,@x) finEval }")   ; 5
(ejecutar "declarar (@k=10;) { declarar (@f=procedimiento (@x) { (@x + @k) };) { declarar (@k=999;) { evaluar @f (1) finEval } } }")   ; 11

;; -------------------------------
;; Pruebas del punto 8
;; -------------------------------
;; sumaNaturales recursiva: suma 1+2+...+n
(ejecutar "declarar-rec @sumaNat (@n) = { Si @n { (@n + evaluar @sumaNat( (@n ~ 1) ) finEval) } sino { 0 } } en { evaluar @sumaNat (5) finEval }")   ; 15
(ejecutar "declarar-rec @sumaNat (@n) = { Si @n { (@n + evaluar @sumaNat( (@n ~ 1) ) finEval) } sino { 0 } } en { evaluar @sumaNat (10) finEval }")  ; 55
;; -------------------------------
;; Pruebas del 9a
;; -------------------------------
(ejecutar "declarar-rec @resto (@n) = {
  Si (@n < 10) { @n } sino { evaluar @resto ( (@n ~ 10) ) finEval }
} en {
  declarar-rec @sumarDigitos (@n) = {
    Si (@n < 10) { @n } sino { ( evaluar @resto (@n) finEval + evaluar @sumarDigitos ( ((@n ~ evaluar @resto (@n) finEval) / 10) ) finEval ) }
  } en {
    evaluar @sumarDigitos (147) finEval
  }
}")   ; 12

(ejecutar "declarar-rec @resto (@n) = {
  Si (@n < 10) { @n } sino { evaluar @resto ( (@n ~ 10) ) finEval }
} en {
  declarar-rec @sumarDigitos (@n) = {
    Si (@n < 10) { @n } sino { ( evaluar @resto (@n) finEval + evaluar @sumarDigitos ( ((@n ~ evaluar @resto (@n) finEval) / 10) ) finEval ) }
  } en {
    evaluar @sumarDigitos (1234) finEval
  }
}")   ; 10

;; -------------------------------
;; Pruebas del 9c
;; -------------------------------
(ejecutar "declarar-rec @multiplicar (@a @b) = {
  Si @b {
    (@a + evaluar @multiplicar(@a, (@b ~ 1)) finEval)
  } sino {
    0
  }
} en {
  evaluar @multiplicar(4, 3) finEval
}")   ; 12

(ejecutar "declarar-rec @multiplicar (@a @b) = {
  Si @b {
    (@a + evaluar @multiplicar(@a, (@b ~ 1)) finEval)
  } sino {
    0
  }
} en {
  evaluar @multiplicar(5, 3) finEval
}")   ; 15


;; -------------------------------
;; Pruebas del 9d
;; -------------------------------
(ejecutar "declarar-rec @esPar (@n) = {
  Si @n {
    Si (@n ~ 1) {
      evaluar @esPar((@n ~ 2)) finEval
    } sino {
      0
    }
  } sino {
    1
  }
} en {
  declarar-rec @sumaParesRango (@a @b) = {
    Si (@a > @b) {
      0
    } sino {
      Si evaluar @esPar(@a) finEval {
        (@a + evaluar @sumaParesRango(add1(@a), @b) finEval)
      } sino {
        evaluar @sumaParesRango(add1(@a), @b) finEval
      }
    }
  } en {
    evaluar @sumaParesRango(2, 7) finEval
  }
}")   ; 12

;; -------------------------------
;; Pruebas del 9e
;; -------------------------------
(ejecutar "declarar (@integrantes = procedimiento () {\"Robinson_y_Sara\"}; ) {
             declarar (@saludar = procedimiento(@f) {
                                    procedimiento () {(\"Hola:\" concat evaluar @f () finEval)}
                                  }; ) {
               declarar (@decorate = evaluar @saludar (@integrantes) finEval; ) {
                 evaluar @decorate () finEval}}}")   ; "Hola:Robinson_y_Sara"

;; -------------------------------
;; Pruebas del 9f
;; -------------------------------

(ejecutar "declarar (@integrantes = procedimiento () {\"Robinson_y_Sara\"}; ) {
             declarar (@saludar = procedimiento (@f) {
                                    procedimiento (@msg) {((\"Hola:\" concat evaluar @f () finEval) concat @msg)}
                                  }; ) {
               declarar (@decorate = evaluar @saludar (@integrantes) finEval; ) {
                 evaluar @decorate (\"_ProfesoresFLP\") finEval}}}")   ; "Hola:Robinson_y_Sara_ProfesoresFLP"

;; ==================================================================
;; PUNTO 9 - Programas en el lenguaje (cada miembro agrega los suyos)
;; ==================================================================
;; 9a) @sumarDigitos
;; declarar-rec @resto (@n) = {
;;   Si (@n < 10) { @n } sino { evaluar @resto ( (@n ~ 10) ) finEval }
;; } en {
;;   declarar-rec @sumarDigitos (@n) = {
;;     Si (@n < 10) { @n } sino { ( evaluar @resto (@n) finEval + evaluar @sumarDigitos ( ((@n ~ evaluar @resto (@n) finEval) / 10) ) finEval ) }
;;   } en {
;;     evaluar @sumarDigitos (147) finEval
;;   }
;; }
;; Resultados: @sumarDigitos(147) -> 12 ; @sumarDigitos(1234) -> 10

;; 9b)

;; 9c) @multiplicar
;; declarar-rec @multiplicar (@a @b) = {
;;   Si @b {
;;     (@a + evaluar @multiplicar(@a, (@b ~ 1)) finEval)
;;   } sino {
;;     0
;;   }
;; } en {
;;   evaluar @multiplicar(4, 3) finEval
;; }
;; Resultados: @multiplicar(4, 3) -> 12 ; @multiplicar(5, 3) -> 15

;; 9d) @sumaParesRango
;; declarar-rec @esPar (@n) = {
;;   Si @n {
;;     Si (@n ~ 1) {
;;       evaluar @esPar((@n ~ 2)) finEval
;;     } sino {
;;       0
;;     }
;;   } sino {
;;     1
;;   }
;; } en {
;;   declarar-rec @sumaParesRango (@a @b) = {
;;     Si (@a > @b) {
;;       0
;;     } sino {
;;       Si evaluar @esPar(@a) finEval {
;;         (@a + evaluar @sumaParesRango(add1(@a), @b) finEval)
;;       } sino {
;;         evaluar @sumaParesRango(add1(@a), @b) finEval
;;       }
;;     }
;;   } en {
;;     evaluar @sumaParesRango(2, 7) finEval
;;   }
;; }
;; Resultado: @sumaParesRango(2, 7) -> 12

;; 9e)@decorate
;; declarar (@integrantes = procedimiento () {"Robinson_y_Sara"}; ) {
;;                       declarar (@saludar = procedimiento(@f) {
;;                                                               procedimiento () {("Hola:" concat evaluar @f () finEval)}
;;                                          }; ){
;;                                          declarar (@decorate = evaluar @saludar (@integrantes) finEval; ) {
;;                                                              evaluar @decorate () finEval}}}


;; 9f)decorador @saludar con mensaje final
;; declarar (@integrantes = procedimiento () {"Robinson_y_Sara"}; ) {
;;   declarar (@saludar = procedimiento (@f) {
;;                           procedimiento (@x) {(("Hola:" concat evaluar @f () finEval) concat @msg)}
;;                         }; ) {
;;     declarar (@decorate = evaluar @saludar (@integrantes) finEval; ) {
;;       evaluar @decorate ("_ProfesoresFLP") finEval}}}