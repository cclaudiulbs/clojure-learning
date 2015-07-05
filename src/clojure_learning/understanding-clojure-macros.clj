;; The key to Clojure enlightenment is that Clojure evaluates data structures.
;; all the Clojure code you write that actually does anything consists of representations of lists!
(def addition-list (list + 1 2)) ; --> returns a list of: (+ 1 2) -> passed to eval
(eval addition-list) ; 3
(eval 'addition-list) ; (PLUS 1 2)

;; You enter reader forms, or textual representations of data structures.
;; In the example below, you're entering a form which consists of a list data structure
;; that contains three more forms: the str symbol and two strings.

(str "To understand what recursion is," " you must first understand recursion")
;; "To understand what recursion is, you must first understand recursion"

;; After you hit enter, Clojure reads the forms and internally produces the corresponding data structures.
;; It then evaluates the data structures. The textual representation of the result is then printed and you get:


;; Clojure code consists of textual representations of data structures called reader forms.
;; You'll notice that all your code that actually does stuff — function calls, if's, def's, etc — consists of list reader forms.
;; The reader transforms these reader forms into the actual internal data structures.

(read-string "(+ 1 2)")

(#(+ 1 %) 3)
; => 4

;; Well, try this out:

(read-string "#(+ 1 %)")
; => (fn* [p1__423#] (+ 1 p1__423#))

;; Whoa! What just happened?
;; This is not the one-to-one mapping that we're used to.
;; Reading #(+ 1 %) somehow resulted in a list consisting of the fn* symbol, a vector containing a symbol, and a list containing three elements.

;; To answer my own question: the reader used a reader macro to transform #(+ 1 %).
;; Reader macros are not to be confused with macros, which you'll read about later in this chapter.
;; Rather, reader macros are sets of rules for transforming text into data structures.

(read-string "#(+ 1 %)"); (fn* [p1__6236#] (+ 1 p1__6236#))

;; Reader macros often allow us to represent data structures in more compact ways.
;; The example below shows the quote reader macro, which expands to the quote special form:

(read-string "'(a b c)")
; => (quote (a b c))

;; When the reader encounters the single quote, ', it expands it to a list whose first member is the symbol quote
;; and whose second member is the data structure following the single quote.

;; The CLOJURE READER transforms text-representation into DATA-STRUCTURES.

;;;;;;;;;;;;
;; EvALUATOR:
;; Clojure Evaluator evaluates data-structures.
;; Clojure: Strings, numbers, false, true, nil and keywords ALL evaluate to themselvs.
(eval (read-string "\"t\"")) ; "t"
(eval "t") ; "t"

(eval (read-string "true")) ; true
(eval (read-string "false")) ; false
(eval true) ; true

;; Whenever Clojure evaluates these data structures, the result is the data structure itself.

;; Symbols in clojure are ALSO data-structures, else clojure wouldn't know how to evaluate them.
(let [x 4]
  (+ x 1)); 5

;; clojure evaluator resolves the "x" to the bound value of 4.

;; The "closest" binding takes precedence:
(let [x 10]
  (let [x 1]
    (+ 1 x))) ; 2

;; In general, Clojure resolves a symbol by:

;;     Looking up whether the symbol names a special form. If it doesn't…
;;     Trying to find a local binding. If it doesn't…
;;     Trying to find a mapping introduced by def. If it doesn't…
;;     Throwing an exception

;; Lists:: if the data-structure is an empty list -> evaluator evals to an empty list : else it evaluates to a CALL to
;; the first element in the list
(eval (read-string "()")) ; ()
(eval (read-string "(+)")) ; 0
(eval (read-string "(+ 1)")) ; 1
(eval (read-string "(1)")) ; --> throws exception: Long -> cannot be cast to IFunc

;; the last expression is like doing:
(1 2 3) -> throws exception -> long cannot be cast to ifunc
(eval ()) ; ()

(eval (read-string "(+ 1 2)")) ; 3
(eval (+ 1 2))
;; the reader reads the reader-form and PRODUCES a clojure-data-structure ->
;; which is then passsed to Evaluator which evaluates the expression
;; if the first element in the clojure data-structure is a function -> resolved by evaluator
;; then it is called with the arguments following the first-element.
;; When you code Clojure -> you code in fact pretty much data-structures
;; when an element is passed to a function, the Evaluator FIRST evaluates EACH element in the list
;; eagerly -> and then it passes the evaluated-result as the function argument.

;; Each element/arg of the function is eagerly evaluated BEFORE being passed to the function
;; this is NOT the case with clojure-special forms! which do NOT evaluate ALL arguments being special...
(eval (read-string "(if true (println \"evaluating first if true\") (println \"NOT evaluated for clojure-IF-special form\"))"))
;; here ONLY the first form is evaluated -> the second NOT being evaluated
;; this is NOT the same behavior for FUNCTIONS, which evaluate all arguments!!!


(eval (read-string "(if (< 1 2) (println \"evaluating first if true\") (println \"NOT evaluated for clojure-IF-special form\"))"))

;; the quote reader-macro always evaluates the ' to the quote expanded macro as the data-structure.
'(a b c); (a b c)
(read-string "'(a b c)") ; (quote (a b c))

;; what happens is:
;; in general, Clojure would try to resolve the [a] symbol and then call it because it's the first element of a list.
;; The [quote] special form TELLS the EVALUATOR:
;;  --> "instead of evaluating my next data structure like normal, just return the data structure itself."


;; AND MACROs!!!!
;; Clojure programs are comprised of data structures...
;; THE SAME data structures that Clojure is capable of manipulating...
;; wouldn't it be AWESOME if you could USE Clojure to MANIPULATE the data structures that comprise a Clojure program?
;; Yes, yes it would! And guess what: you can do this with macros! Did your head just explode? Mine did!

;; Note the following example is NOT a macro, however this is the behavior that macros implement
;; they allow you do manipulate clojure data-structures, write however you want the textual representation,
;; and then using the macro power they will re-structure the data-structures BEFORE being evaluated!

;; take this infix operation
(read-string "(1 + 2)") ; (1 + 2)
(let [infix (read-string "(1 + 2)")]
  (list (second infix) (first infix) (last infix))) ; (+ 1 2)

;; now this: (+ 1 2) can be easily evaluated by the Evaluator

;; Macros allow you to manipulate lists before Clojure evaluates them, except more conveniently.
;; They work very similarly to functions.
;; They take arguments and return a value, just like a function would.
;; Macro bodies behave exactly like function bodies.

;; The difference is all in the way functions and macros are evaluated.

;;
(defmacro ignore-last
  [form-func]
  (butlast form-func))

(ignore-last (+ 1 2 (println "this will not be printed!"))) ; 3
;; clearly this isnt a function call, since the function in any scenario could NOT manipulate
;; the function arguments.
;; First, when you call a function, each of its operands is evaluated before
;; being passed to the function as an argument.
;; By contrast, when you call a macro, the operands are NOT EVALUATED.
;; In particular, symbols are not resolved — they are passed as symbols.

;; take this example:
(ignore-last (+ 1 2 10)) ; 3
;; here the macro received the LIST un-evaluated, hence NOT the value 13 but the (+ 1 2 10)

;; one more important thing about macros: THE RETURNED EXPRESSION(may be a data-structure) of a FUNCTION is NOT Evaluated!!!
;; however the returned expression from a macro IS EvALUATED!!!
;; in order to inspect the data-structure the macro built, before passing it to the Evaluator, the
;; function [macroexpand] helps here. however the form must be quoted!
(macroexpand '(ignore-last (+ 1 2 10))) ; (+ 1 2)

;; the best way to understand where the macros come in picture, is to visualize an intermediary step between
;; the Reader and the Evaluator. This step can be called the: macroexpansion phase.
(when :in-doubt "some") ; "some"
(macroexpand '(when :in-doubt "some")) ; (if :in-doubt (do "some"))

;; taken a textual-representation:
;; (when :in-doubt "some")

;; -> goes to the Reader, which parses the textual-representation, and produces a clojure data-structure
;; (when :in-doubt "some")

;; -> the returned Reader form data-structure, is passed over to the MacroExpander, which
;; produces a new data-structure, that clojure's Evaluator can evaluate
;; (if :in-doubt (do "some"))

;; -> the returned macro-expanded data-structure is passed to the Evaluator safely to be evaluated.
;; producing a final value.

;; You can use Clojure to extend itself so that you write your program however you please. Macros thus enable syntax abstraction.

;; Taken the THRED-FIRST [->] macro:
;; This is a syntactical abstraction because it lets you write code in a syntax that's different
;; from Clojure's built-in syntax, but which is preferable for human consumption.

(require 'clojure.java.io)
(defn read-resource
  [path]
  (-> path
      clojure.java.io/resource
      slurp
      read-string))

(doc slurp) ; opens a reader on the file-in-stream and read all the contents into a string
