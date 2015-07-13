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

(use 'clojure.repl)
(doc slurp) ; opens a reader on the file-in-stream and read all the contents into a string

;; With macros, you can extend Clojure to suit your problem space, building up the language itself.

;; Briefly the clojure evaluation mechanism:
;;     Strings, numbers, characters, true, false, nil and keywords evaluate to themselves
;;     Clojure resolves symbols by:
;;         Looking up whether the symbol names a special form. If it doesn't...
;;         Trying to find a local binding. If it doesn't...
;;         Trying to find a mapping introduced by def. If it doesn't...
;;         Throwing an exception
;;     Lists result in calls:
;;         When performing a function call, each operand is fully evaluated and then passed to the function as an argument.
;;         Special form calls, like if and quote, follow "special" evaluation rules which implement core Clojure behavior
;;         Macros take unevaluated data structures as arguments and return a data structure which is then evaluated using the rules above
;; So, a macro is a tool for transforming an arbitrary data structure into one which can be evaluated by Clojure.
;; This allows you to introduce new syntax so that you can write code which is more concise and meaningful.

;; Taken the THREAD-FIRST macro:
;; [->] it allows for:
;;        + function pipelined
;;        + requires NO parantheses

(when true
  (println "executed 1")
  (println "executed 2"))
;; all expressions are executed since [when] is a macro that relies on [if] special form which
;; wraps the body of the conditions in a [do] special form.

;; our main task might be to write macros to implement a domain problem in a more concise manner. like
;; implementing a DSL which exposes a clear business communication.

;; remmeber that Clojure uses [list] s to represent function calls.
;; in the same context, macros can be seen as a mechanism for transforming an abstract data structure that
;; clojure CANNOT evaluate into one that it can.

;; we can use any function or macro or special form within the built macro, that is ->
;; we have the full power of Clojure at our disposal to extend Clojure.
;; macros are called just like any other function.

;; One key difference between functions and macros is that function arguments are fully evaluated
;; BEFORE they're PASSED to the FUNCTION, whereas MACROS receive arguments as UNEVALUATED data structures.
;; this is because since the passed in textual-representation is NOT yet a valid clojure data-structure
;; hence cannot be evaluated(this is the purpose of the macro in the end), but the output data-structure
;; will be evaluated since the macro responsibility is to output a valid clojure data-structure.

;; a short demo, to prove that output macro results ARE evaluated is:
(defmacro infix-notation
  [un-evaluated-struct]
  (list (second un-evaluated-struct) (first un-evaluated-struct) (last un-evaluated-struct)))

;; works nicely: notice that the in-args for macro are NOT evaluated
(infix-notation (1 + 2)) ; 3
(infix-notation (1 2 +)) ; cast exception!!! Long -> cannot be cast to Function!
;; this means the output of the macro IS evaluated!!!
;; notice that we output a list which is eagerly evaluated as the returned value for the macro

(macroexpand '(infix-notation (1 + 2))); (+ 1 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building Lists for Evaluation
;; Macro-writing is all about building a list to be evaluated by Clojure and it requires a kind of
;; inversion to your normal way of thinking. In particular, you'll need to be extra careful about
;; the difference between a SYMBOL and its VALUE.

(doc butlast) ; all but last
(conj (butlast '(1 2 3)) (last '(1 2 3))) ; (3 1 2)

;; Every symbol you want to be in the final output list of macro, should be prefixed with the
;; reader-macro [quote]

;; let's build the custom "unless" macro which is result of negation of [if] special form:
(defmacro unless
  [test-condition & forms]
  (list 'if test-condition nil (cons 'do forms) ))
(macroexpand '(unless false (str "higher") (str "than 10")))
;; (if false nil (do (str "higher") (str "than 10") ))
(unless false (str "higher") (str "than 10"))


(defmacro prefixed-notation
  [forms]
  (list (second forms) (first forms) (last forms)))
(macroexpand '(prefixed-notation (1 + 2)))
(prefixed-notation (1 + 2)) ; 3

;; almost always we'll use quoting in the process of building macros. this is for take un-evaluated
;; data-structures and process them as symbols
(+ 1 2) ; 3
(quote (+ 1 2)) ; (+ 1 2)
;; here + is NOT seen as a function, but rather as a symbol.

;; if a symbol not defined is evaluated -> an exception is fired, while if the symbol is
;; quoted, it will yield into itself.
;; the single quote ' is a shorthand for [quote]
(use 'clojure.repl)
(source quote)

;; the main target of quoting is to let the quoted forms be present in the final form which
;; the macro returns.
;; here's a simplified and mind-blowing of [unless]
(defmacro unless
  [test-form & tail-forms]
  (conj (reverse tail-forms) test-form 'if))

(unless false (str "higher") (str "than 10")) ; "higher"
;; decomposing: what happens here?
;; destructuring the args knowing the first condition to [unless] will be a predicate
;; what follows we reverse the list first, so that the last condition will be bounded to
;; quoted 'if and NOT the first one, and using [conj] push the test-form as the head-item
;; in the list reversed, and do the same for quoted 'if, which will be the left-most form.

;; Again, you have to quote if because you want the unevaluated symbol to be placed in the resulting list.

;; each first should evaluate to falsy in order for the second to execute, foreach pair
;; this is how we want to use it:
(while-not
   false (println "some: not false")
   false (println "none: not false")
   true (println "not exec-ed"))
;; output should be: "some: not false" "none: not false"

(defmacro while-not
  [& forms]
    (when forms
      (list 'when-not (first forms)
              (if (next forms)
                (second forms)
                (throw (IllegalArgumentException. "not a pair!")))
        (cons 'while-not (nnext forms)))))


;; inspect the result of macro
(macroexpand '(while-not false (str "some") false (str "none") true (str "not exec-ed")))
;; first thing evaluate if "forms" exist else -> when returns nil, this is for the recursion call(nil needed)
;; return the list having "when-not" macro as the first item in the returned list followed by the
;; first expression in the forms. when condition is met? then check for its pair via [if] if it
;; exists? then call it.
;; (when-not (first forms) (second forms) (recur call to while-not taking the rest of forms))


;;;;;;;;;;;;;;;;;
;; Syntax Quoting
;;;;;;;;;;;;;;;;;
;; Simple QUOTE helps for trivial examples, however it can introduce un-neccesary complexity
(defmacro foo [{:keys [foo bar]}]
  (list 'do (list 'str "some: " foo
            (list 'str ", or: " bar)
)))

(foo {:foo "foo" :bar "bar"}) ; "some: foo, or: bar"

;; the syntax quoting is like format "this is not evaluated, however this is #{name}"
;; it comes in the form of: [`] and introduces [~] for binding expression that must be evaluated!
(defmacro foo [{:keys [foo bar]}]
  `(do (str "some: " ~foo
       (str ", or: " ~bar)
)))

(foo {:foo "foo" :bar "bar"}) ; "some: foo, or: bar"

;; the difference is that [`] will prefix the symbol absolute namespace, while the quote not.
;; the other improvement is that it leads to more cleaner code, evaluating ONLY what needs to
;; be, via the tilda op [~].
;; the last difference is that using [syntax-quoting] the use of [list] function is NOT required
;; since the whole form is not-evaluated, but only prefixed ~foos
(macroexpand `(+ 1 ~(inc 1))) ; (clojure.core/+ 1 2)
(macroexpand (list '+ 1 (inc 1))) ; (+ 1 2)

;; Without syntax-quote, you need to quote the symbol [str].
;; This is because you want the resulting list to include the symbol [str],
;; not the function, which [str] evaluates to.

;; You're using syntax-quote because it lets you write things out more concisely
;; and you're unquoting[~foo] the bits that we want evaluated.

;; the macro always returns ITS LAST expression! and if there are some side-effects
;; functions they will NOT be executed!
;; take fo example:
(defmacro code-more [{:keys [foo bar]}]
  `(println "some" (quote ~foo))
  `(println "none" (quote ~bar)))

(code-more {:foo "foo" :bar "bar"}) ; none bar

;; that's why we introduced [do] which main purpose is to:
;; wrap up multiple expressions into one expression in situations like this.

(defmacro code-more [{:keys [foo bar]}]
  `(do (println "some" (quote ~foo))
       (println "none" (quote ~bar))))

(code-more {:foo "foo" :bar "bar"}) ; some foo + none/bar printed

;; To sum up: Macros receive unevaluated, arbitrary data structures as arguments
;; and return data structures that Clojure evaluates.
;; When defining your macro, you can use argument destructuring just like you can
;; with functions and let bindings.

;; Unquote Splicing: ~@
;; unquote splicing helps when there are scenarios like the one following:
(macroexpand `(+ (list 1 2 3))) ; (clojure.core/+ (clojure.core/list 1 2 3))

;; that is when we get another list of what should be evaluated by the first form-list.
(defmacro sum-all [& nums]
  `(+ ~nums))
(macroexpand '(sum-all 1 2 3)) ; (+ (1 2 3))
(sum-all 1 2 3) ;; throws exception class cast long to function! for any NON-EMPTY-LIST

;; now that's NOT what we wanted in the first.
;; because clojure evaluates any non-empty list taking 1st item as the function
;; and in this case the 1st item is a number! -> class cast exception!

;; the unquote-splicing mechanism was built for these special situations:
(defmacro sum-all [& nums]
  `(+ ~@nums))
(sum-all 1 2 3) ; 6
(macroexpand '(sum-all 1 2 3)) ; (clojure.core/+ 1 2 3)

;; hence the unquote-splicing on any annotated symbol will un-wrap that seqable data-structure
;; and will include those un-wrapped items in the enclosing list. or another interpretation:
;; I think of unquote splicing as unwrapping a seqable data structure,
;; placing its contents directly within the enclosing syntax-quoted data structure.

(macroexpand `(+ ~@(list 1 2 3))) ; (clojure.core/+ 1 2 3)

;; things to watch out when it comes to macros:
;; 1. variable capturing:
;; if there's already a defined binding in the current schema resolution, and
;; a macro attempts to "let" that bound variable to something else -> the
;; syntax quote would prevent this behavior however the simple quote would not:
;; simple quote with variable capturing:
(def message "some defined message")

(defmacro capture-demo
  [& forms]
  (concat (list 'let ['message "oh boy...from macro"])
    forms))

(capture-demo (println "going to print something: " message))
;; going to print something: oh boy...from macro

;; hence the variable capturing, macro defines a new binding for the 'message
;; hence the outer-scope-defined-symbol is shaddowed.

;;;;;;;;;;;;;;;;;;;;;
;; the syntax-quoting helps in this case, by at least throwing an exception:
(defmacro capture-demo
  [& forms]
  `(let [message "oh boy...from macro"] ;; the only reason is to shaddow message of let here
     forms))

(capture-demo (println "going to print something: " message))
;; exception: can't let qualified name: message!

;; to overcome this, the [gensym] comes to the rescue, creating an inner-unique name
;; for the message to include.
(gensym) ; G__10126
(gensym 'message) ; message10130

;; and here's the final version fixed:
(defmacro capture-demo
  [& forms]
  (let [macro-message (gensym 'message)]
    `(let [~macro-message "oh boy...from macro"]
       ~@forms
       (println (str "and we'll still going to print macro-message: " ~macro-message)))))

(capture-demo (println "going to print something: " message))
;; going to print something:  some defined message
;; and we'll still going to print macro-message: oh boy...from macro



(defmacro report
  [to-try]
  `(if ~to-try
     (println (quote ~to-try) "was successful:" ~to-try)
     (println (quote ~to-try) "was not successful:" ~to-try)))

;; Thread/sleep takes a number of milliseconds to sleep for
(report (do (Thread/sleep 1000) (+ 1 1)))
