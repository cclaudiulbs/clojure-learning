;; Build a macro which takes the reversed version of all the clojure operations yield a valid clojure data-structure
;; (reverse-ops nltnirp "this gets printed")
(require '(clojure [string :as str]
                   [walk :as walk]))
(use '(clojure [repl :as repl]))

(defmacro reverse-it [form]
  (walk/postwalk
     (fn [form-entry]
        (if (symbol? form-entry)
          (symbol (str/reverse (name form-entry)))
          form-entry))
     form))

(repl/doc walk/postwalk) ;; performs a depth-first, post-order traversal of form. calls func on each sub-form,
;; by using func's return values instead of the func itself.

(reverse-it (nltnirp "this gets printed")) ;; --> prints the entry

(reverse-it
   (od
      (fi (emos (nf [x] (= 12 x)) [12 13 14])
        (nltnirp "Found 12!!!")))) ;; prints: Found 12!!!

(macroexpand (reverse-it (nltnirp "this gets printed")))
(symbol? #(= 2 2)) ;; false

;; (symbol (str/reverse (name %)))
;; our macro should take the name of the symbol, apply str/reverse on it, and then convert back to a symbol.

;; macros allow to control the clojure compiler.
;; clojure source code is read by the clojure-reader -> which produces clojure data-structures from TEXTUAL
;; clojure code. this property where a language's code is represented using it's OWN data-structures is called
;; homoiconicity. Normally these data-structures are then evaluated.
;; in clojure: many literals evaluate to themselves(integers, strings, keywords), symbols evaluate to a value
;; in a var from some namespace, and lists denote CALLS either to: special forms, macros or functions.
;; MACROS ARE CALLED BY THE COMPILER WITH THEIR UN-EVALUATED DATA-STRUCTURES, AND MUST RETURN A CLOJURE
;; DATA-STRUCTURE THAT CAN BE EVALUATED!

;; if:
(defn foo [a b] (str a b))
(def a "this")
(def b "that")
(foo a b) ;; "thisthat"
;; but for macros -> the a + b are SYMBOLS passed to clojure compiler. the macro itself can choose
;; what to do with them -> either call them or use them; the condition is that the macro must return
;; a valid-clojure-data-structure.
(defmacro bar [a b] (list str a b))
(bar a b) ;; "thisthat"

;; the macro when compiled, the actual expanded code gets compiled and not the macro as we write.
;; The compilation process ensures that any macro calls are replaced
;; wholesale with their expansions long before a program’s runtime; thus,
;; macros are only ever evaluated at compile time.

;; macros from other languages are not so powerfull than clojure macros. C preprocessor macros work with
;; textual string manipulation and substitution, allowing the user to define some "macros" that gets substituted
;; in the code at a later time. same applies for JS eval, or other scafolding mechanism such as java's annotations.
;; while these mechanisms seem helpful they often lack the compilation step. while clojure macros are folded
;; into the same compilation process as for the rest of clojure code.

;; the power of macros in clojure: enhancing the language with the [for] loop
;; this is how we intend to use: (foreach [x [1 2 3]] (println x))
;;                                 macro   symbol coll body
(defmacro foreach
  [[bind-var coll] & body]
  `(loop [coll# ~coll]
     (when-let [[~bind-var & tail#] (seq coll#)] ;; re-bind the bind-var to each item in the collection: same identifier
        ~@body
        (recur tail#))))

(foreach [x [1 2 3]]
   (println x))

(foreach [x ["this" "that" "those"]]
    (println x))

;; start with the args-destructuring and the body of the for loop, which contains commands-like operations
;; start a loop binding the "evaluated" coll to "coll#" internal local. when-let is mandatory as it
;; starts a condition until NOT null, and also RE-BINDS every time in the recur-loop the "bind-var"
;; to point to each item from the coll#; then when this re-bind is done -> the user's "x" will be bound
;; to each item from coll -> hence the body unspliced can be evaluated. a recur is required to take the
;; the rest of the items within the collection: tail

;; another attempt:
(defmacro foreach [[each in-coll] & body]
  `(loop [coll# ~in-coll]
     (when-let [[~each & tail#] (seq coll#)]
       ~@body
       (recur tail#))))

(foreach [x (range 0 9)] (println x))

;; macros being a tool of abstraction(that can aggregate other macros), the code-print is exapnded on
;; macro invocation normally to a larget footprint. This mechanism for replacing macros with the
;; code that they produce is called macroexpansion.
;; -> macros are ONLY evaluated at compile-time.
;; functions are excelent tool of abstractions, but there are certain things functions can't do:
;; because they are ONLY called at runtime, and have NO access to the compiler.
;; make the distinction between the JS "eval" and the clojure macro. the "eval" is just a function
;; which is invoked at runtime, evaluating some piece of code, bypassing any moral rules,
;; it's like you're on your own there, while clojure macros have the compilation phase which catches
;; errors at compile-time(sooner).
;; apart from this, let's not forget that "eval" evaluates a String, while clojure a structured piece of code.
;; having only string-related operations -> regexps heavy used -> concatenation...and so on...
;; not extensible + not reusable at all.
;; clojure macros deal directly with clojure data-structures such as lists, vectors, maps, since clojure
;; is homoiconic.

;; macros in clojure work on data-structures that define the language itself.
;; macros can be notoriously difficult to debug.

;; if for instance we have an example such as:
(defn some-func [x] (tutankamon x)) ;; -> a runtime-exception is fired -> unable to resolve symbol in this context

;; doing the same thing but with a macro does not prompt with exception on macro-compilation
(defmacro some-macro [x] `(tutankamon ~x))

;; the macro is successfully compiled. the exceptional case comes when the macro is first use:
(some-macro 2) ;; -> compiler-exception: no such var: user/tutankamon, compiling...
;; what happend here: clojure macros work at compile-time -> clojure has no idea about the vars that might
;; exist at runtime, and hence make no assumption about that.
;; the macro sees and return ONLY lists, symbols and other valid clojure-data-structures.

;; luckily there are some debugging tools that make clojure-macros more appealing:
;; macroexpand-1 -> is the core function, which normally expands the macro ONLY ONE TIME(meaning
;; it does not go recursively and expand other macros, the macroexpansion level is restricted to 1 level)
;; other returned macros are NOT macroexpanded using the core [macroexpand-1] func
(macroexpand-1 '(some-macro 2)) ;; (user/tutankamon 2)

;; now we can check and see if we really have defined a var in the current symbol-schema resolution.

;; for macroexpanding all the macros until the top-level, one can use the core [macroexpand]
;; function which macro-expands ALL(and including the core) macros -> a lot of code -> difficult to debug
;; do to the ocean of information dumped. BUT [macroexpand] does NOT macro-expand nested macros:
;; cond -> throught macroexpansion -> results in if and else branch, but if else branch consists another [cond]
;; macro -> that macro is NOT expanded anymore, so NOT recursively.

;; since macros return valid clojure data-structures we will often require to return lists to denote
;; further function calls.
(defmacro hello [name]
  (list 'println name))
(hello "cc")

;; this approach comes quickly un-readable, therefore clojure provides some syntactic sugars such as:
;; ` do not eval
;; @ unsplice the list
;; ~ evaluate on demand


;; let's build a macro which emulates the imperative while loop, that actually runs some statefull commands
(defmacro while-do
  [predicate & body]
  `(when-let [evaled# ~predicate]
      ~@body
     (recur)))

(while-do (< 1 2)
  (println "1 :smaller 2")) ;; -> this will print forever!!!!

;; as known, simple quote returns the args un-evaluated. '() is the alias for: (quote some)
;; the backtick stands for syntax-quoting.
;; the difference between the two, is that syntax-quoting evaluates the symbols to current namespace resolution
;; using the fully-ns-path-name.
;; macros should NOT redefine values depending on the context/namespace they are used!
;; --> this is called macro-hygiene.
;; the second difference between simple-quoting and syntax-quoting, is that syntax-quoting allows for
;; manually evalute some expressions/symbols that resolve to the ns-schema-resolution in which they are defined.

;; clojure syntax-quoting allows to use lists and syntax-quote-only some elements that need to be evaluated.
;; but avoiding things such as: (list `map `println [foo]) -> using syntax-quoting, we can syntax-quote
;; all the list, leaving only the to-be-evaluated elements use: ~ evaluate it
(list `map `println ["cclaudiu"]) ;; (clojure.core/map clojure.core/println ["cclaudiu"])

;; note that unquoting will pass to evaluation the entire form where the "unquote" is prefixing.

;;;;;;;;;;;;;;;
;; there are times when we simply need the spliced version of the & tail args. however they will be
;; wrapped into a list. we need the content for that list. one idiomatic way might be to use
;; concat to concatenate the content of those into one list
;; a synonim for @ is "unpacking" -> it unpacks the content from a list

;; remember that macros only operate at compile-time -> hence they are not first-class-citizens of a
;; a running clojure-program like functions are. they don't know anything about runtime vars, and their
;; scope is to read clojure-un-evaluated data-structures -> and yield clojure evaluable-data-structures.

;; NOTE::
;; the more macros you need to solve the issue of their being unavailable at runtime, and
;; therefore not suitable for use in many functional programming idioms that call for
;; passing higher-order functions around.
;; First, a macro is convenient or powerful in one context (compilation), but can make life difficult in
;; another (runtime)

;; consider this:
(defn say-hi [name] (str name))
(defmacro say-hi-macro [name] `(str ~name))
(map say-hi ["claudiu" "cosar"])
(map say-hi-macro ["cclaudiu" "cosar"]) ;; --> COMPILER EXCEPTION: cannot take value of a macro at runtime!
;; hence macro are some actors which play an important role at compile time NOT at runtime, when CALLING FUNCTIONS.
;; the workaround for this is to wrap the macro in a anonymous function, or wrap the whole map in another macro
;; the last decision would be to induce more macros in the game...and will reach to what the above note says :)

;; macros are NOT recommended to be used, when a function can be used instead! They should ONLY be used with care
;; when we need a new language-construct, or we use some repetitive domain specific constructs!
;; for anything else -> use FUNCTIONs instead!

;; when writing macros it's important to note that, the macro vars DO NOT collide with other vars from
;; where the macro is used/compiled. this is called macro-hygiene. do achieve this clojure provides some
;; unique mechanism to generate unique identifiers: [gensym]
(gensym 'foo) ;; foo-every-time-a-diff-number

;; Gensyms in macros are common enough that there is a shorthand way of using them.
;; Any symbol ending in # inside a syntax-quote form will be expanded automatically into
;; a gensym, and will expand to the same gensym every time it appears. This is called an
;; auto-gensym.
(macroexpand-1 '(foreach [x [1 2]] (println x)))
;; --> the x -> will be bound internally to: coll__6856__auto__
;; each usage of the macro by using gensym or auto-gensym ensures that vars have unique IDs,
;; that do not clash with other namespace-define-vars on macro invocation.

;; remember that macros DONT evaluate their arguments!

;; however double evaluation is a behavior that macros have; when a macro relies and tries to use more than
;; one time an expression, or form that has side-effects, there will be un-expected behavior for those macros.
;; this principle is tight to macro-hygiene.
(defmacro double-eval-demo [s-expr]
  `(do
     (println (str "evaluating: " ~s-expr)
     (println (str "double-evaluation: " ~s-expr)))))

(double-eval-demo (rand-int 10))
;; 2 and 3 get printed -> therefore the s-expr is each time evaluated as a whole, therefore it should be
;; free of side-effects.

(macroexpand-1 '(double-eval-demo (rand-int 10)))
;; (do (clojure.core/println
;;       (clojure.core/str "evaluating: " (rand-int 10))
;;     (clojure.core/println
;;       (clojure.core/str "double-evaluation: " (rand-int 10)))))
;; this is the macro compiled and remember on compilation, macro changes into this expanded form.
;; there we see also the double invoked (rand-int) expressions, which normally yield 2 diff results each time.

;; the clojure idiom for solving this is to introduce a local binding, and use it anywhere it requires
(defmacro double-eval-demo-solved [sexpr]
  `(let [one-time# ~sexpr]
    (do
      (println one-time#)
      (println one-time#))))
(double-eval-demo-solved (rand-int 10)) ;; -> there! solved
(macroexpand-1 '(double-eval-demo-solved (rand-int 10)))
;; (let [one-time__6329__auto__ (rand-int 10)])

;; remember that is NOT a good pattern, to put complex logic inside of a macro! dont do this. macros should
;; behave as a thin layer on top of other macros, that one time can be replaceable by function application.

;; other neat info: the misterious zipmap function :)
(zipmap [:foo :bar] [1 2]) ;; {:bar 2, :foo 1}
;; mapcat is the result of applying [concat] on the result of applying [map fn colls].

(mapcat #(vec (% (inc %2))) [:foo :bar] [1 2])

(mapcat #(apply str (concat % %2)) ["some" "none"] ["foo" "bar"])

;; internally macros are imlemented as functions, that take 2 extra arguments: form + env, in addition to the
;; macro's signature arguments, when they are invoked by the clojure compiler.

