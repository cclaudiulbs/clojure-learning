;;;;;;;;;;;;;;
;; Clojure-Programming:: All about ns 
;;;;;;;;;;;;;;
;; ns are mappings of symbols to vars, or java classes.

;; the [in-ns] construct will SWITCH to the new ns and if not existing will create it as necessary
;; the [def] constructs will create vars within the current ns, which is ALWAYS bound to *ns* special value
;; the clojure.core is bound to the user's default ns, so switching to other ns using [in-ns] will not have loaded the
;; clojure.core library
(in-ns 'clojure-learning.clj-programming-all-about-ns)
(some #{:foo} [:foo]) ;; ->unable to resolve symbol "some"
(clojure.core/some #{:foo} [:foo :bar]) ;; :foo -> works

;; the [refer] construct will add the mappings from the refered ns into our current ns
;; assumming the other(from which we refer) ns is already loaded!
(ns some-other-ns)
(clojure.core/refer 'clojure-learning.clj-programming-all-about-ns)
(clojure.core/refer 'clojure.core)
(filter (partial < 0) [-1 1 2 3]) ;; (1 2 3)

;; we can now use the core ns and the one created just as if they were defined locally in our some-other-ns ns.

;; the other two actors are more smart: [require] + [use]
;; they actually handle the LOADING of those "required" modules INTO THE CURRENT ns, and establish aliases optionally,
;; and "use" is doing one more thing (that require doesnt), it automatically triggers "refer" enabling direct
;; reference from the current ns to other ns, whitout abs path refernce. hence [use] is built on top of [require] +
;; [refer]

(clojure.set/union #{1 2} #{3 4})
(require 'clojure.set)

;; require provides a way to establish an alias for a specific ns
(require '[clojure.string :as str])
(str/join ",") ;; ","

;; as seen there's no need in specifying the full path name of the clojure.string ns
;; the alias is enabled by placing the ns into a vector, else 
(require '(clojure 
              string 
              [set :as s]))

(s/intersection #{1 2} #{2 4}) ;; #{2}



