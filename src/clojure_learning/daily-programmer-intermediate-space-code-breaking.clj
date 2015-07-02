;; 1. Problem:
;;;;;;;;;;;;;;
;; The year 2266 we have encountered alien planets who use very simple encryption to send messages.
;; Lucky for us we intercept all these messages and we can break the code.
;; The problem is the collection of messages are all from the same space probe.
;; So we are not sure which message is from what system.
;; Our challenge today is to decode the message and have our solutions determine which planet system the message came from.
;; Input will be a set of numbers each represent a byte in the message.

;;;;;;;;;;
;; Output:
;; The name of the system and the message decoded.

;;;;;;;;;;;;;;
;; Validation:
;; It is not enough to just take the message and decode it in all 4 ways and let you decide which one is right or wrong.
;; You need to have your program/solution determine the right decoding.
;; All messages are in english (I know even in the future on alien planets).

;; solution validation: in order to validate and decide WHICH planet to decode ->
;; pass the identifier-func which will decide the func to apply based on ASCI chars: 0 -> 127

(require '[clojure.string :as str])
(reduce char (str/split" 101 99 97 101 112 32 110 105 32 101 109 111 99 32 101 87 "))

(doc str/split) ; string pattern

(str/split " 101 99 97 101 112 32 110 105 32 101 109 111 99 32 101 87 " #"\s")
;; ["" "101" "99" "97" "101" "112" "32" "110" "105" " … "" ]
(str/trim " 101 99 97 101 112 32 110 105 32 101 109 111 99 32 101 87 ")
;; trims the leading+trailing whitespaces

;; Solution:
;; DEFINE a set of RULES for each planet!

;; func-implementation
(defn decode-alien-input
  [encoded-msg]
  (letfn [(convert-to-list [encoded-msg]
             (-> encoded-msg
                 (str/trim)
                 (str/split #"\s")))
          (map-to-nums [encoded-chars])
          (map-to-chars [encoded-chars]
           (map (comp char read-string) encoded-chars))
          (asci-decode-chars []
              (comp map-to-chars convert-to-list))
          (htrae-planet [decoded-char-seq]
             (apply str (reverse decoded-char-seq)))]
    (htrae-planet ((asci-decode-chars) encoded-msg))))

(decode-alien-input " 101 99 97 101 112 32 110 105 32 101 109 111 99 32 101 87 ")
; "We come in peace"


(map (comp char read-string)
     (-> " 101 99 97 101 112 32 110 105 32 101 109 111 99 32 101 87 "
         (str/trim)
         (str/split #"\s")))
;; (\e \c \a \e \p \space \n \i \space \e \m \o \c \space \e \W)

;; another option to parseInts or numbers is to use the java-interoperability
(. java.lang.Integer parseInt "100") ; 100
(read-string "100") ; 100
