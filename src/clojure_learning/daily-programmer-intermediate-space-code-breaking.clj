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

;; Solution:
;; DEFINE a set of RULES for each planet!

;; func-implementation
(require '[clojure.string :as str])
(defn decode-alien-input
  [encoded-msg]
  (letfn [(format-encoded [encoded-msg]
             (-> encoded-msg
                 (str/trim)
                 (str/split #"\s")))

          (decode-to-asci [encoded-msg]
             (map read-string (format-encoded encoded-msg)))

          ;; general shared func
          (decode-alien-msg [encoded-msg planet-specific-decode]
              (->> (decode-to-asci encoded-msg)
                   planet-specific-decode
                   (filter #(< % 128)) ;; prevent exception for higher-nums
                   (map char)
                   (apply str)))

          ;; building the map: {func-name, alien-msg}
          (extract-func-name [func]
              (->>
                 (-> (.toString func)
                     (str/split #"@")
                     (first)
                     (str/split #"\$")
                     (last))
                 (re-seq #"[a-zA-Z]")
                 (apply str)))

          (apply-planet-funcs [encoded-msg & planet-funcs]
              (reduce (fn [m planet-fn]
                          (assoc m (extract-func-name planet-fn) (decode-alien-msg encoded-msg planet-fn)))
                      {} planet-funcs))

          (english-word? [decoded-msg]
              (if (empty? decoded-msg)
                  false
                  (let [non-words (re-seq #"[^a-zA-Z\s\.\,]" decoded-msg)]
                    (empty? non-words))))

          ;; [seq] builds a pair of each map-entry: ([key val], [key val])
          (english-msg? [planet-msg-vec]
               (english-word? (last planet-msg-vec)))

          ;; specific funcs for each planet
          (htrae-planet [asci-codes] (reverse asci-codes))
          (hoth-planet [asci-codes] (map (partial + 10) asci-codes))
          (ryza-planet [asci-codes] (map dec asci-codes))
          (omicron-planet [asci-codes] (map #(bit-flip 5 %) asci-codes))]

    (filter english-msg? (seq (apply-planet-funcs encoded-msg  htrae-planet hoth-planet ryza-planet omicron-planet)))
))

;; demo::
(decode-alien-input " 101 99 97 101 112 32 110 105 32 101 109 111 99 32 101 87 ")


;; exercise the solution::
(defn english? [decoded-msg]
              (let [non-words (re-seq #"[^a-zA-Z\s\.\,]" decoded-msg)]
                (empty? non-words)))

(filter english? ["hothplanet" "omkoz*xs*owym*oa"])

(doc bit-flip); flips a bit at index n: bit-flip x n

;; another option to parseInts or numbers is to use the java-interoperability
(. java.lang.Integer parseInt "100") ; 100
(read-string "100") ; 100

(re-seq #"[^a-zA-Z\s\.\,]" "adsds# asasa@@") ; ("#" "@" "@")
(empty? nil) ; true

(doc assoc)

(-> (.toString assoc)
    (str/split #"@")
    (first)
    (str/split #"\$")
    (last)) ; "assoc"

(empty? (re-seq #"[^a-zA-Z\s\.\,]" "")) ; true
