(ns clojure_learning.how-many-lines
  (:import [java.io.File])
  (:require [clojure.java.io :as io]))

(defn file? [file]
  (.isFile file))

(defn files-only [dir]
  (filter file?
    (file-seq dir)))

(defn lines-counter [file]
  (count
     (line-seq (io/reader file))))

(defn dir-lines-counter [dir]
  (reduce +
    (map lines-counter (files-only (File. dir)))))

;; demo:
(dir-lines-counter ".") ;; 21413

(doc file-seq)
(doc line-seq)
(files-only ".")
