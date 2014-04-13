(ns info.josf.examples.zipper-seq
  (:require [clojure.zip :as zip]
            [clojure.data.xml :as dx]))


(defn recursion-for-recursion
  "Walking around doing nothing."
  [loc]
  (if (zip/end? loc)
    loc
    (recur (zip/next loc))))

;;; Stolen from http://ketnowledge.blogspot.fr/2013/02/clojure-zippers.html
(defn leafnodes 
  "Returns all leaf nodes in loc. "
  [loc]
  (filter (complement zip/branch?) ;filter only non-branch nodes
          (take-while (complement zip/end?) ;take until the :end
                      (iterate zip/next loc))))


(defn leafnodes-r
  "Recursive version of leafnodes."
  [loc accum]
  (if (zip/end? loc)
    accum
    (recur (zip/next loc) 
           (if (zip/branch? loc)
             accum
             (conj accum loc)))))


(def zzz (zip/vector-zip [:a [:b [:c :d] :e]]))

;; grab a webpage and parse it into a zipper
(def zappy
     (-> "http://richhickey.github.io/clojure/clojure.zip-api.html"
     	  slurp
	  xml/parse-str
	  zip/xml-zip))

(defn get-links [loc]
  (map #(get-in (zip/node %) [:attrs :href])
       (filter #(= :a (:tag (zip/node %))) 
               (take-while (complement zip/end?) 
                           (iterate zip/next loc)))))
