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

(defn zappy
  "grab a webpage we can trust, and parse it into a zipper"
  []
  (-> "http://richhickey.github.io/clojure/clojure.zip-api.html"
      clojure.java.io/input-stream
      dx/parse
      zip/xml-zip))

(defn get-links [loc]
  (map #(get-in (zip/node %) [:attrs :href])
       (filter #(= :a (:tag (zip/node %))) 
               (take-while (complement zip/end?) 
                           (iterate zip/next loc)))))

(defn get-a-zips
  "Return a lazy seq of :a locations"
  [loc]
  (filter #(= :a (:tag (zip/node %)))
          (take-while (complement zip/end?)
                      (iterate zip/next loc))))

(defn ancestor-count
  "How many ancestors do I have?"
  [loc]
  (count (take-while zip/up
                     (iterate zip/up loc))))

(defn descendant-count
  "How many descendants do I have?"
  [loc]
  (let [sub-zip (zip/xml-zip (zip/node loc))]
    (count (take-while (complement zip/end?)
                       (iterate zip/next (zip/down sub-zip))))))

(defn extract-url
  "Get the href value from an <a> zipper. Takes a loc."
  [loc]
  (get-in (zip/node loc) [:attrs :href]))


(defn depth-href-kidcount
  "loc-seq is a seq of <a> node zipper locations."
  [loc-seq]
  (map
   (fn [loc]
    {:depth (ancestor-count loc)
     :href (extract-url loc)
     :subnodes (descendant-count loc)})
   loc-seq))


