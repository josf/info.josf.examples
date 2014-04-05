(ns info.josf.examples.zipper-mind
  (:require [clojure.zip :as zip]))

(def zzz (zip/vector-zip [1 [:a :b] 2 3 [40 50 60]]))

(defn death-to-big-numbers
  [loc]
  (if (zip/end? loc)
    loc
    (if (or (not (integer? (zip/node loc)))
            (< (zip/node loc) 11))
      (recur (zip/next loc)) ;; base case: keep moving
      (recur (zip/next (zip/remove loc)))))) ;; evil big numbers

(def z2 (zip/vector-zip [1 2 :a :b [3 4 :c :d 5] :e]))

(defn strategy-one-a
  "WARNING: infinite loop. Trying to wrap adjacent keyword siblings in
  a branch node (vector)."
  [loc]
  (if (zip/end? loc)
    loc
    (if (keyword? (zip/node loc))
      (recur (zip/next (zip/edit loc #(vector %))))
      (recur (zip/next loc)))))

(defn parent-ok?
  "Helper function for determining if a keyword is already in an
  all-keyword branch."
  [loc]
  (when (zip/up loc)
    (every? keyword? (zip/children (zip/up loc)))))

(defn strategy-one-b
  "Wraps keywords, but individually."
  [loc]
  (if (zip/end? loc)
    loc
    (if (and (keyword? (zip/node loc))
             (not (parent-ok? loc)))
      (recur (zip/next (zip/edit loc #(vector %))))
      (recur (zip/next loc)))))


(defn strategy-one-c
  "Wraps adjacent keyword leaf nodes in a new branch node, but doesn't
  remove them from where they were before, so we end up with lots of
  duplicate nodes."
  [loc]
  (if (zip/end? loc)
    loc
    (if (and (keyword? (zip/node loc))
             (not (parent-ok? loc))) ;; up to here, same as before
      (let [right-keys (take-while #(keyword? %) (zip/rights loc))]
        (recur (zip/next 
                (reduce (fn [z n] 
                          (zip/append-child z n))
                        (zip/edit loc #(vector %))
                        right-keys))))
      (recur (zip/next loc)))))


(defn strategy-one-d
  "Does the job: wraps adjacent keywords. Not an elegant solution
  though."
  [loc]
  (if (zip/end? loc)
    loc
    (if (and (keyword? (zip/node loc))
             (not (parent-ok? loc))) 
      (let [right-keys (take-while #(keyword? %) (zip/rights loc))
            loc-no-rights (reduce (fn [z _] (zip/remove (zip/right z))) loc right-keys)
            loc-w-incorporated-keys (reduce (fn [z n] 
                                              (zip/append-child z n)) 
                                            (zip/edit loc-no-rights #(vector %)) 
                                            right-keys)]
        (recur (zip/next loc-w-incorporated-keys)))
      (recur (zip/next loc)))))


(defn wrap-keywords
  "Helper function. Given a seq, wraps any adjacent keyword elements
  in a vector."
  [lst]
  (into []
        (mapcat #(if (keyword? (first %))
                   (list (apply vector %))
                   %)
                (partition-by keyword? lst))))


(defn strategy-two
  "Wraps adjacent keywords in a branch node. A more pleasing solution."
  [loc]
  (if (zip/end? loc)
    loc
    (if (and (zip/branch? loc)
             (some keyword? (zip/children loc))
             (not (every? keyword? (zip/children loc))))
      (recur (zip/next (zip/edit loc wrap-keywords)))
      (recur (zip/next loc)))))
