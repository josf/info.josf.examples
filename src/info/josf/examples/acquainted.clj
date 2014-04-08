(ns info.josf.examples.acquainted
  "Functions used as examples for my first post about Clojure zippers:
  http://josf.info/blog/2014/03/21/getting-acquainted-with-clojure-zippers/"
  (:require [clojure.zip :as zip]))

(def zzz (zip/vector-zip [1 [:a :b] 2 3 [40 50 60]]))

(defn fast-forward [loc]
	(if (zip/end? loc)
	  (zip/node loc)
	  (recur (zip/next loc))))

 (defn fast-forward-z [loc]
	(if (zip/end? loc)
	  loc
	  (recur (zip/next loc))))

 (defn first-keyword [loc]
	(if (zip/end? loc)
	  nil
	  (if (keyword? (zip/node loc))
	    (zip/node loc)
	    (recur (zip/next loc)))))

