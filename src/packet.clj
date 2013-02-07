(ns packet
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(defne counto [l n]
  ([() 0])
  ([[h . t] _]
     (fresh [m]
            (fd/in m (fd/interval 0 Integer/MAX_VALUE))
            (fd/+ m 1 n)
            (counto t m))))

(defne binaryo [p n]
  ([() 0])
  ([[h . t] _]
     (fresh [m m-times-two]
            (fd/in m (fd/interval 0 Integer/MAX_VALUE))
            (fd/in h (fd/interval 0 1))
            (binaryo t m)
            (fd/* m 2 m-times-two)
            (fd/+ m-times-two h n))))

(defne packeto [p pattern]
  ([() ()])
  ([_ [:chunk n c . t]]
     (fresh [pt]
            (appendo c pt p)
            (counto c n)
            (packeto pt t)))
  ([_ [:binary n v . t]]
     (fresh [c np]
            (appendo [:chunk n c] t np)
            (packeto p np)
            (binaryo c v))))

(run 1 [q]
     (fresh [b c
             rest-size rest-chunk]
            (packeto [1 0 0 1 0 1 0 1 1 1 1 1]
                     [:binary 4 b
                      :chunk 6 c
                      :chunk rest-size rest-chunk])
            (== q [b c])))
;; => ([9 (0 1 0 1 1 1)])

(run 1 [q]
     (fresh [b c
             rest-size rest-chunk]
            (packeto [1 0 0 1 0 1 0 0 1 0 1 1 1 1 1 1]
                     [:binary 4 b
                      :chunk b c
                      :chunk rest-size rest-chunk])
            (== q [b c])))
;; => ([9 (0 1 0 0 1 0 1 1 1)])

(run 1 [q]
     (fresh [b c]
            (packeto q
                     [:binary 4 b
                      :chunk b [0 1 0 1 0 1 0 1 1]])))
;; => ((1 0 0 1 0 1 0 1 0 1 0 1 1))