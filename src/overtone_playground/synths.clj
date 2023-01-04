(ns overtone-playground.synths
  (:require
   [clojure.repl :refer [source doc]]
   [overtone.core :refer :all]
   [overtone.inst.synth :as sth]))o

(defn bass
  ([]
   (bass 60))
  ([note]
   (bass note 0.6 0.7))
  ([note t]
   (bass note t 0.7))
  ([note t amp]
   (sth/bass (midi->hz note) t amp)))
