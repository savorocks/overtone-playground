(ns overtone-playground.core
  (:require
   [overtone.live :refer :all]
   [overtone.inst.synth :as sth]
   [overtone-playground.samples :as s]
   [overtone.midi :as midi]))

(defn play [x]
  "Can play either single tone: (play 60), (play :C4) or Whole chords"
  (if (seqable? x)
    (doseq [note x] (sth/overpad note))
    (if (keyword? x)
      (sth/overpad (note x))
      (sth/overpad x))))

(defn melody [notes sleep]
  (if (empty? notes) nil
      (doseq []
        (play (first notes))
        (Thread/sleep sleep)
        (melody (rest notes) sleep))))

(defn vechord [x y]
  (conj (vec (chord x y)) 0))

(doseq []
  (melody (vechord :a3 :minor) 250)
  (melody (vechord :f3 :major) 250)
  (melody (vechord :c4 :major) 250)
  (melody (vechord :g3 :major) 250)
  (melody (vechord :a3 :minor) 250)
  (melody (vechord :f3 :major) 250)
  (melody [48 52 55 0] 250)
  (melody [43 50 55 0] 250))

(definst saw-wave [freq 440
                   attack 0.01
                   sustain 0.4
                   release 0.1
                   vol 0.4]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn looper
  "Function used for looping audio samples. Takes two arguments, tempo and sample path.
  Use it like this:
  (looper (metronome 120) s/bd_tek)
  To stop the loop, evaluate (stop)"
  [bpm sound]
  (let [beat (bpm)]
    (at (bpm beat) (sound))
    (apply-by (bpm (inc beat)) looper bpm sound [])))
