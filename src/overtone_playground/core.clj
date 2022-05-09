(ns overtone-playground.core
  (:require
   [overtone.live :refer :all]
   [overtone.at-at :as a]
   [overtone.inst.synth :as sth]
   [overtone-playground.samples :as s]
   [overtone.midi :as midi]))

(def global_bpm (metronome 120))

(defn play
  "Can be used in following ways:

  For playing single notes:
  (play 60)
  (play :C4)

  For playing multiple notes/chords (accepts both collections and multiple-single arguments):
  (play 57 60 64)
  (play [57 60 64])
  (play :c3 :a4 :f3) <-- arguments are case insensitive, btw.
  (play [:C3 :A4 :F3]) <-- as you can see here.
  (play (chord :a3 :minor))"

  ([]
   (play 60))
  ([x]
   (if (seqable? x)
       (map play x)
       (if (keyword? x)
         (sth/overpad (note x))
         (sth/overpad x))))
  ([x & args]
   (play (conj args x))))

;; This is leftover from previous imperfect noodling around. Left it for now, will be deleted
;; in the refactoring process.
;; (if (seqable? x)
;;   (doseq [note x] (sth/overpad note))
;;   (if (keyword? x)
;;     (sth/overpad (note x))
;;     (sth/overpad x)))

(defn melody [notes sleep]
  (if (empty? notes) nil
      (doseq []
        (play (first notes))
        (Thread/sleep sleep)
        (melody (rest notes) sleep))))

(defn vechord [x y]
  (conj (vec (chord x y)) 0))


;; This is bad way to do things, since it will play sounds when whole file is evaluated.
;; Keeping it as a way to quickly check if sound is working.
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


;; (defn looper
;;   "Function used for looping audio samples. Takes two arguments, tempo and sample path.
;;   Use it like this:
;;   (looper (metronome 120) s/bd_tek)
;;   To stop the loop, evaluate (stop)"
;;   [bpm sound]
;;   (let [beat (bpm)]
;;     (at (bpm beat) (sound))
;;     (apply-by (bpm (inc beat)) looper bpm sound [])))


(def my-pool (overtone.at-at/mk-pool))

(defn show-loops [] (a/show-schedule my-pool))

(defn stop-loop
  "stops desired loop. Uses Job ID for loop reference. If called with no arguments stops all loops.
  Example usage: (stop-loop 23), (stop-loop 23 26 30) or (stop-loop). To print a list of loops in
  the REPL use (show-loops) function."
  ([]
   (map stop-loop (filter number? (flatten (map first (a/scheduled-jobs my-pool)))) ))
  ([num]
   (a/stop num my-pool))
  ([num & args]
   (map stop-loop (conj args num))))

(defn looper
  ([func ms]
   (a/every ms #(func) my-pool))
  ([func ms desc]
   (a/every ms #(func) my-pool :desc desc)))
