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
     (doall(map play x))
     (if (keyword? x)
       (sth/overpad (note x))
       (sth/overpad x))))
  ([x & args]
   (play (conj args x))))

;; (defn melody [notes sleep]
;;   (if (empty? notes) nil
;;       (doseq []
;;         (play (first notes))
;;         (Thread/sleep sleep)
;;         (melody (rest notes) sleep))))

(defn vechord [x y]
  (conj (vec (chord x y)) 0))


;; This is bad way to do things, since it will play sounds when whole file is evaluated.
;; Keeping it as a way to quickly check if sound is working.
;; (doseq []
;;   (melody (vechord :a3 :minor) 250)
;;   (melody (vechord :f3 :major) 250)
;;   (melody (vechord :c4 :major) 250)
;;   (melody (vechord :g3 :major) 250)
;;   (melody (vechord :a3 :minor) 250)
;;   (melody (vechord :f3 :major) 250)
;;   (melody [48 52 55 0] 250)
;;   (melody [43 50 55 0] 250))

(definst saw-wave [freq 440
                   attack 0.01
                   sustain 1
                   release 0.1
                   vol 0.4]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(def my-pool (overtone.at-at/mk-pool))

(defn show-loops
  "Prints the loops that are running in the REPL. Does not take arguments!"
  []
  (a/show-schedule my-pool))

(defn stop-loop
  "Stops desired loop. Uses Job ID for loop reference. If called with no arguments stops all loops. Example usage: (stop-loop 23), (stop-loop 23 26 30) or (stop-loop). To print a list of loops in the REPL use (show-loops) function."
  ([]
   (map stop-loop (filter number? (flatten (map first (a/scheduled-jobs my-pool))))))
  ([num]
   (a/stop num my-pool))
  ([num & args]
   (map stop-loop (conj args num))))

(defn looper
  "Repeats call to given function in a period of milliseconds. Takes optional third argument which serves as a description to help you identify the loop when you list all loops. Should be used like this:
  (looper #(play :c3) 250)
  (looper #(melody (scale :g4 :major) 250) 5000)
  Keep in mind that you always have to give it anonymous function like in above examples, otherwise it won't repeat it."
  ([func ms]
   (a/every ms #(func) my-pool))
  ([func ms desc]
   (a/every ms #(func) my-pool :desc desc)))

(defn melody
  [notes ms]
  (loop [not-important []
         init-time (- (now) ms)
         notes notes
         ms ms]
    (if (= 1 (count notes))
      (a/at (+ ms init-time) #(play (first notes)) my-pool)
      (recur (a/at (+ ms init-time) #(play (first notes)) my-pool) (+ ms init-time) (rest notes) ms))))


(doseq []
  (looper #(melody (scale :c4 :minor) 250) (* (count (scale :c4 :minor)) 250))
  (looper #(s/bd_haus) 500)
  (looper #(s/bass_thick_c) 500))

(doseq []
  (looper #(play (rand-nth (scale :c4 :minor))) 250)
  (looper #(s/bd_haus) 500)
  (looper #(s/bass_thick_c) 500))
