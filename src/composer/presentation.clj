(ns composer.presentation
  (:use overtone.live)
  (:require [quil.core :as q])
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l]))

;; Overtone - defining the chord and the play loop

(definst triangle-wave [freq 440 attack 0.01 sustain 0.1 release 0.4 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))

(triangle-wave)

(defn note->hz [music-note]
  (midi->hz (note music-note)))

(defn triangle2 [music-note]
  (triangle-wave (midi->hz (note music-note))))

(triangle2 :A4)
(triangle2 :C5)
(triangle2 :C4)

(defonce metro (metronome 200))
(metro)

(def m-atom (atom [:C4]))

(defn chord-progression-atom [m beat-num melody-atom]
  (println @melody-atom)
  (doseq [[beat note] (zipmap (range) @melody-atom)]
    (at (m (+ beat beat-num)) (triangle2 note)))
  (apply-at
   (m (+ (count @melody-atom) beat-num))
   chord-progression-atom
   m
   (+ (count @melody-atom) beat-num) melody-atom []))

(defn play []
  (chord-progression-atom metro (metro) m-atom))

(play)
(stop)

(reset! m-atom [:D#4 :G#4 :D5 :F4 :B4 :F#4 :A#4 :D#4])

;; Quil - Drawing notes for current m-atom melody

(defn setup []
  (q/smooth)
  (q/frame-rate 1)
  (q/background 255))

(def white-notes
  [:C3 :D3 :E3 :F3 :G3 :A3 :B3
   :C4 :D4 :E4 :F4 :G4 :A4 :B4
   :C5 :D5])

(def black-notes
  [:C#3 :D#3 :NO :F#3 :G#3 :A#3 :NO
   :C#4 :D#4 :NO :F#4 :G#4 :A#4 :NO])

(defn draw []
  (q/background 255)
  (q/stroke (q/color 0))
  (let [positions (merge
                   (zipmap white-notes (range))
                   (zipmap black-notes (range)))
        x (fn [pos] (+ 50 (* 30 pos)))
        y (fn [pos] (+ -20 (- (q/height) (* 7 pos))))
        line (fn [note] (q/line (x -1) (y (positions note))
                                (x  8) (y (positions note))))]
    (doseq [[pos note] (zipmap (range) @m-atom)]
      (q/fill (q/color 255))
      (q/ellipse
       (x pos)
       (y (positions note))
       14 11)
      (when (even? (positions note))
        (q/line (- (x pos) 10) (y (positions note))
                (+ (x pos) 10) (y (positions note))))
      (when (contains? (set black-notes) note)
        (q/fill (q/color 0))
        (q/text-align :right)
        (q/text-size 24)
        (q/text "#" (- (x pos) 7) (+ (y (positions note)) 9))))
    (doseq [line-note [:E3 :G3 :B3 :D4 :F4]]
      (line line-note))))

(q/defsketch sketch
  :title "Notes"
  :setup setup
  :draw draw
  :size [320 140])

(play)
(stop)

;; core.logic - The Composing Schemer

(l/defrel semitone note-1 note-2)
(l/facts
 semitone
 (partition
  2 1 [:C3 :C#3 :D3 :D#3 :E3 :F3 :F#3 :G3 :G#3 :A3 :A#3 :B3
       :C4 :C#4 :D4 :D#4 :E4 :F4 :F#4 :G4 :G#4 :A4 :A#4 :B4
       :C5]))

(l/run* [q]
        (semitone :C#4 q))

;; tones
(defn tone [note-1 note-2]
  (l/fresh [middle-note]
           (semitone note-1 middle-note)
           (semitone middle-note note-2)))

(l/run* [q]
        (tone :C#4 q))

;; tone and a half
(defn tone-and-a-half [note-1 note-2]
  (l/fresh [middle-tone]
           (tone note-1 middle-tone)
           (semitone middle-tone note-2)))

(l/run* [q]
        (tone-and-a-half :C#4 q))

;; scales and modes
(def major-scale
  [tone tone semitone tone tone tone semitone])
(def harmonic-minor-scale
  [tone semitone tone tone semitone tone-and-a-half semitone])
(def natural-minor-scale
  [tone semitone tone tone semitone tone tone])
(def locrian-mode
  [semitone tone tone semitone tone tone tone])
(def mixolydian-mode
  [tone tone semitone tone tone semitone tone])

(defn scaleo-fn [[s1 s2 s3 s4 s5 s6 s7]]
  (fn [m1 m2 m3 m4 m5 m6 m7 m8]
    (l/all
     (s1 m1 m2)
     (s2 m2 m3)
     (s3 m3 m4)
     (s4 m4 m5)
     (s5 m5 m6)
     (s6 m6 m7)
     (s7 m7 m8))))

(let [scaleo (scaleo-fn major-scale)]
  (l/run 2 [scale]
         (l/fresh [s1 s2 s3 s4 s5 s6 s7 s8]
                  (l/== scale [s1 s2 s3 s4 s5 s6 s7 s8])
                  (scaleo s1 s2 s3 s4 s5 s6 s7 s8))))

(reset!
 m-atom
 (rand-nth
  (let [scaleo (scaleo-fn major-scale)]
  (l/run 10 [scale]
         (l/fresh [s1 s2 s3 s4 s5 s6 s7 s8]
                  (l/== scale [s1 s2 s3 s4 s5 s6 s7 s8])
                  (scaleo s1 s2 s3 s4 s5 s6 s7 s8))))))

(play)
(stop)

;; The full thing

(reset!
 m-atom
 (rand-nth
  (let [
        scaleo (scaleo-fn major-scale)
        ;;scaleo (scaleo-fn harmonic-minor-scale)
        ;;scaleo (scaleo-fn natural-minor-scale)
        ;;scaleo (scaleo-fn mixolydian-mode)
        ]
    (l/run 512 [melody2]
           (l/fresh [melody
                     m1 m2 m3 m4 m5 m6 m7 m8
                     scale
                     s1 s2 s3 s4 s5 s6 s7 s8]
                    ;;(l/== s1 :D#4)
                    (l/== melody [m1 m2 m3 m4 m5 m6 m7 m8])
                    (l/== scale [s1 s2 s3 s4 s5 s6 s7 s8])
                    (scaleo s1 s2 s3 s4 s5 s6 s7 s8)
                    (l/permuteo scale melody)
                    (l/== m1 s1)
                    (l/== m8 s8)
                    (l/== m7 s5) ;; perfect cadence
                    ;;(l/== m7 s4) ;; plagal cadence
                    ;;(l/== m7 s2) ;; just nice cadence
                    (l/== melody2 [m1 m2 m3 m4 m5 m6 m7 m1]))))))

(play)
(stop)
