(ns composer.sketches
  (:use overtone.live)
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l])
  (:require [quil.core :as q]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SKETCHES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEWARE
;;
;; These are just my experiments, you probably
;; don't want to be here! Please look at
;; presentation.clj for something much more
;; pleasing (and easy to understand)

(stop)

(definst foo [] (saw 220))

(foo)

(kill foo)

(odoc saw)

(definst bar [freq 220] (saw freq))

(bar 110)
(kill bar)
(bar)

(definst bar [freq 440] (* 0.3 (saw freq)))

(bar 110)

(kill bar)

(bar 220)
(bar 660)
(kill bar)

(foo)
(bar 220)
(bar 660)

(stop)

(definst quux [freq 440] (* 0.3 (saw freq)))
(quux)
(ctl quux :freq 440)
(kill quux)

(odoc line:kr)
(odoc sin-osc)

(definst trem [freq 440 depth 10 rate 6 length 3]
    (* 0.3
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (sin-osc:kr rate))))))

(trem)
(trem 200 60 0.8)
(trem 60 30 0.2)
(trem 200 60 8)

(definst sin-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (sin-osc freq)
     vol))
(sin-wave)

(def notes
  {:a 440.000
   :g-sharp 415.305
   :g 391.995})

(doseq [[_ freq] notes]
  (sin-wave freq))

(sin-wave)

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(saw-wave)

(definst square-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-pulse freq)
     vol))

(square-wave)

(definst noisey [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (pink-noise) ; also have (white-noise) and others...
     vol))

(noisey)

(definst triangle-wave [freq 440 attack 0.01 sustain 0.1 release 0.4 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))

(triangle-wave)

(definst spooky-house [freq 440 width 0.2 
                       attack 0.3 sustain 4 release 0.3 
                       vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (sin-osc (+ freq (* 20 (lf-pulse:kr 0.5 0 width))))
     vol))

(spooky-house)

(demo 10 (lpf (saw 100) (mouse-x 40 5000 EXP)))
(demo 10 (hpf (saw 100) (mouse-x 40 5000 EXP)))
(demo 30 (bpf (saw 100) (mouse-x 40 5000 EXP) (mouse-y 0.01 1 LIN)))
(let [freq 220]
  (demo (pluck (* (white-noise) (env-gen (perc 0.001 2) :action FREE)) 1 3 (/ 1 freq))))

(def snare (sample (freesound-path 26903)))
(snare)

(def t (sample (freesound-path 44293)))
(t)

(stop)

(def kick (sample (freesound-path 2086)))

(def one-twenty-bpm (metronome 120))

(defn looper [nome sound]    
  (let [beat (nome)]
    (at (nome beat) (sound))
    (apply-at (nome (inc beat)) looper nome sound [])))

(looper one-twenty-bpm kick)

(stop)

(def nome (metronome 200))
(nome)

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(saw-wave 440)
(saw-wave 523.25)
(saw-wave 261.62)

(saw-wave (midi->hz 69))
(saw-wave (midi->hz 72))
(saw-wave (midi->hz 60))

(saw-wave (midi->hz (note :A4)))
(saw-wave (midi->hz (note :C5)))
(saw-wave (midi->hz (note :C4)))

(defn note->hz [music-note]
  (midi->hz (note music-note)))

(saw-wave (note->hz :C5))

(defn saw2 [music-note]
  (saw-wave (midi->hz (note music-note))))

(defn triangle2 [music-note]
  (triangle-wave (midi->hz (note music-note))))

(saw2 :A4)
(saw2 :C5)
(saw2 :C4)

(defn play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(play-chord (chord :C4 :major))

(defn chord-progression-time []
  (let [time (now)]
    (at time (play-chord (chord :C4 :major)))
    (at (+ 2000 time) (play-chord (chord :G3 :major)))
    (at (+ 3000 time) (play-chord (chord :F3 :sus4)))
    (at (+ 4300 time) (play-chord (chord :F3 :major)))
    (at (+ 5000 time) (play-chord (chord :G3 :major)))))

(chord-progression-time)

(defonce metro (metronome 200))
(metro)
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 14 beat-num)) (play-chord (chord :F3 :major))))

(chord-progression-beat metro (metro))

(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))
  (apply-at (m (+ 16 beat-num)) chord-progression-beat m (+ 16 beat-num) []))
(chord-progression-beat metro (metro))

(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (saw2 :C4))
  (at (m (+ 4 beat-num)) (saw2 :G3))
  (at (m (+ 8 beat-num)) (saw2 :A3))
  (at (m (+ 12 beat-num)) (saw2 :F3))
  (apply-at (m (+ 16 beat-num)) chord-progression-beat m (+ 16 beat-num) []))
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (saw2 :C4))
  (at (m (+ 1 beat-num)) (saw2 :D4))
  (at (m (+ 2 beat-num)) (saw2 :E4))
  (at (m (+ 3 beat-num)) (saw2 :F4))
  (at (m (+ 4 beat-num)) (saw2 :G4))
  (at (m (+ 5 beat-num)) (saw2 :A4))
  (at (m (+ 6 beat-num)) (saw2 :B4))
  (at (m (+ 7 beat-num)) (saw2 :C5))
  (apply-at (m (+ 4 beat-num)) chord-progression-beat m (+ 8 beat-num) []))
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (saw2 :C4))
  (at (m (+ 1 beat-num)) (saw2 :B4))
  (at (m (+ 2 beat-num)) (saw2 :A4))
  (at (m (+ 3 beat-num)) (saw2 :F4))
  (at (m (+ 4 beat-num)) (saw2 :G4))
  (at (m (+ 5 beat-num)) (saw2 :E4))
  (at (m (+ 6 beat-num)) (saw2 :D4))
  (at (m (+ 7 beat-num)) (saw2 :C5))
  (apply-at (m (+ 4 beat-num)) chord-progression-beat m (+ 8 beat-num) []))
(chord-progression-beat metro (metro))

(saw2 :G4)
(saw2 :C4)

(stop)

;; major scales are things that go:
;; t t st t t t st
;; and uses all the ones in between
;; a permutation would sound nice
;; should keep first and last note the same
;;
;; if ends in V->I sounds finished, also IV->I

;; G major
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (saw2 :G4))
  (at (m (+ 1 beat-num)) (saw2 :A4))
  (at (m (+ 2 beat-num)) (saw2 :B4))
  (at (m (+ 3 beat-num)) (saw2 :C5))
  (at (m (+ 4 beat-num)) (saw2 :D5))
  (at (m (+ 5 beat-num)) (saw2 :E5))
  (at (m (+ 6 beat-num)) (saw2 :F#5))
  (at (m (+ 7 beat-num)) (saw2 :G5))
  (apply-at (m (+ 4 beat-num)) chord-progression-beat m (+ 8 beat-num) []))
(chord-progression-beat metro (metro))

(stop)

;; harmonic minor scale
;; t st t t st t1.5 st
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (saw2 :G4))
  (at (m (+ 1 beat-num)) (saw2 :A4))
  (at (m (+ 2 beat-num)) (saw2 :Bb4))
  (at (m (+ 3 beat-num)) (saw2 :C5))
  (at (m (+ 4 beat-num)) (saw2 :D5))
  (at (m (+ 5 beat-num)) (saw2 :Eb5))
  (at (m (+ 6 beat-num)) (saw2 :F#5))
  (at (m (+ 7 beat-num)) (saw2 :G5))
  (apply-at (m (+ 4 beat-num)) chord-progression-beat m (+ 8 beat-num) []))
(defn chord-progression-beat [m beat-num melody]
  (println melody)
  (doseq [[beat note] (zipmap (range) melody)]
    (at (m (+ beat beat-num)) (triangle2 note)))
  (apply-at (m (+ (count melody) beat-num)) chord-progression-beat m (+ (count melody) beat-num) melody []))

(chord-progression-beat metro (metro) [:G5 :A4 :Bb4 :C5 :Eb5 :F#5 :D5 :G4])
(chord-progression-beat metro (metro) [:G4 :A4 :Bb4 :C5 :D5 :Eb5 :F#5 :G5])
(stop)
;;
;; melodic minor scale
;;
;;

;; 12 (semi)tone scale (but 13 if you include the last)
;;semitone :B4 :C5
;;semitone :C5 :C#5
;;semitone :F4 :F#4
;;...
(stop)

;; semitones
(l/defrel semitone note-1 note-2)
(l/facts
 semitone
 (partition
  2 1 [:C3 :C#3 :D3 :D#3 :E3 :F3 :F#3 :G3 :G#3 :A3 :A#3 :B3
       :C4 :C#4 :D4 :D#4 :E4 :F4 :F#4 :G4 :G#4 :A4 :A#4 :B4
       :C5]))

[     :C3 :C#3
       :Db3 :D3 :D#3
       :Eb3 :E3 :E#3
       :Fb3 :F3 :F#3
       :Gb3 :G3 :G#3
       :Ab3 :A3 :A#3
       :Bb3 :B3 :B#3
       :Cb4 :C4
       ]


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
        (tone-and-a-half :C4 q))

;; scales
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

(stop)

#_(defn scaleo [scale melody]
  (fn [a]
    (apply
     l/bind* a
     (for [[tone-rel [note-1 note-2]] (zipmap scale (partition 2 1 melody))]
       (tone-rel note-1 note-2)))))

;; doesn't work, see later
#_(l/defne scaleo [scale melody]
  ([() ()])
  ([[sh . st] [[mh1 mh2] . mt]]
     (sh mh1 mh2)
     (scaleo st mt)))

(l/defne all-relso [rels lv]
  ([() _])
  ([[h . t] lv]
     (h lv)
     (all-relso t lv)))

(l/defrel even x)
(l/facts even [[2] [4] [6]])
(l/defrel fav x)
(l/facts fav [[4] [5]])

(l/run* [q]
        (even q)
        (fav q))

;; doesn't work:
#_(l/run* [q]
        (all-relso [even fav] q))
;; makes sense, as we can't just infer arbitary functions

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
  (l/run 4 [q]
         (l/fresh [scale
                   m1 m2 m3 m4 m5 m6 m7 m8]
                  (l/== scale [m1 m2 m3 m4 m5 m6 m7 m8])
                  (scaleo m1 m2 m3 m4 m5 m6 m7 m8)
                  (l/permuteo scale q)
                  )))

(l/defne lasto [l a]
  ([[a] a])
  ([[h . t] a] (lasto t a)))

(chord-progression-beat
 metro
 (metro)
 (apply concat
        (let [scaleo (scaleo-fn major-scale)
              scaleo (scaleo-fn harmonic-minor-scale)]
          (l/run 4 [q]
                 (l/fresh [scale
                           m1 m2 m3 m4 m5 m6 m7 m8
                           first-q last-q]
                          (l/== scale [m1 m2 m3 m4 m5 m6 m7 m8])
                          (scaleo m1 m2 m3 m4 m5 m6 m7 m8)
                          (l/permuteo scale q)
                          (l/firsto q first-q)
                          (lasto q last-q)
                          ;; these two can be switched
                          (l/== m1 first-q)
                          (l/== m8 last-q)
                          )))))

(stop)

(chord-progression-beat
 metro
 (metro)
 (apply concat
        (let [scaleo (scaleo-fn major-scale)
              scaleo (scaleo-fn harmonic-minor-scale)
              ]
          (l/run 8 [melody]
                 (l/fresh [m1 m2 m3 m4 m5 m6 m7 m8
                           scale
                           s1 s2 s3 s4 s5 s6 s7 s8
                           first-q last-q]
                          (l/== melody [m1 m2 m3 m4 m5 m6 m7 m8])
                          (l/== scale [s1 s2 s3 s4 s5 s6 s7 s8])
                          (scaleo s1 s2 s3 s4 s5 s6 s7 s8)
                          (l/permuteo scale melody)
                          ;; these two can be switched
                          (l/== m1 s1)
                          (l/== m8 s8)
;;                          (l/== m7 s5) ;; perfect cadence
                          (l/== m7 s4) ;; plagal cadence
                          )))))

(chord-progression-beat
 metro
 (metro)
 (apply concat
        (let [scaleo (scaleo-fn major-scale)
              ;;scaleo (scaleo-fn harmonic-minor-scale)
              ]
          (l/run 8 [melody2]
                 (l/fresh [melody
                           m1 m2 m3 m4 m5 m6 m7 m8
                           scale
                           s1 s2 s3 s4 s5 s6 s7 s8
                           first-q last-q]
                          (l/== melody [m1 m2 m3 m4 m5 m6 m7 m8])
                          (l/== scale [s1 s2 s3 s4 s5 s6 s7 s8])
                          (scaleo s1 s2 s3 s4 s5 s6 s7 s8)
                          (l/permuteo scale melody)
                          ;; these two can be switched
                          (l/== m1 s1)
                          (l/== m8 s8)
;;                          (l/== m7 s5) ;; perfect cadence
                          (l/== m7 s4) ;; plagal cadence
                          (l/== melody2 [m1 m2 m3 m4 m5 m6 m7 m1])
                          )))))

(stop)

(defn chord-no-loop [m beat-num melody]
  (println melody)
  (doseq [[beat note] (zipmap (range) melody)]
    (at (m (+ beat beat-num)) (triangle2 note))))

(recording-start "~/Desktop/simple.wav")

(chord-no-loop
 metro
 (metro)
 (apply concat
        (let [scaleo (scaleo-fn major-scale)
              ;;scaleo (scaleo-fn harmonic-minor-scale)
              ]
          (l/run 4 [melody2]
                 (l/fresh [melody
                           m1 m2 m3 m4 m5 m6 m7 m8
                           scale
                           s1 s2 s3 s4 s5 s6 s7 s8]
                          (l/== s1 :D#4)
                          (l/== melody [m1 m2 m3 m4 m5 m6 m7 m8])
                          (l/== scale [s1 s2 s3 s4 s5 s6 s7 s8])
                          (scaleo s1 s2 s3 s4 s5 s6 s7 s8)
                          (l/permuteo scale melody)
                          ;; these two can be switched
                          ;;(l/== m1 s1)
                          ;;(l/== m8 s8)
                          ;;                          (l/== m7 s5) ;; perfect cadence
                          ;;(l/== m7 s4) ;; plagal cadence
                          ;;(l/== m7 s2)
                          (l/== melody2 [m1 m2 m3 m4 m5 m6 m7 m1])
                          )))))


(stop)

(recording-stop)

(def m-atom (atom [:C4]))

(defn chord-progression-atom [m beat-num melody-atom]
  (println @melody-atom)
  (doseq [[beat note] (zipmap (range) @melody-atom)]
    (at (m (+ beat beat-num)) (triangle2 note)))
  (apply-at (m (+ (count @melody-atom) beat-num)) chord-progression-atom m (+ (count @melody-atom) beat-num) melody-atom []))

(chord-progression-atom metro (metro) m-atom)

(reset!
 m-atom
 (rand-nth
  (let [
        ;;scaleo (scaleo-fn major-scale)
        scaleo (scaleo-fn harmonic-minor-scale)
        ;;scaleo (scaleo-fn natural-minor-scale)
        ;;scaleo (scaleo-fn mixolydian-mode)
        
        ]
    (l/run 1024 [melody2]
           (l/fresh [melody
                     m1 m2 m3 m4 m5 m6 m7 m8
                     scale
                     s1 s2 s3 s4 s5 s6 s7 s8]
                    ;;(l/== s1 :D#4)
                    (l/== melody [m1 m2 m3 m4 m5 m6 m7 m8])
                    (l/== scale [s1 s2 s3 s4 s5 s6 s7 s8])
                    (scaleo s1 s2 s3 s4 s5 s6 s7 s8)
                    (l/permuteo scale melody)
                    ;; these two can be switched (but core.logic blows up)
                    (l/== m1 s1)
                    (l/== m8 s8)
                    
                    ;;(l/!= m2 s2) ;; second note cannot be number 2
                    
                    (l/== m7 s5) ;; perfect cadence
                    ;;(l/== m7 s4) ;; plagal cadence
                    ;;(l/== m7 s2)
                    (l/== melody2 [m1 m2 m3 m4 m5 m6 m7 m1])
                    )))))

;; Annabel likes these
[:C#4 :A4 :D#4 :C5 :F#4 :E4 :G#4 :C#4]
[:D#4 :G#4 :D5 :F4 :B4 :F#4 :A#4 :D#4]

(reset! m-atom [:D#4 :G#4 :D5 :F4 :B4 :F#4 :A#4 :D#4])

(stop)

(defn setup []
  (q/smooth)
  (q/frame-rate 1)
  (q/background 255))

(def white-notes
  [:C3 :D3 :E3 :F3 :G3 :A3 :B3
   :C4 :D4 :E4 :F4 :G4 :A4 :B4
   :C5])

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