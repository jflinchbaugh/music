(ns music.scratch
  (:require [overtone.live :refer :all]
            [music.core :refer :all]
            [overtone.inst.drum :as d]
            [overtone.inst.sampled-piano :as sp]
            [overtone.inst.piano :as p]
            [overtone.inst.sampled-flute :as f]))

(def ring-hat (freesound 12912))
(def snare (freesound 26903))
(def click (freesound 406))
(def wop (freesound 85291))
(def subby (freesound 25649))

(def pats {subby [0 0 0 0 0 0 0 0]
           snare [0 0 0 0 0 0 0 0]
           wop   [0 0 0 0 0 0 0 0]})

(def live-pats (atom pats))

(defn flatten1
  "Takes a map and returns a seq of all the key val pairs:
      (flatten1 {:a 1 :b 2 :c 3}) ;=> (:b 2 :c 3 :a 1)"
  [m]
  (reduce (fn [r [arg val]] (cons arg (cons val r))) [] m))

(defn live-sequencer
  ([curr-t sep-t live-patterns] (live-sequencer curr-t sep-t live-patterns 0))
  ([curr-t sep-t live-patterns beat]
   (doseq [[sound pattern] @live-patterns
           :let [v (nth pattern (mod beat (count pattern)))
                 v (cond
                     (= 1 v)
                     []

                     (map? v)
                     (flatten1 v)

                     :else
                     nil)]
           :when v]
     (at curr-t (apply sound v)))
   (let [new-t (+ curr-t sep-t)]
     (apply-by new-t #'live-sequencer [new-t sep-t live-patterns (inc beat)]))))

(defn normalise-beat-info
  [beat]
  (cond
    (= 1 beat)         {}
    (map? beat)        beat
    (sequential? beat) beat
    :else              {}))

(defn schedule-pattern
  [curr-t pat-dur sound pattern]
  {:pre [(sequential? pattern)]}
  (let [beat-sep-t (/ pat-dur (count pattern))]
    (doseq [[beat-info idx] (partition 2 (interleave pattern (range)))]
      (let [beat-t    (+ curr-t (* idx beat-sep-t))
            beat-info (normalise-beat-info beat-info)]
        (if (sequential? beat-info)
          (schedule-pattern beat-t beat-sep-t sound beat-info)
          (at beat-t (apply sound (flatten1 beat-info))))))))

(defn live-sequencer
  [curr-t pat-dur live-patterns]
  (doseq [[sound pattern] @live-patterns]
    (schedule-pattern curr-t pat-dur sound pattern))
  (let [new-t (+ curr-t pat-dur)]
    (apply-by new-t #'live-sequencer [new-t pat-dur live-patterns])))

(comment

  (live-sequencer (+ 200 (now)) 200 live-pats)

  (stop)

  (do
    (swap! live-pats assoc subby [0 0 0 0 0 0 0 0])
    (swap! live-pats assoc snare [0 0 0 0 0 0 0 0])
    (swap! live-pats assoc wop   [0 0 0 0 0 0 0 0]))

  (swap! live-pats assoc subby [1 1 0 1 0 0 1 1])
  (swap! live-pats assoc snare [1 1 0 0 0 1 0 0])
  (swap! live-pats assoc wop   [1 0 0 0 0 0 0 0])

  (swap! live-pats assoc subby [1 0 0 0 1 0 0 0 1 0 0 1 0 1 0 0])
  (swap! live-pats assoc snare [1 1 0 1 0 1 0 1])

  (swap! live-pats assoc wop   [1 0 0 1 1 0 0 1])

  (swap! live-pats assoc p/sampled-piano  [0 0 0 0 0 0 0])
  (swap! live-pats dissoc p/sampled-piano)
  (swap! live-pats dissoc snare)
  (swap! live-pats dissoc wop)

  (def a {:rate 0.5})
  (def b {:rate 3})
  (def c {:rate 10})

  (swap! live-pats assoc subby [1 1 0 b 0 1 a c])
  (swap! live-pats assoc snare [1 1 c c 1 a b c])
  (swap! live-pats assoc wop   [c c 1 0 0 0 a c])

  ;; 4/4

  (swap! live-pats assoc subby [1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])
  (swap! live-pats assoc snare [0 1 0 0 1 0 0 0 1 0 0 1 0 1 0 0])

  (live-sequencer (now) 2000 live-pats)
  (swap! live-pats assoc subby [1 1 0 b 0 1 [1 1 1] [1 1 1 1 1 1 1]])
  (swap! live-pats assoc snare [1 1 c c 1 a [1 a c 1] c])
  (swap! live-pats assoc wop   [c a 0 0 a c c c])

  (stop)

  (for [x (range 200)]
    (demo 0.1 (sin-osc (+ 400 (* 2 x)))))

  (envelope [0 0.5 1] [1 1] :step)

  (let [env (envelope [0 1] [2] :sqr)]
    (demo (sin-osc :freq (+ 200 (* 200 (env-gen env :action FREE))))))
  (demo (* (env-gen (lin 0.1 1 1 0.25) :action FREE) (sin-osc)))
  (demo (* 0.25 (linen (impulse 0) 0.1 1 1.9 :action FREE) (sin-osc)))

  (demo (let [dur 1
              env (sin-osc:kr (/ 1 (* 2 dur)))]
          (line:kr 0 1 dur :action FREE)
          (* env (saw 220))))

  (demo (let [dur 1
              env (abs (sin-osc:kr (/ 10 dur)))]
          (line:kr 0 1 dur :action FREE)
          (* env (saw 220))))

  (demo (let [dur 1
              env (abs (sin-osc:kr :freq (/ 1 dur) :mul 0.5 :add 1))]
          (line:kr 0 1 dur :action FREE)
          (* env (saw 220))))

  (demo (let [dur 1
              env (abs (lf-saw :freq (/ 1 dur) :mul 0.5 :add 1))]
          (line:kr 0 1 dur :action FREE)
          (* env (saw 220))))

  (demo (let [dur 1
              env (lf-saw :freq (/ 1 dur) :iphase -2 :mul 0.5 :add 1)]
          (line:kr 0 1 dur :action FREE)
          (* 0.1 env (saw 220))))

  (defn generate-sinewave-buffer [len]
    (let [wavetable-vector (mapv (fn [n] (Math/sin (/ (* 2 Math/PI n) len))) (range 0 len))
          empty-buffer     (buffer len)
          wavetable-buffer (buffer-write! empty-buffer wavetable-vector)]
      wavetable-buffer))

  (def our-sinewave-table (generate-sinewave-buffer 1024))

  (demo (let [buf                 our-sinewave-table
              frequency           440
              table-size          (buffer-size buf)
              current-sample-rate (sample-rate)
              playback-rate       (* frequency (/ table-size current-sample-rate))]
          (play-buf:ar 1 buf :rate playback-rate :loop true :action FREE)))

  (definst foo [freq 400] (* 0.3 (saw freq)))

  (foo)

  (ctl foo :freq 700)

  (stop)

  (odoc saw)

  (definst trem [freq 440 depth 10 rate 6 length 3]
    (* 0.3
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (sin-osc:kr rate))))))

  (trem)

  (trem 300 20 1 5)

  (examples)

  (demo 10 (example compander :noise-gate))

  (prn (mouse-x))

  (apropos 'adsr)

  (demo (* (env-gen (perc 0.02 0.1 4) :action FREE) (brown-noise)))

  (let [freq 220]
    (demo (pluck (* [1 1] (white-noise) (env-gen (perc 0.001 1) :action FREE)) 1 2 (/ 1 freq))))

  (demo 7 (lpf (mix (saw [50 (line 100 1600 5) 101 100.5]))
               (lin-lin (lf-tri (line 2 20 5)) -1 1 400 (4000))))

  (d/noise-snare (midi->hz 64) 1 0.5)

  (demo (* [1 1] (white-noise)))

  (* [1 1] (brown-noise) (env-gen (perc 0.001 0.5) :action FREE))

  (p/sampled-piano (midi->hz (chord :a4 :major)))

  (sp/sampled-piano 70)

  (for [n (chord :a2 :major)]
    (sp/sampled-piano n))

  (demo (sin-osc:kr 1000))

  (now)

  (doseq []
    (at (+ (now) 0) (sp/sampled-piano 60))
    (at (+ (now) 0) (d/noise-snare (midi->hz 60) 1 0.5))
    (at (+ (now) 250) (sp/sampled-piano 52))
    (at (+ (now) 500) (sp/sampled-piano 65))
    (at (+ (now) 500) (sp/sampled-piano 67)))

  (for [t (range 0 10)]
    (let [tt (* t 50)
          n (+ t 40)
          voices 3
          level (/ 1.0 voices)]
      (at (+ tt (now))
          (sp/sampled-piano n level)
          (sp/sampled-piano (+ 3 n) level)
          (sp/sampled-piano (+ 5 n) level))))

  (stop)

  (definst tone [freq 400 amp 0.8] (* amp (env-adsr 0.2 0 1 0.5) (sin-osc freq)))

  (looper 100 (partial wop))

  (tone)

  (looper 500 (partial sp/sampled-piano 65))

  (looper 500 (partial sp/sampled-piano 60))

  (stop)

  (now)

  ((constantly [1 2 3]))

  (cycle [1 2 3])

  (tone)
  (sp/sampled-piano)

  (def attack (atom 1.0))

  (definst steel-drum [note 60 amp 0.8 attack 0.5 decay 0.5]
    (let [freq (midicps note)]
        (* amp
          (env-gen (perc attack decay) 1 1 0 1 :action FREE)
          (+ (sin-osc (/ freq 2))
            (rlpf (saw freq) (* 1.1 freq) 0.4)))))

  (stop)

  (steel-drum 80)

  (midi-connected-devices)

  (event-debug-on)

  (event-debug-off)

  (on-event [:midi :note-on]
            (fn [e]
              (let [note (:note e)
                    freq (midi->hz note)
                    vel  (+ 0.25 (* 0.75 (:velocity-f e)))]
                (prn "+" note vel)
                #_(sp/sampled-piano note vel)
                (steel-drum note vel @attack (- 1 @attack))))
            ::keyboard-handler)

  (on-event [:midi :note-off]
            (fn [e]
              (let [vel (:velocity-f e)
                    note (:note e)]
                (prn "-" note)))
            ::key-off-handler)

  (on-event [:midi :control-change]
            (fn [e]
              (let [vel (:velocity-f e)
                    data1 (:note e)
                    data2 (:data2 e)]
                (prn data1 data2 vel)
                (if (= 106 data1) (stop))
                (if (= 1 data1) (reset! attack vel))))
            ::control-handler)

  (on-event [:midi :pitch-bend]
            (fn [e]
              (let [vel (:velocity-f e)
                    data1 (:data1 e)
                    data2 (:data2 e)
                    ev (dissoc e :device :msg :dev-key)
                    bend (if (> 64 data2) data2 (+ -127 data2))]
                (prn "pitch" bend)
                ))
            ::bend-handler)


  (remove-event-handler ::bend-handler )
  (remove-event-handler ::control-handler)
  (remove-event-handler ::key-off-handler)
  (remove-event-handler ::keyboard-handler)

  nil)

