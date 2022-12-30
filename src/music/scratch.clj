(ns music.scratch
  (:require [overtone.core :refer :all]
            [music.core :refer :all]
            [overtone.inst.drum :as d]
            [overtone.inst.sampled-piano :as sp]
            [overtone.inst.sampled-flute :as f]
            [overtone.inst.synth :as synth]
            [overtone.synth.sts :as sts]))

(comment



  (def ring-hat (freesound 12912))
  (def snare (freesound 26903))
  (def click (freesound 406))
  (def wop (freesound 85291))
  (def subby (freesound 25649))

  (click)

  (snare)

  (wop)

  (subby)

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
       (sin-osc (+ freq (* depth (sin-osc:kr rate))))))

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

  (do
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

  (definst steel-drum [note 60 amp 0.8 attack 0.001 decay 0.3]
    (let [freq (midicps note)]
        (* amp
          (env-gen (perc attack decay) 1 1 0 1 :action FREE)
          (+ (sin-osc (/ freq 2))
            (rlpf (saw freq) (* 1.1 freq) 0.4)))))

  (stop)

  (steel-drum 80 1)

  (sp/sampled-piano 80 0.6)

  (d/tom (midi->hz 62) 0.6 0.6 1)


  (scale :a3 :major)

  (def scale-degrees [:vi :vii :i+ :_ :vii :_ :i+ :vii :vi :_ :vii :_])

  (def pitches (degrees->pitches scale-degrees :dorian :C4))


  ; setup a sound for our metronome to use
  (def kick (sample (freesound-path 2086)))

                                        ; setup a tempo for our metronome to use
  (def one-twenty-bpm (metronome 120))

  ; this function will play our sound at whatever tempo we've set our metronome to
  (defn looper [nome sound]
    (let [beat (nome)]
      (at (nome beat) (sound))
      (apply-by (nome (inc beat)) looper nome sound [])))

  ; turn on the metronome
  (looper one-twenty-bpm kick)
  (stop)

  (definst kick [freq 120 dur 0.3 width 0.5]
    (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
          env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
          sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
          src (sin-osc freq-env)
          drum (+ sqr (* env src))]
      (compander drum drum 0.2 1 0.1 0.01 0.01)))


  (kick 100)

  ;; degrees

  (def metro (metronome 120))

  (def scale-degrees [:vi :vii :i+ :_ :vii :_ :i+ :vii :vi :_ :vii :_])

  (def pitches (degrees->pitches scale-degrees :dorian :C4))

  (iterate #(prn %) pitches)

  (defn play [time notes sep]
    (let [note (first notes)]
      (when note
        (at time (saw (midi->hz note))))
      (let [next-time (+ time sep)]
        (apply-by next-time #'play [next-time (rest notes) sep]))))

  (play (+ 1000 (now)) pitches 200000)

  (metro (metro))

  (metro)

  (stop)


  ;; playing notes

  ;; We use a saw-wave that we defined in the oscillators tutorial
  (definst saw-wave [freq 440 attack 0.01 sustain 0.5 release 0.5 vol 0.4]
    (* (env-gen (env-lin attack sustain release vol) 1 1 0 1 FREE)
      (saw freq)
      vol))

  (stop)

  ;; We can play notes using frequency in Hz
  (saw-wave 440)
  (saw-wave 523.25)
  (saw-wave 261.62) ; This is C4

  ;; We can also play notes using MIDI note values
  (saw-wave (midi->hz 69))
  (saw-wave (midi->hz 72))
  (saw-wave (midi->hz 60)) ; This is C4

  ;; We can play notes using standard music notes as well
  (saw-wave (midi->hz (note :A4)))
  (saw-wave (midi->hz (note :C5)))
  (saw-wave (midi->hz (note :C4))) ; This is C4! Surprised?

  ;; Define a function for convenience
  (defn note->hz [music-note]
    (midi->hz (note music-note)))

  ; Slightly less to type
  (saw-wave (note->hz :C5))

  ;; Let's make it even easier
  (defn saw2 [music-note]
    (saw-wave :freq (midi->hz (note music-note)) :vol 0.2))

  ;; Great!
  (saw2 :A4)
  (saw2 :C5)
  (saw2 :C4)

  ;; Let's play some chords


  ;; this is one possible implementation of play-chord
  (defn play-chord [a-chord]
    (doseq [note (take 4 a-chord)] (saw2 note)))

  ;; We can play many types of chords.
  ;; For the complete list, visit https://github.com/overtone/overtone/blob/master/src/overtone/music/pitch.clj and search for "def CHORD"
  (play-chord (chord :C4 :major))

  ;; We can play a chord progression on the synth
  ;; using times:
  (defn chord-progression-time []
    (let [time (now)]
      (at time (play-chord (chord :C4 :major)))
      (at (+ 2000 time) (play-chord (chord :G3 :major)))
      (at (+ 3000 time) (play-chord (chord :F3 :sus4)))
      (at (+ 4300 time) (play-chord (chord :F3 :major)))
      (at (+ 5000 time) (play-chord (chord :G3 :major)))))

  (chord-progression-time)

  ;; or beats:
  (defonce metro (metronome 120))

  (metro-bpm metro 120)


  ;; We can use recursion to keep playing the chord progression
  (defn chord-progression-beat [m beat-num]
    (let [progression [:i+ :i+ :i+ :i+
                       :v :_ :v :_
                       :vi :_ :vi :_
                       :iv :iv :iv :v]
          notes (degrees->pitches progression :dorian :c3)]
      (doseq [[b n] (map list (range 16) notes)]
        (prn b n)
        (when n
          (at (m (+ b beat-num)) (play-chord (chord n :major)))))

      (doseq [b [0 2 6 10 14 14.5 15 15.5]]
        (at (m (+ b beat-num)) (d/kick)))

      #_(apply-at (m (+ (count notes) beat-num)) (var chord-progression-beat) m (+ (count notes) beat-num) []))
  )

  (chord-progression-beat metro (metro))

  (map note [:c3 :a3 :c4 :g3 :a3 :f3]);; => (60 55 57 53)

  (map note [:c3 :d3 :e3 :f3 :g3 :a3 :b3]);; => (60 55 57 53)


  (degrees->pitches [:i :ii :iii :iv :v :vi :vii] :major :c3);; => (48 50 52 53 55 57 59)

  (note :c3)

  (stop)

  (map (fn [b n] (-> [b n])) (range 16) [:i :ii :iii])


  nil)


