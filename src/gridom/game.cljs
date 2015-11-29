(ns gridom.game
  (:require [om.core :as om :include-macros true]
            [gridom.utils :as utils]))

(def rows-n 5)
(def cols-n 5)
(def board-n (* rows-n cols-n))
(def mana-regen 4)
(def mana-eff 7.5)
(def peril-scale 1)

(defn mkbox [[id _v]]
  {:id id
   :v  60})

(defn mkboard [size]
  (vec (map mkbox (zipmap (range size) (repeat 100)))))

; defonce to make this only initialize on hard reload
(def app-state
  (atom {:mana  100
         :board (mkboard board-n)
         :live (vec (range board-n))}))

(defn dead? [box]
  (= 0 (:v box)))
(defn alive? [box]
  (not (dead? box)))

(defn heal [box v]
  (if (alive? box)
    (assoc-in box [:v] (utils/minmax 0 (+ (:v box) v) 100))
    box))

(defn heal-in [boxes pred v]
  (vec (map #(if (pred %) (heal % v) %) boxes)))

(defn mana-gain [data v]
  (assoc data :mana (utils/minmax 0 (+ (:mana data) v) 100)))

(defn boxplus [a b]  ; TODO retest
  (let [[aid bid] [(:id a) (:id b)]
        m5 (mod aid cols-n)]
    (or (= bid aid)
      (= bid (- aid rows-n))
      (= bid (+ aid rows-n))
      (and (= bid (- aid 1)) (not= 0 m5))
      (and (= bid (+ aid 1)) (not= 4 m5)))))

(defn boxrow [a b]
  (let [[aid bid] [(:id a) (:id b)]
        c1 (- aid (mod aid cols-n))]
    (some #{bid} (range c1 (+ c1 cols-n)))))

(defn boxcol [a b]
  (let [[aid bid] [(:id a) (:id b)]
        d (Math/abs (- aid bid))]
    (= 0 (mod d 5))))

(defn target-bonus [targets]
  (/ (Math/log targets) (Math/log 5)))

(defn spell-cost [spell]
  (let [t (:targets spell)
        e (+ mana-eff (target-bonus t))
        h (:hp spell)]
    (assoc spell :mana (/ (* h t) e))))

(def spells
  (utils/fmap spell-cost
    {:single {:hp   30 :targets 1
              :pred (fn [center box] (= center box))}
     :row    {:hp   20 :targets 5
              :pred (fn [center box] (boxrow center box))}
     :col    {:hp   20 :targets 5
              :pred (fn [center box] (boxcol center box))}
     :plus   {:hp   20 :targets 5
              :pred (fn [center box] (boxplus center box))}
     :all    {:hp   5 :targets 25
              :pred (fn [center box] (boxplus center box))}
     }))
(doseq [[k s] spells]
  (print k (/ (* (:hp s) (:targets s)) (:mana s))))

(def binds
  {:left  :col
   :mid   :plus
   :right :row})

(def perils_by_delay
  ; {freq_ms [pred(box) v]
  {500  [[#(>= 0.25 (rand)) 3]]                             ; 1.5 dps * 25
   1500 [[#(>= 0.25 (rand)) 3]]                             ; 0.5 dps * 25
   3000 [[#(= (:id %) (+ 1 (rand-int 25))) 30]]             ; 10 dps
   })

(defn cast [data box input]
  (let [{:keys [hp mana pred]} (get spells (get binds input))]
    (if (>= (:mana @app-state) mana)
      (do
        ; TODO combine
        (om/transact! data
          (fn [d] (assoc d :board (heal-in (:board d) (partial pred box) hp))))
        (om/transact! data
          (fn [x] (mana-gain x (- mana))))
        ))))
