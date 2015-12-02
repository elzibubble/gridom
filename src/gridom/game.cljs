(ns gridom.game
  (:require [om.core :as om :include-macros true]
            [gridom.utils :as utils]))

;; TODO score
;; TODO peril list
;; TODO penalise death (live list)
;; TODO key binds

(def rows-n 5)
(def cols-n 5)
(def board-n (* rows-n cols-n))
(def mana-eff 6)
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
         :live  (vec (range board-n))}))

(defn dead? [box]
  (= 0 (:v box)))
(defn alive? [box]
  (not (dead? box)))

(defn heal [box v]
  (if (alive? box)
    (assoc-in box [:v] (utils/minmax 0 (+ (:v box) v) 100))
    box))
(defn hurt [box v] (heal box (* (- v) peril-scale)))
(defn heal-in [boxes pred v]
  (vec (map #(if (pred %) (heal % v) %) boxes)))
(defn hurt-in [boxes pred v]
  (vec (map #(if (pred %) (hurt % v) %) boxes)))

(defn mana-gain [data v]
  (assoc data :mana (utils/minmax 0 (+ (:mana data) v) 100)))

(defn boxplus [a b]
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

(defn mana-regen [data]
  (let [c (count (:live data))]
    (* c (/ 1 25))))

(defn target-bonus [targets]
  (/ (Math/log targets) (Math/log 5)))

(defn spell-cost [spell]
  (let [t (:targets spell)
        e (+ mana-eff (target-bonus t))
        h (:hp spell)]
    (assoc spell :mana (/ (* h t) e))))

(def spells
  (utils/fmap spell-cost
    {:single {:name "Illumination"
              :hp   50 :targets 1
              :pred (fn [center box] (= center box))}
     :row    {:name "Wall of Light"
              :hp   30 :targets 5
              :pred (fn [center box] (boxrow center box))}
     :col    {:name "Lightray"
              :hp   30 :targets 5
              :pred (fn [center box] (boxcol center box))}
     :plus   {:name "Radiant Burst"
              :hp   30 :targets 5
              :pred (fn [center box] (boxplus center box))}
     :all    {:name "Serenity"
              :hp   10 :targets 25
              :pred (fn [center box] (boxplus center box))}
     }))

(def binds
  {:left  :col
   :1     :col
   :mid   :single
   :2     :single
   :right :row
   :3     :row})

(defn perbox_peril_fn [pred]
  (fn perbox [peril data]
    (assoc data :board (hurt-in (:board data) pred (:hp peril)))))

(defn idlist_peril_fn [ids_fn]
  (fn [peril data]
    (let [ids (ids_fn data)]
      ;(print (:name peril) ids)
      (reduce (fn [acc n]
                (let [box (nth (get acc :board) n)]
                  (assoc-in data [:board n]
                    (hurt box (:hp peril)))))
        data ids))))

(def perils
  {:rand_25 {:name "Tremor"
             :desc "25% chance of {:hp} damage to each raid member"
             :func (perbox_peril_fn #(>= 0.25 (rand)))
             :hp   3}
   :slam    {:name "Slam"
             :desc "{:hp} damage to one live raid member"
             :func (idlist_peril_fn #(condp = (:live %)
                                      [] []
                                      [(rand-nth (:live %))]))
             :hp   30}
   :rampage {:name "Rampage"
             :desc "Don't let more than 5 people die!"
             :func (idlist_peril_fn
                     #(condp >= (count (:live %))
                       0 []
                       19 [(first (:live %))]
                       []))
             :hp   100}
   })

(def perils_by_delay
  {150  [:rampage]
   500  [:rand_25]                                          ;; 1.5 dps * 25
   1500 [:rand_25]                                          ;; 0.5 dps * 25
   3000 [:slam]                                             ;; 10 dps
   })

(defn cast [data box input]
  (let [{:keys [hp mana pred]} (get spells (get binds input))]
    (if (and (>= (:mana @app-state) mana)
          (alive? box))
      (do
        ; TODO combine
        (om/transact! data
          (fn [d] (assoc d :board (heal-in (:board d) (partial pred box) hp))))
        (om/transact! data
          (fn [x] (mana-gain x (- mana))))
        ))))
