(ns gridom.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan <!]]
            [gridom.utils :as utils]))
(enable-console-print!)

(defn mkbox [[id _v]]
  {:id id
   :v 80})

(defn mkboard [size maxhp]
  (vec (map mkbox
         (zipmap (range 1 (+ size 1)) (repeat maxhp)))
    ))

; defonce to make this only initialize on hard reload
(def app-state
  (atom (let [rows_n 5
              cols_n 5
              size (* rows_n cols_n)
              board (mkboard size 100)
              rows (->> board (partition cols_n) (map vec) (into []))
              cols (apply map vector rows)]
          {:mana  {:id 0 :v 100}
           :board board
           :rows rows
           :cols cols})))

(defn pow [n e]
  (apply * (repeat e n)))
(defn powbend [n max e]
  (/ (pow n e) (pow max e)))

(defn hp->rgb [v]
  (apply utils/rgb->hexstr
    (utils/hsv->rgb
      (* 120 (powbend v 100 2))
      1
      (- 1 (/ (powbend v 100 4) 3.5)))))

(defn mana->rgb [v]
  (apply utils/rgb->hexstr
    (utils/hsv->rgb
      240
      0.75
      (+ 0.5 (/ v 200)))))

(defn click-handler [channel box ev]
  (do (put! channel [@box (condp = ev.button 0 :left 1 :mid 2 :right)])
      (.preventDefault ev)))

(defn view-box [box _owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [click]}]
      (dom/div #js {:className     "box"
                    :onClick       (partial click-handler click box)
                    :onContextMenu (partial click-handler click box)}
        (dom/span #js {:className "box-text"}
          (if (= 0 (:v box)) "XXX" (int (:v box))))
        (dom/div #js {:className "flex box-shim"}
          (dom/div #js {:className "box-hp"
                        :style     #js {"backgroundColor" (hp->rgb (:v box))
                                        "height"          (str (:v box) "%")}})
          )))))

(defn view-mana [box _owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "box"}
        #_(dom/span #js {:className "box-text"
                         :style     #js {:color "white"}}
            (int (:v box)))
        (dom/div #js {:className "flex box-shim"}
          (dom/div #js {:className "box-hp"
                        :style     #js {:backgroundColor (mana->rgb (:v box))
                                        :height          (str (:v box) "%")}})
          )))))

(defn heal [box v]
  (assoc-in box [:v] (max 0 (min 100 (+ (:v box) v)))))

(defn heal-in [boxes pred v]
  (vec (map #(if (pred %) (heal % v) %) boxes)))

(defn boxplus [a b]
  (let [[aid bid] [(:id a) (:id b)]
        m5 (mod aid 5)]
    (or (= bid aid)
        (= bid (- aid 5))
        (= bid (+ aid 5))
        (and (= bid (- aid 1)) (not= 1 m5))
        (and (= bid (+ aid 1)) (not= 0 m5)))))

(defn boxrow [a b]
  (let [[aid bid] [(:id a) (:id b)]
        c1 (- aid (mod (- aid 1) 5))]
    (some #{bid} (range c1 (+ c1 5)))))

(defn boxcol [a b]
  (let [[aid bid] [(:id a) (:id b)]
        d (utils/abs (- aid bid))]
    (= 0 (mod d 5))))

(def spells
  ;; aim for ~10 hp / mana
  {:single {:hp 27 :mana 9
            :pred (fn [center box] (= center box))}
   :row {:hp 20 :mana 15
         :pred (fn [center box] (boxrow center box))}
   :col {:hp 20 :mana 15
         :pred (fn [center box] (boxcol center box))}
   :plus {:hp 24 :mana 15
           :pred (fn [center box] (boxplus center box))}}
  )

(def binds
  {:left :col
   :mid :single
   :right :row})

(def mana_regen 4)
(def peril_scale 0.8)
(def perils_by_delay
  ; {freq_ms [pred(box) v]
  {500 [[#(>= 0.25 (rand)) 3]]  ; 1.5 dps * 25
   1500 [[#(>= 0.25 (rand)) 3]] ; 0.5 dps * 25
   3000 [[#(= (:id %) (+ 1 (rand-int 25))) 30]] ; 10 dps
   })

(defn cast [data box input]
  (let [{:keys [hp mana pred]} (get spells (get binds input))]
    (if (and (>= (:v (:mana @app-state)) mana)
          (> (:v box) 0))
      (do
        (om/transact! data :board
          (fn [xs] (heal-in xs (partial pred box) hp)))
        (om/transact! data :mana
          (fn [x] (heal x (- mana))))))))

(defn view-page [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:click (chan)
       :queue (chan)})
    om/IWillMount
    (will-mount [_]
      (do
        (go-loop []
          (<! (async/timeout 500))
          (om/transact! data :mana
            (fn [x] (heal x mana_regen)))
          (recur))
        (doseq [[ms perils] perils_by_delay]
          (go-loop []
            (<! (async/timeout ms))
            (doseq [[pred v] perils]
              (om/transact! data :board
                (fn [xs] (heal-in xs pred (- (* v peril_scale))))))
            (recur))
        )
        (let [click (om/get-state owner :click)]
          (go-loop []
            (let [[box input] (<! click)]
              (cast data box input)
              (recur))))
    ))
    om/IRenderState
    (render-state [_ {:keys [click]}]
      (dom/div nil
        (apply dom/div #js {:className "flex board"}
          (om/build-all view-box (:board data)
            {:init-state {:click click}}))
        (om/build view-mana (:mana data))
        ))))

(om/root
  (fn [data _owner]
    (reify om/IRender
      (render [_]
        (om/build view-page data))))
  app-state
  {:target (. js/document (getElementById "app"))})

; This seems to be necessary for figwheel to connect!
(defn on-js-reload []
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )