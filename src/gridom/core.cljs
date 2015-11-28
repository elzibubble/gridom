(ns gridom.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan <!]]
            [gridom.utils :as utils]))
(enable-console-print!)

(defn mkbox [[id _v]]
  {:id id
   :v  (* 4 id)})

(defn mkboard [size maxhp]
  (vec (map mkbox
         (zipmap (range 1 (+ size 1)) (repeat maxhp)))
    ))

; defonce to make this only initialize on hard reload
(def app-state
  (atom (let [rows 5
              cols 5
              size (* rows cols)
              maxhp 100]
          {:mana  {:id 0 :v 100}
           :rows  rows
           :cols  cols
           :size  size
           :maxhp maxhp
           :board (mkboard size maxhp)})))

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
  (do (put! channel [@box (condp = ev.button 0 10 1 0 2 -10)])
      (.preventDefault ev)))

(defn view-box [box _owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [click]}]
      (dom/div #js {:className     "box"
                    :onClick       (partial click-handler click box)
                    :onContextMenu (partial click-handler click box)}
        (dom/span #js {:className "box-text"}
          (:v box))
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
            (:v box))
        (dom/div #js {:className "flex box-shim"}
          (dom/div #js {:className "box-hp"
                        :style     #js {:backgroundColor (mana->rgb (:v box))
                                        :height          (str (:v box) "%")}})
          )))))

(defn heal [box v]
  (assoc-in box [:v] (max 0 (min 100 (+ (:v box) v)))))

(defn heal-in [boxes box v]
  (vec (map #(if (= % box) (heal % v) %) boxes)))

(defn view-page [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:click (chan)
       :tick (chan)})
    om/IWillMount
    (will-mount [_]
      (do
        (let [tick (om/get-state owner :tick)]
          (go-loop []
            (<! (async/timeout 200))
            (put! tick 0)
            (recur)))
        (let [tick (om/get-state owner :tick)]
          (go-loop []
            (<! tick)
            (om/transact! data :mana
              (fn [x] (heal x 1)))
            (recur)))
        (let [click (om/get-state owner :click)]
          (go-loop []
            (let [[box v] (<! click)]
              (if (>= (:v (:mana @app-state)) v)
                (do
                  (om/transact! data :board
                    (fn [xs] (vec (heal-in xs box v))))
                  (om/transact! data :mana
                    (fn [x] (heal x (- v))))))
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