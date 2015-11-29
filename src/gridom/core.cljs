(ns gridom.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan <!]]
            [gridom.utils :as utils]
            [gridom.game :as game]))

(defn setup-timers [data owner]
  (go-loop []
    (<! (async/timeout 500))
    (om/transact! data
      (fn [x] (game/mana-gain x game/mana-regen)))
    (recur))
  (doseq [[ms perils] game/perils_by_delay]
    (go-loop []
      (<! (async/timeout ms))
      (doseq [[pred v] perils]
        (om/transact! data :board
          (fn [xs] (game/heal-in xs pred (- (* v game/peril-scale))))))
      (recur))
    )
  (let [click (om/get-state owner :click)]
    (go-loop []
      (let [[box input] (<! click)]
        (game/cast data box input)
        (recur)))))

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
                        :style     #js {"backgroundColor" (utils/hp->rgb (:v box))
                                        "height"          (str (:v box) "%")}})
          )))))

(defn view-mana [v _owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "box"}
        (dom/div #js {:className "flex box-shim"}
          (dom/div #js {:className "box-hp"
                        :style     #js {:backgroundColor (utils/mana->rgb v)
                                        :height          (str v "%")}})
          )))))

(defn view-page [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:click (chan)
       :queue (chan)})
    om/IWillMount
    (will-mount [_]
      (setup-timers data owner))
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
  game/app-state
  {:target (. js/document (getElementById "app"))})

; This seems to be necessary for figwheel to connect!
(defn on-js-reload []
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )