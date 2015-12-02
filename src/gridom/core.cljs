(ns gridom.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan <!]]
            [gridom.utils :as utils]
            [gridom.game :as game]))

(defn setup-timers [data owner]
  (go-loop []
    (<! (async/timeout 125))
    (om/transact! data
      (fn [x] (game/mana-gain x game/mana-regen)))
    (recur))
  (doseq [[ms codes] game/perils_by_delay]
    (go-loop []
      (<! (async/timeout ms))
      (doseq [code codes]
        (let [peril (get game/perils code)]
          (om/transact! data (partial (get peril :func) peril)))
        (om/transact! data :live
          (fn [_] (vec (map :id (filterv game/alive? (:board @game/app-state)))))))
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

(defn mouseenter-handler [box ev]
  )

(defn key-handler [channel box ev]
  (do (print ev)
      (put! channel [@box (condp = ev.button 0 :left 1 :mid 2 :right)])
      (.preventDefault ev)))

(defn view-box [box _owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [click]}]
      (dom/div #js {:className     "box"
                    :onMouseEnter  (partial mouseenter-handler box)
                    :onKeyDown     (partial key-handler click box)
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
      (dom/div #js {:className "box mana"}
        (dom/div #js {:className "flex box-shim"}
          (dom/div #js {:className "box-hp"
                        :style     #js {:backgroundColor (utils/mana->rgb v)
                                        :height          (str v "%")}})
          )))))

(defn code->key [code]
  (condp = code
    :left "LMB"
    :mid "MMB"
    :right "RMB"
    (name code)))

(defn view-spell [[spellcode binds] _owner]
  (reify
    om/IRender
    (render [_]
      (dom/p nil
        (let [spell (get game/spells spellcode)]
          (str (reduce (fn [a i] (+ (str a " / " i)))
                 (map #(code->key %) binds)) ": "
            ; (:name spell) ":"
            " restores " (:hp spell) " hp"
            " for " (Math/round (:mana spell)) " mana"))))))

(defn view-peril [code _owner]
  (reify
    om/IRender
    (render [_]
      (let [peril (get game/perils code)]
        (dom/li nil
          (str (:name peril) ": "
            (clojure.string/replace (:desc peril) "{:hp}" (:hp peril))
            ))))))

(defn view-perils [[millis codes] _owner]
  (reify
    om/IRender
    (render [_]
      (dom/span nil
        (str "Every " (/ millis 1000) " seconds: ")
        (apply dom/ul nil
          (om/build-all view-peril codes)
          )))))

(defn view-help [data _owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "help"}
        (om/build-all view-spell (utils/map-invert game/binds))
        (dom/br nil)
        (om/build-all view-perils (seq game/perils_by_delay))
      ))))

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
      (dom/div #js {:className "flex"
                    :style     #js {:justifyContent "flex-start"}}
        (apply dom/div #js {:className "flex board"}
          (om/build-all view-box (:board data)
            {:init-state {:click click}}))
        (om/build view-mana (:mana data))
        (om/build view-help data)
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