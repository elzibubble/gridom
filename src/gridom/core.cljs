(ns gridom.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [gridom.utils :as utils]))
(enable-console-print!)

(defn mkboard [size maxhp]
  (vec (map (fn [i] {:id (first i)
                     :hp (* 4 (+ 1 (first i)))})
         (zipmap (range size) (repeat maxhp)))
    ))

; defonce to make this only initialize on hard reload
(def app-state
  (atom (let [rows 5
              cols 5
              size (* rows cols)
              maxhp 100]
          {:text  "Hello world."
           :rows  rows
           :cols  cols
           :size  size
           :maxhp maxhp
           :board (mkboard size maxhp)})))

(defn hp->rgb [hp]
  (apply utils/rgb->hexstr (utils/hsv->rgb (* 1.20 hp) 1 0.75)))

(defn view-box [box _owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [click]}]
      (dom/div #js {:className "box"}
        (dom/span #js {:className "box-text"
                       :onClick   (fn [_] (put! click @box))}
          (:hp box))
        (dom/div #js {:className "flex box-shim"}
          (dom/div #js {:className "box-guts"
                        :style     #js {"background-color" (hp->rgb (:hp box))
                                        "height"           (str (:hp box) "%")}})
          )))))

(defn heal [box v]
  (assoc-in box [:hp] (+ (:hp box) v)))

(defn heal-in [boxes box v]
  (vec (map #(if (= % box) (heal % v) %) boxes)))

(defn view-page [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:click (chan)})
    om/IWillMount
    (will-mount [_]
      (let [click (om/get-state owner :click)]
        (go (loop []
              (let [box (<! click)]
                (om/transact! data :board
                  (fn [xs] (vec (heal-in xs box 10))))
                (recur))))))
    om/IRenderState
    (render-state [_ {:keys [click]}]
      (apply dom/div #js {:className "flex board"}
        (om/build-all view-box (:board data)
          {:init-state {:click click}})))))

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