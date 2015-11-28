(ns gridom.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [gridom.utils :as utils]))
(enable-console-print!)

(defn mkboard [size maxhp]
  (vec (map (fn [i] {:id (first i)
                     :hp (* 4 (+ 1 (first i)))})
            (zipmap (range size) (repeat maxhp)))
       ))

; defonce to make this only initialize on hard reload
(def app-state (atom (let
                           [rows 5
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

(defn draw-box [box]
  (dom/div #js {:className "box"}
           (dom/span #js {:className "box-text"}
                     (:hp box))
           (dom/div #js {:className "flex box-shim"}
                    (dom/div #js {:className "box-guts"
                                  :style     #js {"background-color" (hp->rgb (:hp box))
                                                  "height" (str (:hp box) "%")}})
                    )))

(defn draw-board [board]
  (apply dom/div #js {:className "flex board"}
         (map (fn [box] (draw-box box))
              board)))

(defn draw [z]
  (draw-board (:board z)))

(om/root
  (fn [data _owner]
    (reify om/IRender
      (render [_]
        (draw data))))
  app-state
  {:target (. js/document (getElementById "app"))})

; This seems to be necessary for figwheel to connect!
(defn on-js-reload []
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )