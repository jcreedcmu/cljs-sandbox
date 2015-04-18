(ns cljs-sandbox.core
    (:require [reagent.core :as reagent :refer [atom]]
              [cljsjs.react :as react]))

(defn without-nth [seq n]
  vec (concat (subvec seq 0 n) (subvec seq (+ 1 n))))

(def css-transition-group
  (reagent/adapt-react-class js/React.addons.CSSTransitionGroup))

(defn list-comp [items]
   [css-transition-group {:transition-name "foo"}
    (map-indexed (fn [i x] ^{:key x} [:li x]) @items)  ])

(defn main-comp []
  (let [items (atom ["A" "B" "C"])
        next-item (atom 0)
        adder (fn [position]
                (fn [e]
                  (do (swap! items #(vec (let [[bef aft] (split-at position %)]
                                           (concat bef [@next-item] aft))))
                      (swap! next-item inc))))
        deleter (fn [position]
                  (fn [e] (swap! items #(vec (without-nth % position)))))
        _ (js/setTimeout (adder 0) 300)
        _ (js/setTimeout (adder 2) 450)
        _ (js/setTimeout (adder 5) 900)
        _ (js/setTimeout (deleter 0) 1600)
        _ (js/setTimeout (deleter 2) 2200)
        _ (js/setTimeout (deleter 2) 2350)]
    [:div
     [:button {:on-click (adder 0)} "add"]
     [:button {:on-click (deleter 0)} "remove"]
     [:ul
      [list-comp items]]
     ]))

(defn main []
  [main-comp])

(defn mount-root []
  (reagent/render [main] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
