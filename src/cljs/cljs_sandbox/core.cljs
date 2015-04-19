(ns cljs-sandbox.core
  (:require [reagent.core :as reagent :refer [atom cursor]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [clojure.string :refer [join]]
            [cljsjs.react :as react]
            [cljs-sandbox.state :as state])
  (:import goog.History))

;; -------------------------
;; Views

(defn clog [x] (.log js/console (clj->js x)))
(defn jsclog [x] (.log js/console (.stringify js/JSON (clj->js x))))


(defn slider [atom]
  [:input {:value @atom :type :range :style {:width "500px"}
           :on-change #(reset! atom (-> % .-target .-value) )}])

(defn controls [state]
  [:div.panel [:div.controls
               [slider (cursor state [:radius])] [:br]
               [slider (cursor state [:x])] [:br]
               [slider (cursor state [:y])] [:br]
               [:input {:type "text" :on-change slider}]
               ]])

(defn init-dragger [f e]
  (let [fe (f (.-pageX e) (.-pageY e))]
    (aset js/document "onmousemove" fe)
    (aset js/document "onmouseup" #(aset js/document "onmousemove" nil)))
  )

(defn init-dragger-thunk [f]
  (fn [e]
    (init-dragger f e)))

(defn mover [rw]
  (let [orig @(:r rw)]
    (fn [x y]
      (fn [e] ((:w rw) {:x (+ (:x orig) (- (.-pageX e) x))
                        :y (+ (:y orig) (- (.-pageY e) y))})
        ))))

(def point-color "#5af")
(def pre-color "#f3a")
(def post-color "#5af")

(def background-color "#ffe")


(defn get-pre [node] {:x (+ (get-in node [:pt :x]) (get-in node [:pre :x]))
                      :y (+ (get-in node [:pt :y]) (get-in node [:pre :y]))})
(defn get-post [node] {:x (+ (get-in node [:pt :x]) (get-in node [:post :x]))
                       :y (+ (get-in node [:pt :y]) (get-in node [:post :y]))})

(defn node-handle [pt rw color]
  (let [size 3]
    [:g
     [:line {:x1 (+ size (:x pt)) :y1 (+ size (:y pt)) :x2 (- (:x pt) size) :y2 (- (:y pt) size) :style {:stroke-width 1 :stroke color}}]
     [:line {:x1 (- (:x pt) size) :y1 (+ size (:y pt)) :x2 (+ (:x pt) size) :y2 (- (:y pt) size) :style {:stroke-width 1 :stroke color}}]
     [:circle.vertex {:cx (:x pt) :cy (:y pt) :r 10
                      :fill "rgba(255,255,0,0.01)"
                      :on-mouse-down #(init-dragger (mover rw) %)}]]))

(defn screen-round [x]
  (+ 0.5 (.floor js/Math x)))


(defn deselect-all [state]
  (swap! state #(vec (for [pt %]  (assoc pt :selected false)))))

(defn make-rw [cell] {:r cell :w (partial reset! cell)})

(defn veclen [pt]
  (let [{:keys [x y]} pt]
    (.sqrt js/Math (+ (* x x) (* y y)))))

(defn stimes [s v]
  (let [{:keys [x y]} v]
    {:x (* s x) :y (* s y)}))

(defn normalize [v] (stimes (/ 1 (veclen v)) v))

(defn handle-rw [cell this other]
(let [free-motion (make-rw (cursor cell [this]))
      other-cell (cursor cell [other])
      other-length (veclen @other-cell)]
 (case (:type @cell)
   :corner free-motion
   :curve (update free-motion :w (fn [old-w] (fn [new-val]
                                               (reset! other-cell (stimes (- 0 other-length) (normalize new-val)))
                                               (old-w new-val))))))
)

;; cell is a derefable pointing to a structure like
;; {
;; :pt xy,
;; :pre xy,
;; :post xy,
;; :type [:curve|:corner],
;; }

(defn movable-point [selection cell]
  (let [ptcell (cursor cell [:pt])
        pt @ptcell
        pre (get-pre @cell)
        post (get-post @cell)
        selected (:selected @cell)
        common {
                :stroke point-color
                :fill (if selected point-color background-color)
                :stroke-width 1
                :on-mouse-down (fn [e]
                                 (if (and (not selected) (not (.-shiftKey e)))
                                   (deselect-all selection))
                                 (swap! cell #(assoc % :selected (not (and selected (.-shiftKey e)))))
                                 (init-dragger (mover (make-rw ptcell)) e))
                :cursor "pointer"
                }
        handles [[:line {:x1 (:x pt) :y1 (:y pt) :x2 (:x pre) :y2 (:y pre) :style {:stroke-width 1 :stroke pre-color}}]
                 [:line {:x1 (:x pt) :y1 (:y pt) :x2 (:x post) :y2 (:y post) :style {:stroke-width 1 :stroke post-color}}]
                 [node-handle  pre (handle-rw cell :pre :post) pre-color]
                 [node-handle post (handle-rw cell :post :pre) post-color]]]
    `[:g

      ;; maybe handles
      ~@(if selected handles [])

      ;; render the actual point
      ~@(case (:type @cell)
          :curve [[:circle.vertex (merge common {:cx (screen-round (:x pt)) :cy (screen-round (:y pt)) :r 4})]]
          :corner [[:rect.vertex (merge common {:x (screen-round (- (:x pt) 3.5)) :y (screen-round (- (:y pt) 3.5)) :width 7 :height 7})]]
          [])]))


(defn curve-elts [p q first]
  (let [p1 (:pt p)
        p2 (get-post p)
        p3 (get-pre q)
        p4 (:pt q)]
    (concat
     (if first ["M" (:x p1) (:y p1)] [] )
     ["C"  (:x p2)  (:y p2)
      (:x p3)  (:y p3)
      (:x p4) (:y p4)])))

(defn display [state]
  (let [ast          @state
        curve-length (count ast)
        last (- curve-length 1)
        pathspec (join " "
                       (apply concat (for [ix  (range curve-length)
                                           jx [ (if (= ix last) 0 (inc ix))]]
                                       ^{:key [:line ix]} (curve-elts (get ast ix) (get ast jx) (= ix 0))
                                       )))]
    [:svg {:style {:background-color background-color} :height "100%" :width "100%"}
     [:path { :stroke point-color
             :stroke-width 1
             :fill "#eef"
             :d pathspec}]

     (map (fn [ix]
            ^{:key ix} [movable-point state (cursor state [ix])]
            ) (range (count ast)))]))


(defn home-page []
  [:div
   [display state/state]
   ])


(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))



;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app



(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (mount-root))
