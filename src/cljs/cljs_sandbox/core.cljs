(ns cljs-sandbox.core
  (:require [reagent.core :as reagent :refer [atom cursor]]
            [cljs.core.match :refer-macros [match]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [clojure.string :refer [join]]
            [cljsjs.react :as react]
            [cljs-sandbox.state :as state])
  (:import goog.History))

(enable-console-print!)
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

(defn init-dragger
  ([move-cb e1] (init-dragger move-cb (fn []) e1))
  ([move-cb up-cb e1]
   (let [x1 (.-pageX e1) y1 (.-pageY e1)]
     (aset js/document "onmousemove"
           (fn [e2]
             (let [x2 (.-pageX e2) y2 (.-pageY e2)]
               (move-cb x1 y1 x2 y2))))
     (aset js/document "onmouseup"
           (fn [e2]
             (let [x2 (.-pageX e2) y2 (.-pageY e2)]
               (up-cb x1 y1 x2 y2))
             (aset js/document "onmousemove" nil)
             (aset js/document "onmouseup" nil))))
   (.stopPropagation e1)
   (.preventDefault e1)))

(defn mover [rw]
  (let [orig @(:r rw)]
    (fn [x1 y1 x2 y2]
      ((:w rw) {:x (+ (:x orig) (- x2 x1))
                :y (+ (:y orig) (- y2 y1))})
      )))

(def point-color "#5af")
(def select-color "#aaa")
(def pre-color "#f3a")
(def post-color "#5af")

(def select-tool-color "#ffe")
(def background-color "#ffe")
(def pen-tool-color "#eef")


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


(defn transform-all-in-glyph!  [f glyph]
  (let [state-f #(vec (map (fn [path] (update-in path [:points] (fn [points] (vec (map f points))))) %))]
   (swap! glyph state-f)
    )
  )

(defn transform-all-in-path!  [f path]
  (let [state-f (fn [path] (update-in path [:points] (fn [points] (vec (map f points)))))]
   (swap! path state-f)
    )
  )

(defn deselect-all-in-glyph! [glyph]
  (transform-all-in-glyph! #(assoc % :selected false) glyph))

(defn select-all-in-path! [path]
  (transform-all-in-path! #(assoc % :selected true) path))

(defn make-rw [cell] {:r cell :w (partial reset! cell)})



(defn vlen [pt]
  (let [{:keys [x y]} pt]
    (.sqrt js/Math (+ (* x x) (* y y)))))

(defn stimes [s v]
  (let [{:keys [x y]} v]
    {:x (* s x) :y (* s y)}))

(defn normalize [v] (stimes (/ 1 (vlen v)) v))
(defn v- [{ vx :x vy :y} {wx :x wy :y }] {:x (- vx wx) :y (- vy wy)} )
(defn vproj [v w] (stimes (vlen v) (normalize w)))

(defn handle-rw [cell this other]
(let [free-motion (make-rw (cursor cell [this]))
      other-cell (cursor cell [other])
      other-length (vlen @other-cell)]
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

;(mover (make-rw ptcell))

(defn glyph-map [f glyph]
  (vec (for [path glyph] (update path :points (fn [points] (vec (for [point points] (f point))))))))

(defn big-mover [glyph]
  (let [orig @glyph]
    (fn [x1 y1 x2 y2]
      (let [move-by (fn [v] (if (:selected v) (update v :pt (fn [{:keys [x y]}] {:x (+ x (- x2 x1)) :y (+ y (- y2 y1))})) v))
            ident (fn [v] v)]
       (reset! glyph (glyph-map move-by orig))))))

(def last-click (atom 0))
(defn now [] (js* "0 + Date.now()"))
(defn set-now [] (reset! last-click (now)))
(defn recent? [] (let [recent (< (- (now) @last-click) 300)] (set-now) recent))

(defn movable-point [glyph path cell acell]
  (let [ptcell (cursor cell [:pt])
        pt @ptcell
        pre (get-pre acell)
        post (get-post @cell)
        selected (:selected @cell)
        common {
                :stroke point-color
                :fill (if selected point-color background-color)
                :stroke-width 1
                :on-mouse-down (fn [e]
                                 (if (recent?)
                                   (do (select-all-in-path! path)
                                       (init-dragger (big-mover glyph) e))
                                   (do
                                     (if (and (not selected) (not (.-shiftKey e)))
                                       (deselect-all-in-glyph! glyph))
                                     (swap! cell #(assoc % :selected (not (and selected (.-shiftKey e)))))
                                     (init-dragger (big-mover glyph) e)))
                                 (.preventDefault e)
                                 (.stopPropagation e))
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

(defn render-path [glyph path]
  (let [points        (cursor path [:points])
        ast           @points
        curve-length  (count ast)
        last          (- curve-length 1)
        pathspec      (join " "
                            (apply concat (for [ix  (range last) ;; replace last with curve-length to get closed
                                                jx [ (if (= ix last) 0 (inc ix))]]
                                            ^{:key [:line ix]} (curve-elts (get ast ix) (get ast jx) (= ix 0))
                                            )))]
    [:g
     [:path { :stroke point-color
             :stroke-width 1
             :fill "none"
             :d pathspec}]
     ;; perf debugging
;     (do (print (get @path :name)) [:g])
     (doall (map (fn [ix]
             ^{:key ix} [movable-point glyph path (cursor points [ix]) @(cursor points [ix])]
             ) (range (count ast))))]

    )
  )

(defn abs [x] (.abs js/Math x))

(defn pts-to-xywh [rect]
 {:x (+ 0.5 (min (:x1 rect) (:x2 rect)))
  :y (+ 0.5 (min (:y1 rect) (:y2 rect)))
  :width (abs (- (:x2 rect) (:x1 rect)))
  :height (abs (- (:y2 rect) (:y1 rect)))})

(defn pt-in-rect [pt rect]
  (let [[x1 y1 x2 y2] rect
        {:keys [x y]} pt]
    (and (>= x (min x1 x2)) (>= y (min y1 y2)) (<= x (max x1 x2)) (<= y (max y1 y2)))))

(defn init-selection-rect-dragger [selection-rect glyph e]
  (init-dragger (fn [x1 y1 x2 y2] (reset! selection-rect {:x1 x1 :x2 x2 :y1 y1 :y2 y2}))
                (fn [x1 y1 x2 y2]
                  (transform-all-in-glyph! #(assoc % :selected (pt-in-rect (:pt %) [x1 y1 x2 y2])) glyph)
                  (reset! selection-rect nil)) e))

;; takes a list, returns the unique non-nil element if any.
(defn unique [list] (match (reduce #(match [%1 %2]
                                           [nil x] x
                                     [x nil] x
                                     [_ _] :dup) nil list)
                           :dup nil
                           x x))

;; takes a points; if there is a unique selected endpoint, return a path to it, else nil
(defn points-stem [points] (let [num-selected (count (filter :selected points))
                                 last (- (count points) 1)]
                             (if (= 1 num-selected)
                               (cond
                                 (:selected (get points 0)) [0]
                                 (:selected (get points last)) [last]
                                 true nil))))

;; takes a glyph; if there is a unique selected point, return a path to it, else nil
(defn glyph-stem [glyph]
  (unique (map #(if-let [st (points-stem (get-in glyph [%1 :points]))] `[~%1 :points ~@st] ) (range (count glyph)))))


(defn init-pen-dragger [glyph e]
  (let [newvert {:type :corner
                 :pt {:x (.-pageX e) :y (.-pageY e)}
                 :pre {:x 0 :y 0}
                 :post {:x 0 :y 0} :selected true}]
    (match (glyph-stem @glyph)
           [n :points 0] (do (deselect-all-in-glyph! glyph)
                             (swap! (cursor glyph [n :points]) #(vec (concat [newvert] %))))
           [n :points m] (do (deselect-all-in-glyph! glyph)
                             (swap! (cursor glyph [n :points]) #(vec (concat % [newvert]))))
           _ (swap! glyph #(vec (concat % [{:name "new"
                                            :points [newvert]}]))))))


(defn display [selection-rect state]
  (let [paths (cursor state [:paths])
        mode  (:mode @state)]
    (aset js/document "onkeydown"
          (fn [e] (let [k (.-keyCode e)]
                    (print k)
                    (cond (= k 49) (transform-all-in-glyph!
                                    (fn [v] (if (:selected v) (assoc v :type :corner) v))
                                    paths))
                    (cond (= k 50) (transform-all-in-glyph!
                                    (fn [v] (if (:selected v) (let [{pre :pre post :post} v] (merge v {:pre (vproj pre (v- pre post))
                                                                                                       :post (vproj post (v- post pre)) :type :curve})) v))
                                    paths))
                    (cond (= k 86) (swap! state assoc :mode :select))
                    (cond (= k 67) (swap! state assoc :mode :pen)))))

    `[:div [:svg
            ~{:style {:background-color (case mode :select select-tool-color :pen pen-tool-color)}
              :height "100%" :width "100%"
              :on-mouse-down (fn [e] (case mode
                                       :select (init-selection-rect-dragger selection-rect paths e)
                                       :pen (init-pen-dragger paths e)))
              }
            ~(for [path-ix (range (count @paths))] ^{:key path-ix} [render-path paths (cursor paths [path-ix])])

            ~@(if-let [sr @selection-rect]
                [[:rect (merge (pts-to-xywh sr) {
                                                 :stroke-width 1 :stroke select-color :fill "rgba(0,0,0,0.05)"})]] [])
            ]]))

(defn home-page []
  [display
   (cursor state/state [:selection-rect])
   state/state])


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
