(ns lwjgl.experiment.kons9.ui
  "kons-9 的轻量 UI 数据结构与布局计算。"
  (:require [clojure.string :as str]
            [lwjgl.core :as core])
  (:import (org.lwjgl.glfw GLFW)))

(def ^:const menu-margin 16)
(def ^:const menu-padding 8)
(def ^:const menu-item-height 26)
(def ^:const menu-title-height 28)
(def ^:const menu-key-gap 10)
(def ^:const menu-label-gap 6)
(def ^:const menu-border-width 1)

(def ^:const button-height 28)
(def ^:const button-padding 12)
(def ^:const button-spacing 8)
(def ^:const button-min-width 88)
(def ^:const button-border-width 1)

(def ^:const info-padding 10)
(def ^:const info-line-height 18)
(def ^:const info-max-width 420)

(def ^:const font-px-height (* core/font-height core/font-scale))
(def ^:const text-baseline-offset 2)

(def ^:const theme
  {:menu-bg [0.95 0.95 0.96 0.92]
   :menu-title [0.22 0.23 0.26 0.92]
   :menu-item [0.88 0.89 0.92 0.92]
   :menu-hover [0.72 0.82 0.98 0.95]
   :menu-border [0.2 0.2 0.22 0.95]
   :button-bg [0.93 0.93 0.95 0.92]
   :button-hover [0.7 0.82 1.0 0.95]
   :button-border [0.2 0.2 0.22 0.95]
   :text [0.08 0.08 0.1]})

(defonce ui-state
  (atom {:menu {:visible? true
                :origin [20 20]
                :stack []
                :hover nil}
         :buttons {:items []
                   :dock :top-right
                   :hover nil}
         :info {:visible? false
                :title "Info"
                :lines []
                :dock :bottom-left}
         :layout {:menu nil
                  :buttons nil
                  :info nil}}))

(defn make-command-table
  "创建命令表。entries 为命令条目向量。"
  [title entries]
  {:title title
   :entries (vec entries)})

(defn command-entry
  "创建命令条目。"
  [key label action]
  {:key key
   :label label
   :action action})

(defn subtable-entry
  "创建子菜单条目。"
  [key label table]
  {:key key
   :label label
   :submenu table})

(defn set-menu!
  "设置根菜单。"
  [table]
  (swap! ui-state assoc-in [:menu :stack] (if table [table] []))
  (swap! ui-state assoc-in [:menu :hover] nil)
  table)

(defn set-buttons!
  "设置按钮列表，每个按钮包含 :id :label :action。"
  [buttons]
  (swap! ui-state assoc-in [:buttons :items] (vec buttons))
  (swap! ui-state assoc-in [:buttons :hover] nil)
  buttons)

(defn show-menu!
  "显示菜单。"
  []
  (swap! ui-state assoc-in [:menu :visible?] true)
  true)

(defn hide-menu!
  "隐藏菜单。"
  []
  (swap! ui-state assoc-in [:menu :visible?] false)
  true)

(defn toggle-menu!
  "切换菜单显示状态。"
  []
  (swap! ui-state update-in [:menu :visible?] not)
  true)

(defn info-visible?
  "判断信息面板是否可见。"
  []
  (get-in @ui-state [:info :visible?]))

(defn set-info!
  "设置信息面板内容并显示。"
  [title lines]
  (swap! ui-state assoc :info {:visible? true
                               :title title
                               :lines (vec lines)
                               :dock (get-in @ui-state [:info :dock])})
  true)

(defn set-info-lines!
  "更新信息面板内容。"
  [lines]
  (swap! ui-state assoc-in [:info :lines] (vec lines))
  true)

(defn clear-info!
  "隐藏信息面板。"
  []
  (swap! ui-state assoc-in [:info :visible?] false)
  true)

(defn toggle-info!
  "切换信息面板显示状态。"
  []
  (swap! ui-state update-in [:info :visible?] not)
  true)

(defn menu-visible?
  "判断菜单是否可见。"
  []
  (get-in @ui-state [:menu :visible?]))

(defn- text-width
  [s]
  (if (seq s)
    (:w (core/text-size [(str s)]))
    0))

(defn- text-baseline
  [y h]
  (+ y (int (/ (- h font-px-height) 2)) text-baseline-offset))

(defn- label->text
  [label]
  (cond
    (string? label) label
    (fn? label) (str (label))
    (keyword? label) (-> label name)
    (nil? label) ""
    :else (str label)))

(defn- key-label
  [key]
  (cond
    (and (int? key) (<= GLFW/GLFW_KEY_0 key GLFW/GLFW_KEY_9))
    (str (- key GLFW/GLFW_KEY_0))

    (and (int? key) (<= GLFW/GLFW_KEY_A key GLFW/GLFW_KEY_Z))
    (str (char (+ (int \A) (- key GLFW/GLFW_KEY_A))))

    (= key GLFW/GLFW_KEY_SPACE) "SP"
    (= key GLFW/GLFW_KEY_BACKSPACE) "BS"
    (= key GLFW/GLFW_KEY_GRAVE_ACCENT) "`"
    (= key GLFW/GLFW_KEY_TAB) "TAB"
    (= key GLFW/GLFW_KEY_ENTER) "ENTER"
    (= key GLFW/GLFW_KEY_ESCAPE) "ESC"
    :else ""))

(defn- active-table
  [state]
  (peek (get-in state [:menu :stack])))

(defn- find-entry
  [table key]
  (some (fn [entry]
          (when (= key (:key entry))
            entry))
        (:entries table)))

(defn- clamp-origin
  [[x y] [w h] win-w win-h]
  [(-> x (max menu-margin) (min (max menu-margin (- win-w w menu-margin))))
   (-> y (max menu-margin) (min (max menu-margin (- win-h h menu-margin))))])

(defn- build-menu-layout
  [menu win-w win-h]
  (let [table (active-table {:menu menu})]
    (when table
      (let [entries (:entries table)
            key-labels (map (comp key-label :key) entries)
            label-texts (map (comp label->text :label) entries)
            key-width (apply max 0 (map text-width key-labels))
            label-width (apply max 0 (map text-width label-texts))
            width (max 160
                       (+ (* 2 menu-padding)
                          key-width
                          (if (pos? key-width) menu-key-gap 0)
                          label-width))
            height (+ menu-title-height (* menu-item-height (count entries)))
            [ox oy] (clamp-origin (:origin menu) [width height] win-w win-h)
            items (vec
                   (map-indexed
                    (fn [idx entry]
                      (let [item-y (+ oy menu-title-height (* idx menu-item-height))]
                        {:entry entry
                         :x ox
                         :y item-y
                         :w width
                         :h menu-item-height
                         :key-text (key-label (:key entry))
                         :label-text (label->text (:label entry))}))
                    entries))]
        {:panel {:x ox :y oy :w width :h height}
         :title {:x ox :y oy :w width :h menu-title-height
                 :text (label->text (:title table))}
         :key-width key-width
         :items items}))))

(defn- button-width
  [label]
  (max button-min-width
       (+ (* 2 button-padding) (text-width (label->text label)))))

(defn- build-button-layout
  [buttons win-w win-h]
  (let [items (map (fn [btn]
                     (assoc btn :w (button-width (:label btn)) :h button-height))
                   buttons)
        total-w (reduce + 0 (map :w items))
        total-w (+ total-w (* button-spacing (max 0 (dec (count items)))))
        dock (get-in @ui-state [:buttons :dock])
        margin menu-margin
        x0 (case dock
             :top-left margin
             :bottom-left margin
             :bottom-right (- win-w margin total-w)
             :top-right (- win-w margin total-w)
             margin)
        y0 (case dock
             :top-left margin
             :top-right margin
             :bottom-left (- win-h margin button-height)
             :bottom-right (- win-h margin button-height)
             margin)
        items (loop [acc []
                     x x0
                     remaining (seq items)]
                (if (seq remaining)
                  (let [btn (first remaining)
                        item (assoc btn :x x :y y0)]
                    (recur (conj acc item)
                           (+ x (:w item) button-spacing)
                           (next remaining)))
                  acc))]
    {:items items}))

(defn- build-info-layout
  [info win-w win-h]
  (let [{:keys [title lines dock]} info
        lines (vec (or lines []))
        title (when (seq (str title)) (str title))
        rows (cond-> []
               title (conj title)
               (seq lines) (into lines))
        widths (map text-width rows)
        width (-> (if (seq widths) (apply max widths) 0)
                  (+ (* 2 info-padding))
                  (min info-max-width)
                  (max 160))
        height (+ (* (count rows) info-line-height) (* 2 info-padding))
        x (case dock
            :top-left menu-margin
            :top-right (- win-w menu-margin width)
            :bottom-right (- win-w menu-margin width)
            :bottom-left menu-margin
            menu-margin)
        y (case dock
            :top-left menu-margin
            :top-right menu-margin
            :bottom-right (- win-h menu-margin height)
            :bottom-left (- win-h menu-margin height)
            menu-margin)]
    {:panel {:x x :y y :w width :h height}
     :rows rows}))

(defn update-layout!
  "更新 UI 布局，返回最新布局。"
  [win-w win-h]
  (let [valid? (and (number? win-w) (number? win-h) (pos? win-w) (pos? win-h))]
    (if (not valid?)
      (:layout @ui-state)
      (let [state @ui-state
            menu (get state :menu)
            menu-layout (when (and (:visible? menu) (active-table state))
                          (build-menu-layout menu win-w win-h))
            buttons-layout (build-button-layout (get-in state [:buttons :items]) win-w win-h)]
        (swap! ui-state assoc :layout {:menu menu-layout
                                       :buttons buttons-layout})
        (:layout @ui-state)))))

(defn- inside?
  [px py {:keys [x y w h]}]
  (and (<= px (+ x w))
       (<= py (+ y h))
       (>= px x)
       (>= py y)))

(defn- hit-menu-item
  [layout x y]
  (when-let [items (get-in layout [:menu :items])]
    (first (keep-indexed
            (fn [idx item]
              (when (inside? x y item)
                {:index idx :item item}))
            items))))

(defn- hit-button
  [layout x y]
  (when-let [items (get-in layout [:buttons :items])]
    (first (filter #(inside? x y %) items))))

(defn handle-mouse-move!
  "更新鼠标悬停状态。"
  [x y win-w win-h]
  (let [layout (update-layout! win-w win-h)]
    (if-not layout
      false
      (let [menu-hit (hit-menu-item layout x y)
            button-hit (hit-button layout x y)]
        (swap! ui-state assoc-in [:menu :hover] (:index menu-hit))
        (swap! ui-state assoc-in [:buttons :hover] (:id button-hit))
        (boolean (or menu-hit button-hit))))))

(defn handle-mouse-press!
  "处理鼠标按下事件，返回是否命中 UI。"
  [x y button action win-w win-h]
  (if (and (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
           (= action GLFW/GLFW_PRESS))
    (let [layout (update-layout! win-w win-h)]
      (if-not layout
        false
        (let [menu-hit (hit-menu-item layout x y)
              button-hit (hit-button layout x y)]
          (cond
            menu-hit
            (let [{:keys [entry]} (:item menu-hit)]
              (if-let [submenu (:submenu entry)]
                (do
                  (swap! ui-state update-in [:menu :stack] conj submenu)
                  (show-menu!))
                (when-let [f (:action entry)]
                  (f)))
              true)

            button-hit
            (do
              (when-let [f (:action button-hit)]
                (f))
              true)

            :else false))))
    false))

(defn handle-key!
  "处理菜单相关键盘输入，返回是否消费事件。"
  [key action]
  (let [state @ui-state
        menu (:menu state)]
    (cond
      (not= action GLFW/GLFW_PRESS) false

      (= key GLFW/GLFW_KEY_TAB)
      (do
        (if (and (:visible? menu) (> (count (:stack menu)) 1))
          (swap! ui-state assoc-in [:menu :stack] [(first (:stack menu))])
          (toggle-menu!))
        true)

      (= key GLFW/GLFW_KEY_LEFT)
      (if (and (:visible? menu) (> (count (:stack menu)) 1))
        (do
          (swap! ui-state update-in [:menu :stack] pop)
          true)
        false)

      :else
      (if (and (:visible? menu) (active-table state))
        (when-let [entry (find-entry (active-table state) key)]
          (if-let [submenu (:submenu entry)]
            (do
              (swap! ui-state update-in [:menu :stack] conj submenu)
              (show-menu!)
              true)
            (do
              (when-let [f (:action entry)]
                (f))
              true)))
        false))))

(defn draw-commands
  "生成绘制指令。"
  [win-w win-h]
  (let [state @ui-state
        layout (update-layout! win-w win-h)
        menu-layout (:menu layout)
        menu-hover (get-in state [:menu :hover])
        button-hover (get-in state [:buttons :hover])
        rects (transient [])
        texts (transient [])]
    (when menu-layout
      (let [{:keys [panel title items key-width]} menu-layout
            {:keys [menu-bg menu-title menu-item menu-border text] :as theme} theme
            menu-hover-color (:menu-hover theme)]
        (conj! rects {:x (:x panel) :y (:y panel) :w (:w panel) :h (:h panel) :color menu-border})
        (conj! rects {:x (+ (:x panel) menu-border-width)
                      :y (+ (:y panel) menu-border-width)
                      :w (- (:w panel) (* 2 menu-border-width))
                      :h (- (:h panel) (* 2 menu-border-width))
                      :color menu-bg})
        (conj! rects {:x (:x title) :y (:y title) :w (:w title) :h (:h title) :color menu-title})
        (conj! texts {:x (+ (:x title) menu-padding)
                      :y (text-baseline (:y title) (:h title))
                      :text (label->text (:text title))})
        (doseq [[idx item] (map-indexed vector items)]
          (let [hover? (= idx menu-hover)
                bg (if hover? menu-hover-color menu-item)
                ky (:key-text item)
                label (:label-text item)
                key-x (+ (:x item) menu-padding)
                label-x (+ (:x item) menu-padding (if (pos? key-width) (+ key-width menu-key-gap) 0))
                text-y (text-baseline (:y item) (:h item))]
            (conj! rects {:x (:x item) :y (:y item) :w (:w item) :h (:h item) :color bg})
            (when (seq ky)
              (conj! texts {:x key-x :y text-y :text ky}))
            (conj! texts {:x label-x :y text-y :text (label->text label)})))))

    (let [{:keys [button-bg button-border text] :as theme} theme
          button-hover-color (:button-hover theme)]
      (doseq [item (get-in layout [:buttons :items])]
        (let [hover? (= (:id item) button-hover)
              bg (if hover? button-hover-color button-bg)
              label (label->text (:label item))
              text-x (+ (:x item) (int (/ (- (:w item) (text-width label)) 2)))
              text-y (text-baseline (:y item) (:h item))]
          (conj! rects {:x (:x item) :y (:y item) :w (:w item) :h (:h item) :color button-border})
          (conj! rects {:x (+ (:x item) button-border-width)
                        :y (+ (:y item) button-border-width)
                        :w (- (:w item) (* 2 button-border-width))
                        :h (- (:h item) (* 2 button-border-width))
                        :color bg})
          (conj! texts {:x text-x :y text-y :text label}))))

    (when (get-in state [:info :visible?])
      (let [{:keys [menu-bg menu-border text] :as theme} theme
            info-layout (build-info-layout (get state :info) win-w win-h)]
        (when info-layout
          (let [{:keys [panel rows]} info-layout
                {:keys [x y w h]} panel
                rows (vec rows)]
            (conj! rects {:x x :y y :w w :h h :color menu-border})
            (conj! rects {:x (+ x menu-border-width)
                          :y (+ y menu-border-width)
                          :w (- w (* 2 menu-border-width))
                          :h (- h (* 2 menu-border-width))
                          :color menu-bg})
            (doseq [[idx row] (map-indexed vector rows)]
              (let [ry (+ y info-padding (* idx info-line-height))
                    text-y (text-baseline ry info-line-height)]
                (conj! texts {:x (+ x info-padding)
                              :y text-y
                              :text (label->text row)})))))))

    {:rects (persistent! rects)
     :texts (persistent! texts)
     :text-color (:text theme)}))

(defn format-title
  "将关键字转换为标题字符串。"
  [k]
  (->> (str/split (name k) #"-")
       (map str/capitalize)
       (str/join " ")))
