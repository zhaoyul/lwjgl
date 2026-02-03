(ns lwjgl.experiment.kons9.geometry
  (:require [lwjgl.experiment.kons9.math :as math]))

(defn uv-sphere
  "生成 UV 球体网格，返回 {:vertices float-array :indices int-array}。
  参数：
    lat-segs - 纬向分段数
    lon-segs - 经向分段数"
  [lat-segs lon-segs]
  (let [lat-segs (max 3 (int lat-segs))
        lon-segs (max 3 (int lon-segs))
        vertex-count (* (inc lat-segs) (inc lon-segs))
        vertices (transient [])
        indices (transient [])]
    (dotimes [i (inc lat-segs)]
      (let [v (/ (double i) lat-segs)
            phi (* Math/PI v)
            y (Math/cos phi)
            r (Math/sin phi)]
        (dotimes [j (inc lon-segs)]
          (let [u (/ (double j) lon-segs)
                theta (* 2.0 Math/PI u)
                x (* r (Math/cos theta))
                z (* r (Math/sin theta))
                nx x
                ny y
                nz z]
            (conj! vertices (float x))
            (conj! vertices (float y))
            (conj! vertices (float z))
            (conj! vertices (float nx))
            (conj! vertices (float ny))
            (conj! vertices (float nz))))))
    (dotimes [i lat-segs]
      (dotimes [j lon-segs]
        (let [row-a (* i (inc lon-segs))
              row-b (* (inc i) (inc lon-segs))
              a (+ row-a j)
              b (+ row-b j)
              c (+ row-b (inc j))
              d (+ row-a (inc j))]
          (conj! indices a)
          (conj! indices b)
          (conj! indices d)
          (conj! indices b)
          (conj! indices c)
          (conj! indices d))))
    {:vertices (float-array (persistent! vertices))
     :indices (int-array (persistent! indices))
     :vertex-count vertex-count}))

(defn make-point-cloud
  "创建点云数据。传入 points 与可选 colors。
  points 为 [[x y z] ...]。
  colors 为 [[r g b] ...]，若不足则使用白色。"
  [points & {:keys [colors]}]
  {:points (vec points)
   :colors (vec (or colors []))})

(defn sphere-point-cloud
  "生成球面点云。"
  [count radius]
  (let [count (max 1 (int count))
        radius (double radius)]
    (vec
     (repeatedly count
                 (fn []
                   (let [u (rand)
                         v (rand)
                         theta (* 2.0 Math/PI u)
                         phi (Math/acos (- 1.0 (* 2.0 v)))
                         x (* radius (Math/sin phi) (Math/cos theta))
                         y (* radius (Math/cos phi))
                         z (* radius (Math/sin phi) (Math/sin theta))]
                     [(float x) (float y) (float z)]))))))

(defn sweep-mesh
  "生成沿 X 轴扫掠的管状网格。返回 {:vertices float-array :indices int-array}。
  参数：
    :segments  路径分段
    :ring      环形分段
    :length    路径长度
    :radius    基础半径
    :amp       路径摆动幅度
    :freq      摆动频率
    :twist     扭转角度（度）"
  [& {:keys [segments ring length radius amp freq twist]
      :or {segments 80 ring 16 length 3.0 radius 0.25 amp 0.35 freq 1.5 twist 0.0}}]
  (let [segments (max 3 (int segments))
        ring (max 6 (int ring))
        length (double length)
        radius (double radius)
        amp (double amp)
        freq (double freq)
        twist (Math/toRadians (double twist))
        vertices (transient [])
        indices (transient [])]
    (dotimes [i (inc segments)]
      (let [t (/ (double i) segments)
            x (- (* t length) (* 0.5 length))
            phase (* 2.0 Math/PI freq t)
            y (* amp (Math/sin phase))
            z (* (* 0.5 amp) (Math/cos phase))
            taper (+ 0.85 (* 0.25 (Math/sin (* 2.0 Math/PI t))))
            r (* radius taper)
            twist-angle (* twist t)]
        (dotimes [j (inc ring)]
          (let [u (/ (double j) ring)
                theta (+ (* 2.0 Math/PI u) twist-angle)
                cy (Math/cos theta)
                sy (Math/sin theta)
                px x
                py (+ y (* r cy))
                pz (+ z (* r sy))
                nx 0.0
                ny cy
                nz sy]
            (conj! vertices (float px))
            (conj! vertices (float py))
            (conj! vertices (float pz))
            (conj! vertices (float nx))
            (conj! vertices (float ny))
            (conj! vertices (float nz))))))
    (dotimes [i segments]
      (dotimes [j ring]
        (let [row-a (* i (inc ring))
              row-b (* (inc i) (inc ring))
              a (+ row-a j)
              b (+ row-b j)
              c (+ row-b (inc j))
              d (+ row-a (inc j))]
          (conj! indices a)
          (conj! indices b)
          (conj! indices d)
          (conj! indices b)
          (conj! indices c)
          (conj! indices d))))
    {:vertices (float-array (persistent! vertices))
     :indices (int-array (persistent! indices))}))
