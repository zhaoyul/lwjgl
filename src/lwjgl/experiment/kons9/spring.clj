(ns lwjgl.experiment.kons9.spring)

(defn- spring-color
  [ratio]
  (let [ratio (max -1.0 (min 1.0 ratio))]
    (if (pos? ratio)
      [(+ 0.2 (* 0.8 ratio))
       (+ 0.8 (* -0.6 ratio))
       0.2]
      [0.2
       (+ 0.8 (* -0.6 (- ratio)))
       (+ 0.2 (* 0.8 (- ratio)))])))

(defn make-spring-grid
  "创建弹簧网格数据。"
  [rows cols spacing]
  (let [rows (max 2 (int rows))
        cols (max 2 (int cols))
        spacing (double spacing)
        half-w (* 0.5 spacing (dec cols))
        half-d (* 0.5 spacing (dec rows))
        idx (fn [r c] (+ (* r cols) c))
        nodes (vec
               (for [r (range rows)
                     c (range cols)]
                 (let [x (- (* c spacing) half-w)
                       z (- (* r spacing) half-d)
                       y 0.6
                       fixed? (= r 0)]
                   {:pos [(float x) (float y) (float z)]
                    :vel [0.0 0.0 0.0]
                    :fixed? fixed?})))
        springs (transient [])]
    (doseq [r (range rows)
            c (range cols)]
      (let [i (idx r c)]
        (when (< c (dec cols))
          (let [j (idx r (inc c))]
            (conj! springs [i j spacing])))
        (when (< r (dec rows))
          (let [j (idx (inc r) c)]
            (conj! springs [i j spacing])))
        (when (and (< r (dec rows)) (< c (dec cols)))
          (let [j (idx (inc r) (inc c))]
            (conj! springs [i j (* spacing (Math/sqrt 2.0))])))
        (when (and (< r (dec rows)) (> c 0))
          (let [j (idx (inc r) (dec c))]
            (conj! springs [i j (* spacing (Math/sqrt 2.0))])))))
    {:rows rows :cols cols :nodes nodes :springs (persistent! springs)}))

(defn update-spring-grid
  "根据弹簧力学更新节点位置。"
  [nodes springs dt]
  (let [k 8.0
        damping 0.4
        gravity [0.0 -1.4 0.0]
        forces (vec (repeat (count nodes) [0.0 0.0 0.0]))]
    (let [forces
          (reduce
           (fn [fs [i j rest]]
             (let [{:keys [pos]} (nth nodes i)
                   {pos2 :pos} (nth nodes j)
                   [x1 y1 z1] pos
                   [x2 y2 z2] pos2
                   dx (- x2 x1)
                   dy (- y2 y1)
                   dz (- z2 z1)
                   dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 1.0e-6))
                   diff (- dist rest)
                   f (* k diff)
                   fx (* f (/ dx dist))
                   fy (* f (/ dy dist))
                   fz (* f (/ dz dist))
                   [f1x f1y f1z] (nth fs i)
                   [f2x f2y f2z] (nth fs j)]
               (assoc fs
                      i [(+ f1x fx) (+ f1y fy) (+ f1z fz)]
                      j [(+ f2x (- fx)) (+ f2y (- fy)) (+ f2z (- fz))])))
           forces
           springs)]
      (mapv
       (fn [{:keys [pos vel fixed?] :as n} [fx fy fz]]
         (if fixed?
           (assoc n :vel [0.0 0.0 0.0])
           (let [[vx vy vz] vel
                 ax (+ fx (nth gravity 0) (* (- damping) vx))
                 ay (+ fy (nth gravity 1) (* (- damping) vy))
                 az (+ fz (nth gravity 2) (* (- damping) vz))
                 nvx (+ vx (* ax dt))
                 nvy (+ vy (* ay dt))
                 nvz (+ vz (* az dt))
                 [px py pz] pos
                 npx (+ px (* nvx dt))
                 npy (+ py (* nvy dt))
                 npz (+ pz (* nvz dt))]
             (assoc n :pos [(float npx) (float npy) (float npz)]
                    :vel [(float nvx) (float nvy) (float nvz)]))))
       nodes
       forces))))

(defn spring-lines
  "根据节点与弹簧生成线段顶点数据。"
  [nodes springs]
  (let [data (transient [])]
    (doseq [[i j rest] springs]
      (let [{:keys [pos]} (nth nodes i)
            {pos2 :pos} (nth nodes j)
            [x1 y1 z1] pos
            [x2 y2 z2] pos2
            dx (- x2 x1)
            dy (- y2 y1)
            dz (- z2 z1)
            dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 1.0e-6))
            ratio (/ (- dist rest) (max rest 1.0e-6))
            [r g b] (spring-color ratio)]
        (conj! data (float x1))
        (conj! data (float y1))
        (conj! data (float z1))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))
        (conj! data (float x2))
        (conj! data (float y2))
        (conj! data (float z2))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))))
    (persistent! data)))
