(ns lwjgl.advanced-lighting
  (:gen-class)
  (:require [clojure.string :as str]
            [lwjgl.advanced-opengl :as aogl]))

(def ^:private scenario->delegate
  {:advanced-lighting           :depth-testing
   :gamma-correction            :depth-testing-view
   :shadow-mapping-depth        :stencil-testing
   :shadow-mapping-base         :blending-discard
   :shadow-mapping              :blending-sort
   :point-shadows               :cubemaps-skybox
   :point-shadows-soft          :cubemaps-environment-mapping
   :csm                         :face-culling-exercise1
   :normal-mapping              :geometry-shader-normals
   :parallax-mapping            :instancing-quads
   :steep-parallax-mapping      :instancing-quads
   :parallax-occlusion-mapping  :instancing-quads
   :hdr                         :anti-aliasing-msaa
   :bloom                       :anti-aliasing-offscreen
   :deferred-shading            :framebuffers
   :deferred-shading-volumes    :framebuffers-exercise1
   :ssao                        :advanced-glsl-ubo})

(defn run!
  ([] (run! :advanced-lighting))
  ([scenario-key]
   (let [delegate (get scenario->delegate scenario-key :depth-testing)]
     (aogl/run-scene! delegate))))

(defn -main
  [& args]
  (let [scenario (some-> (first args)
                         (str/replace "_" "-")
                         (keyword))]
    (run! (or scenario :advanced-lighting))))
