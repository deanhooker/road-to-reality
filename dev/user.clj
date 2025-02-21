(ns user
  (:require [emmy.clerk :as ec]))

(def index
  "notebooks/math/1_series.clj")

(def defaults
  {;; NOTE by default, your project's first notebook is registered as its index;
   ;; this means that static builds will populate `index.html` with this
   ;; notebook.
   ;;
   ;; Comment out the following line to tell Clerk to generate its own index
   ;; with a list of all built pages.
   :index index
   })

(def serve-defaults
  (assoc defaults
         :port 7777
         :watch-paths ["notebooks"]
         :browse? true))

(def static-defaults
  (assoc defaults
         :browse? false
         :paths ["notebooks/**.clj"]
         :cname ""
         ;; :git/url "https://github.com/myusername/my-emmy-project"
         ))

(defn serve!
  "Alias of [[emmy.clerk/serve!]] with [[defaults]] supplied as
  default arguments.

  Any supplied `opts` overrides the defaults."
  ([] (serve! {}))
  ([opts]
   (ec/serve!
    (merge serve-defaults opts))))

(def ^{:doc "Alias for [[emmy.clerk/halt!]]."}
  halt!
  ec/halt!)

(defn build!
  "Alias of [[emmy.clerk/build!]] with [[static-defaults]] supplied
  as default arguments.

  Any supplied `opts` overrides the defaults."
  [opts]
  (ec/build!
   (merge static-defaults opts)))

(mentat.clerk-utils.css/set-css!
 "https://unpkg.com/mafs@0.18.8/core.css"
 "https://unpkg.com/computer-modern@0.1.2/cmu-serif.css"
 "https://unpkg.com/mafs@0.18.8/font.css")
