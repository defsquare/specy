(ns infra.server
  (:require
    [clojure.java.io :as io]
    [mount.core :as mount :refer [defstate]]
    [yada.yada :refer [listener resource as-resource]]
    [specy.infra.documentation :as doc]
    [domain.amount]
    [domain.training]))

(defn start-server []
  (listener
    ["" [["/js/app.js" (as-resource (io/file "public/js/app.js"))] ;; TODO Move to a CDN
         ["/documentation"
          (resource
            {:methods
             {:get
              {:produces "text/html"
               :response (fn [_] (doc/generate))}}})]]]
    {:port 3000}))

(defstate aleph
          :start (start-server)
          :stop #(((:close aleph))))

(comment
  (mount/start)
  (mount/stop))