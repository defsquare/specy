(ns specy.infra.documentation
  (:require [spec-tools.json-schema :as json-schema]
            [hiccup.page :refer [include-js include-css]]
            [cheshire.core :as csc]
            [specy.infra.bus :refer [assets]]))


(defn asset->asset-doc [{:keys [name longname kind spec doc rely-on] :as asset}]
  (cond-> {:name     name
           :longname longname
           :kind     kind}
          doc (assoc :doc doc)
          rely-on (assoc :rely-on (some-> rely-on clojure.reflect/typename))
          spec (assoc :schema (json-schema/transform spec))))

(defn generate []
  (hiccup.core/html [:html
                     [:meta {:charset "utf-8"}]
                     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
                     [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@tailwindcss/ui@latest/dist/tailwind-ui.min.css"}]
                     [:body
                      [:div#app]
                      (include-js "js/app.js")
                      [:script {:type "text/javascript"}
                       (str "var context = "
                            (csc/encode (map asset->asset-doc (vals @assets)))
                            ";")
                       (str "cljs.pages.init(context);")]]]))