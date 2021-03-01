(ns specy.infra.documentation
  (:require                                                 ;[spec-tools.json-schema :as json-schema]
    [hiccup.page :refer [include-js include-css]]
    [cheshire.core :as csc]
    [specy.infra.bus :refer [assets]]
    [malli.json-schema :as json-schema]
    [specy.infra.repository :refer [building-blocks]]
    [specy.protocols :refer [all]]))

;(defmethod json-schema/accept-spec 'specy.time/duration? [_ _ _ _] {:type "java.time.Duration"})
;(defmethod json-schema/accept-spec 'domain.amount/Currency? [_ _ _ _] {:type "domain.amount.Currency"})
;(defmethod json-schema/accept-spec 'domain.amount/Amount? [_ _ _ _] {:type "domain.amount.Amount"})

(comment
  (remove-ns 'specy.infra.documentation)
  (json-schema/transform (:schema (get @assets :domain.training/training))))

(defn asset->asset-doc [{:keys [name longname kind schema doc ns rely-on] :as asset}]
  (cond-> {:name     name
           :longname longname
           :kind     kind
           :ns       ns}
          doc (assoc :doc doc)
          rely-on (assoc :rely-on (some-> rely-on clojure.reflect/typename))
          schema (assoc :schema (json-schema/transform schema))))

(defn generate []
  (hiccup.core/html [:html
                     [:meta {:charset "utf-8"}]
                     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
                     [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@tailwindcss/ui@latest/dist/tailwind-ui.min.css"}]
                     [:body
                      [:div#app]
                      [:script {:src "https://cdn.jsdelivr.net/gh/defsquare/specy-documentation-ui/dist/js/app.js"}]
                      [:script {:type "text/javascript"}
                       (str "var context = "
                            (csc/encode (map asset->asset-doc (vals (all building-blocks))))
                            ";")
                       (str "specy.documentation.core.init(context);")]]]))

