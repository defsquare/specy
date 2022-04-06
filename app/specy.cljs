(ns specy
  (:require
    [specy.registry :as sr]
    [specy.query :as query]
    [specy.command :as command]
    [specy.event :as event]
    [specy.entity :as entity]
    [specy.value :as value]
    ;        [specy.validation :as sv]
    [malli.registry :as mr]
    [malli.core :as m]
    [malli.core :as mc]
    [reagent.dom :as rdom])
  (:require-macros [specy.entity])
  )

;(mr/set-default-registry! registry)
#_(def registry*
    (atom (mc/default-schemas) #_{:string (m/-string-schema)
                                  :maybe  (m/-maybe-schema)
                                  :map    (m/-map-schema)}))

(mr/set-default-registry!
    (mr/mutable-registry sr/-specy-registry*))

(prn (query/defquery my-query [:map [:id string?]] {:doc "coucou query"}))
(prn (command/defcommand my-command [:map [:id string?]] {:doc "coucou command"}))
(prn (event/defevent my-event [:map [:id string?]] {:doc "coucou event"}))

(prn (entity/defentity coucou [:map [:id string?]] {:doc "lol"}))
(prn (value/defvalue my-value [:map [:id string?]] {:doc "lol"} (fn coucou [this] (prn "coucou"))))

(defn simple-component []
  [:div
   [:p "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red "] "text."]])

(defn ^:export render []
  (rdom/render [simple-component] (.-body js/document)))
