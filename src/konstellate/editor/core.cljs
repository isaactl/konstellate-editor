(ns konstellate.editor.core
  (:require 
    recurrent.drivers.vdom
    recurrent.drivers.http
    cljsjs.js-yaml
    cljsjs.filesaverjs
    [cljs.reader :refer [read-string]]
    [clojure.string :as string]
    [recurrent.core :as recurrent :include-macros true]
    [recurrent.state :as state]
    [konstellate.editor.components :as components]
    [ulmus.keyboard :as keyboard]
    [ulmus.signal :as ulmus]))


(declare start!)

(defonce style-scope (gensym))

(defn clj->yaml
  [x]
  (js/jsyaml.safeDump (clj->js x)))

(defn yaml->clj
  [x]
  (js->clj
    (js/jsyaml.safeLoad x)
    :keywordize-keys true))

(defn KindPicker
  [props sources]
  (let [definitions-$
        (ulmus/map (fn [swagger-text]
                     (:definitions (js->clj (.parse js/JSON swagger-text)
                                            :keywordize-keys true)))
                   ((:swagger-$ sources) [:get]))
        key-picker (components/KeyPicker 
                     {:action "Create"
                      :heading "What type of resource do you want to create?"
                      :single? true
                      :limit [:io.k8s.api.core.v1.Container
                              :io.k8s.api.batch.v1beta1.CronJob
                              :io.k8s.api.apps.v1.DaemonSet
                              :io.k8s.api.apps.v1.Deployment
                              :io.k8s.api.batch.v1.Job
                              :io.k8s.api.core.v1.Pod
                              :io.k8s.api.apps.v1.ReplicaSet
                              :io.k8s.api.apps.v1.ReplicationController
                              :io.k8s.api.apps.v1.StatefulSet
                              :io.k8s.api.core.v1.Endpoints
                              :io.k8s.api.extensions.v1beta1.Ingress
                              :io.k8s.api.core.v1.Service
                              :io.k8s.api.core.v1.ConfigMap
                              :io.k8s.api.core.v1.Secret
                              :io.k8s.api.core.v1.PsersistentVolumeClaim
                              :io.k8s.api.storage.v1.StorageClass
                              :io.k8s.api.core.v1.Volume
                              :io.k8s.api.storage.v1beta1.VolumeAttachment]}
                     {:definitions-$ definitions-$
                      :hidden-keys-$ (ulmus/signal-of [])
                      :recurrent/dom-$ (:recurrent/dom-$ sources)})]
    (assoc 
      key-picker
      :swagger-$ (ulmus/signal-of [:get]))))


(defn Editor 
  [props sources]
  (let [kind-name (last (string/split (str (name (:kind props))) "."))
        api-version (-> (name (:kind props))
                        (string/replace "io.k8s.api." "")
                        (string/replace (str "." kind-name) "")
                        (string/replace "." "/")
                        (string/replace "core/" ""))
        hovered-editor-$
        (ulmus/distinct
          (ulmus/map (fn [e]
                       (.stopPropagation e)
                       (.getAttribute (.-currentTarget e) "data-path"))
                     (ulmus/merge
                       ((:recurrent/dom-$ sources) ".object-editor" "mouseover")
                       ((:recurrent/dom-$ sources) ".editor" "mouseover")
                       ((:recurrent/dom-$ sources) ".array" "mouseover"))))
        definitions-$
        (ulmus/map (fn [swagger-text]
                     (:definitions (js->clj (.parse js/JSON swagger-text)
                                            :keywordize-keys true)))
                   ((:swagger-$ sources) [:get]))
        editor (components/Editor {:kind (:kind props)}
                                  {:definitions-$ definitions-$
                                   :hovered-editor-$ hovered-editor-$
                                   :recurrent/state-$ (:recurrent/state-$ sources)
                                   :recurrent/dom-$ (:recurrent/dom-$ sources)})

        initial-state {:kind kind-name
                       :apiVersion api-version}
        text-area-state-$ 
        (ulmus/map
          (fn [e]
            (let [parsed (yaml->clj (.-value (.-target e)))]
              (cond
                (empty? (.-value (.-target e))) (fn [] initial-state)
                parsed (fn [] parsed)
                :else identity)))
          (ulmus/merge
            ((:recurrent/dom-$ sources) ".text-edit" "change")))]


    (ulmus/subscribe! ((:recurrent/dom-$ sources) ".tooltip" "mouseover")
                      #(.log js/console
                         (.getBoundingClientRect (.-currentTarget %))))

    (ulmus/subscribe! ((:recurrent/dom-$ sources) ".button.done" "click")
                      #(start!))

    (ulmus/subscribe! ((:recurrent/dom-$ sources) ".button.save" "click")
                      (fn []
                        (let [state @(:recurrent/state-$ sources)
                              named (get-in state [:metadata :name])]
                          (js/saveAs
                            (js/Blob.
                              (clj->js [(clj->yaml @(:recurrent/state-$ sources))])
                              (clj->js {:type "text/plain"}))
                            (string/lower-case
                              (str (:kind state)
                                   (if named (str "-" named))
                                   ".yml"))))))
                      

    {:done-$ 
     (ulmus/filter (fn [e] 
                     (= (.-key e) "Escape"))
                   keyboard/up-$)
     :recurrent/state-$ 
     (ulmus/start-with!
       (fn [] (or (:initial-value props) initial-state))
       (ulmus/merge
         (ulmus/map (fn [path-to-remove]
                      (let [[at & path]
                            (->>
                              (string/split path-to-remove ".")
                              (remove empty?)
                              (map (fn [token]
                                     (let [as-int (js/parseInt token 10)]
                                       (if (js/isNaN as-int)
                                         (keyword token)
                                         as-int))))
                              reverse)]
                        (fn [state]
                          (if path
                            (update-in state (reverse path)
                                       (fn [path-state]
                                         (if (vector? path-state)
                                           (into []
                                             (concat
                                               (subvec path-state 0 at)
                                               (subvec path-state (inc at))))
                                           (dissoc path-state at))))
                            (dissoc state at)))))
                    (ulmus/map
                       (fn [e]
                         (.getAttribute (.-currentTarget e) "data-path"))
                    ((:recurrent/dom-$ sources) ".remove" "click")))
         (:recurrent/state-$ editor)
         text-area-state-$))
     :recurrent/dom-$ 
     (ulmus/map (fn [[state gui-editor]]
                  ^{:hipo/key "editor-main"}
                  [:div {:class "editor-main"}
                   [:h1 "Edit Resource"]
                   ^{:hipo/key "left-right"}
                   [:div {:class "left-right"}
                    gui-editor
                    ^{:hipo/key "text-area"}
                    [:textarea {:class "text-edit"
                                :spellcheck "false"
                                :value (string/trim (clj->yaml state))}]]
                   ^{:hipo/key "bar"}
                   [:div {:class "bar"}
                    [:div {:class "button outline done"} "Start Over"]
                    [:div {:style "flex:1"}]
                    [:div {:class "button primary save"} "Save"]]
                   ])
                (ulmus/zip
                  (:recurrent/state-$ sources)
                  (:recurrent/dom-$ editor)))

     :swagger-$ (ulmus/signal-of [:get])}))


(defn pick-kind!
  []
  (let [swagger-path "https://raw.githubusercontent.com/kubernetes/kubernetes/master/api/openapi-spec/swagger.json"]
    (recurrent.core/start!
      KindPicker
      {}
      {:swagger-$
       (recurrent.drivers.http/create!
         swagger-path {:with-credentials? false})
       :recurrent/dom-$ (recurrent.drivers.vdom/for-id! "app")})))


(defn start!
  []
  (let [swagger-path "https://raw.githubusercontent.com/kubernetes/kubernetes/master/api/openapi-spec/swagger.json"]
    (ulmus/subscribe! (:selected-$ (pick-kind!))
                      (fn [selected]
                        (recurrent.core/start!
                          (state/with-state Editor)
                          {:kind (:property selected)}
                          {:swagger-$
                           (recurrent.drivers.http/create!
                             swagger-path {:with-credentials? false})
                           :recurrent/dom-$ (recurrent.drivers.vdom/for-id! "app")})))))


