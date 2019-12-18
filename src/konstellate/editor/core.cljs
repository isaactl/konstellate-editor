(ns konstellate.editor.core
  (:require 
    recurrent.drivers.rum
    recurrent.drivers.http
    cljsjs.js-yaml
    cljsjs.axios
    cljsjs.filesaverjs
    [cljs.reader :refer [read-string]]
    [clojure.string :as string]
    [recurrent.core :as recurrent :include-macros true]
    [recurrent.state :as state]
    [konstellate.editor.components :as components]
    [ulmus.keyboard :as keyboard]
    [ulmus.signal :as ulmus]))


(declare start!)

; (defn by-id
;   [id]
;   (.getElementById
;     js/document
;     (name id)))

(def chart
  (.getAttribute 
    (.getElementById js/document "editor_chart")
    "value"))

(def chartSource
  (.getAttribute
    (. js/document (getElementById "editor_source"))
    "value"))

; (println chartSource.innerHTML)
; (println 
;   (.getAttribute chartSource "name"))

; (println 
;   (goog.object/forEach chartSource.attributes
;      (fn [val key obj]
;         (println val key obj))))

; (println
;   (get-named-item chartSource.attributes "name"))

; (println 
;   (by-id "editor"))

; (set! (.-value (.getElementById js/document "chart")) 23.3)

; (println 
;   (aget
;     (by-id 
;     "editor")
;     "chart"))
; (-> js/document
;   (.getElementById "editor")
;   (.-innerHTML)) ; returns contents of element with id 'app'

(defonce style-scope (gensym))

(defn clj->yaml
  [x]
  (js/jsyaml.safeDump (clj->js x)))

(defn yaml->clj
  [x]
  (js->clj
    (js/jsyaml.safeLoad x)
    :keywordize-keys true))

(defn parse-swagger-definitions
  [swagger]
  (get
    (js->clj
      (.parse js/JSON swagger)
      :keywordize-keys true)
    :definitions))


(defn KindPicker
  [props sources]
  (let [key-picker (components/KeyPicker 
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
                     {:definitions-$ (or (:definitions-$ sources)
                                         (ulmus/map parse-swagger-definitions
                                                    ((:swagger-$ sources) [:get])))
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
        editor (components/Editor {:kind (:kind props)}
                                  {:definitions-$ (or (:definitions-$ sources)
                                                      (ulmus/map
                                                        parse-swagger-definitions
                                                        ((:swagger-$ sources) [:get])))
                                   :hovered-editor-$ hovered-editor-$
                                   :recurrent/state-$ (:recurrent/state-$ sources)
                                   :recurrent/dom-$ (:recurrent/dom-$ sources)})

        initial-state {:kind kind-name
                       :apiVersion api-version
                       :metadata {:name ""}}

        text-area-state-$ 
        (ulmus/map
          (fn [e]
            (let [parsed (yaml->clj (.-value (.-target e)))]
              (cond
                (empty? (.-value (.-target e))) (fn [] initial-state)
                parsed (fn [] parsed)
                :else identity)))
          (ulmus/merge
            ((:recurrent/dom-$ sources) ".text-edit" "change")))
        
        ; merge text-area-state
        ; and keydown events
        text-area-value-$ (ulmus/map #(string/trim (clj->yaml %)) (:recurrent/state-$ sources))]


    ; (ulmus/subscribe! ((:recurrent/dom-$ sources) ".tooltip" "mouseover")
    ;                   #(.log js/console
    ;                      (.getBoundingClientRect (.-currentTarget %))))


    (ulmus/subscribe! ((:recurrent/dom-$ sources) ".button.done" "click")
             #(start!))

    (ulmus/subscribe! ((:recurrent/dom-$ sources) ".button.save" "click")
                      (fn []
                        (let [state @(:recurrent/state-$ sources)
                              named (get-in state [:metadata :name])]
                          (println "Hello world!")
                          (doto (js/axios.post 
                            (str "/api/chart/" chart "/templates/raw/" chart "-"  
                              (string/lower-case
                                (str (:kind state)
                                     (if named (str "-" named))
                                     ".yml")))
                            (clj->yaml @(:recurrent/state-$ sources))
                            (clj->js {:headers {"Content-Type" "text/plain"}}))
                                (.then (fn [resp]
                                        (js/console.log "resp: " resp)))
                                (.catch (fn [err]
                                        (js/alert "err: " err)))))))
                          ; (js/saveAs
                          ;   (js/Blob.
                          ;     (clj->js [(clj->yaml @(:recurrent/state-$ sources))])
                          ;     (clj->js {:type "text/plain"}))
                          ;   (string/lower-case
                          ;     (str (:kind state)
                          ;          (if named (str "-" named))
                          ;          ".yml"))))))


    {:done-$ 
     (ulmus/filter (fn [e] 
                     (= (.-key e) "Escape"))
                   keyboard/up-$)
     :save-$ ((:recurrent/dom-$ sources) ".button.save" "click")
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
     (ulmus/map (fn [[state gui-editor text-area-value]]
                  [:div {:class "editor-main" :key "editor-main"}
                   [:div {:class "left-right"}
                    gui-editor
                    [:textarea {:class "text-edit"
                                :spell-check "false"
                                :value text-area-value}]]
                   [:div {:class "bar"}
                    [:div {:class "button outline done" :style {:display "block"}} "Start Over"]
                    [:div {:style {:flex 1}}]
                    [:div {:class "button primary save"} "Save"]]
                   ])
                (ulmus/zip
                  (:recurrent/state-$ sources)
                  (:recurrent/dom-$ editor)
                  text-area-value-$))

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
       :recurrent/dom-$ (recurrent.drivers.rum/create! "editor")})))


(defn start!
  []
  (let [swagger-path "https://raw.githubusercontent.com/kubernetes/kubernetes/master/api/openapi-spec/swagger.json"
        kind-picker (pick-kind!)]
    (ulmus/subscribe! (:selected-$ kind-picker)
                      (fn [selected]
                        (recurrent.core/close! kind-picker)
                        (recurrent.core/start!
                          (state/with-state Editor)
                          {:kind (:property selected)}
                          {:swagger-$
                           (recurrent.drivers.http/create!
                             swagger-path {:with-credentials? false})
                           :recurrent/dom-$ (recurrent.drivers.rum/create! "editor")})))))



(.addEventListener js/document "DOMContentLoaded" start!)