(ns konstellate.editor.components
  (:require
    recurrent.core
    recurrent.drivers.rum
    [clojure.pprint :as pprint]
    [clojure.set :as sets]
    [clojure.string :as string]
    [recurrent.state :as state]
    [recurrent.core :as recurrent :include-macros true]
    [ulmus.signal :as ulmus]))

(def shadow "2px 2px 4px rgba(0,0,0, 0.25)")
(def primary "#FEA7BD")
(def secondary "#09EDC8")

(defn parse-int
  [v]
  (let [as-int (js/parseInt v 10)]
    (if (js/isNaN as-int)
      0
      as-int)))

(defn add-remove-dom
  [path c]
  [:div {:data-path path :class (str "add-remove " c)}
   [:img {:data-path path :class "remove" :src "images/minus.svg"}]
   [:img {:class "add" :src "images/plus.svg"}]])

(recurrent/defcomponent TextInput
  [props sources]
  (let [value-$ (ulmus/map
                  (fn [e] 
                    (.-value (.-target e)))
                  ((:recurrent/dom-$ sources) "input" "keyup"))]
    {:value-$ value-$
     :recurrent/state-$ (ulmus/map (fn [v] 
                                     (fn []
                                       (condp = (:type props)
                                         :integer (parse-int v)
                                         :boolean (not= v "false")
                                         v))) value-$)
     :recurrent/dom-$ (ulmus/map
                        (fn [value]
                          [:div {:class "text-input"}
                           [:label {} (str (:label props)
                                           (when (:recurrent/key props)
                                             (str "[" (:recurrent/key props) "]")))]
                           [:input {:disabled (if (:disabled? props)
                                                true false)
                                    :class (str (if (:error? props) "error"))
                                    :type "text" 
                                    :default-value value}]])
                        (ulmus/distinct
                          (:recurrent/state-$ sources)))}))

(recurrent/defcomponent KeyPicker
  [props sources]
  (let [props-$ 
        (ulmus/map
          (fn [[definitions hidden-keys]]
            (into {}
              (filter (fn [[k]] 
                        (cond
                          (:limit props)
                          (some #{k} (:limit props))
                          hidden-keys
                          (not (some #{k} hidden-keys))
                          :else
                          true))
                (if (:kind props)
                  (get-in definitions [(keyword (:kind props)) :properties])
                  definitions))))
          (ulmus/zip
            (:definitions-$ sources)
            (:hidden-keys-$ sources)))
        hovered-prop-$ (ulmus/map
                         (fn [e]
                           (keyword
                             (.getAttribute (.-target e) "data-prop")))
                         ((:recurrent/dom-$ sources) ".property" "click"))
        hovered-props-$ (ulmus/reduce (fn [hovered-props [p hidden]]
                                        (sets/difference
                                          (cond
                                            (not p) hovered-props
                                            (:single? props) #{p}
                                            (some #{p} hovered-props)
                                            (disj hovered-props p)
                                            :else
                                            (conj hovered-props p))
                                          hidden))
                                      #{} 
                                      (ulmus/zip
                                        hovered-prop-$
                                        (:hidden-keys-$ sources)))
        ; Look here
        ; return as selected-$
        hovered-definitions-$ (ulmus/map
                               (fn [[props hovered-props]]
                                 (map
                                   (fn [hovered-prop]
                                     {:property hovered-prop
                                      :definition (get props hovered-prop)})
                                   hovered-props))
                               (ulmus/zip
                                 props-$
                                 hovered-props-$))
        selected-$ (ulmus/flatten
                     (ulmus/sample-on hovered-definitions-$
                                      (ulmus/merge
                                        ((:recurrent/dom-$ sources) ".property" "dblclick")
                                        ((:recurrent/dom-$ sources)
                                         ".create.button" "click"))))]
    {:selected-$ selected-$
     :close-$ ((:recurrent/dom-$ sources) ".close" "click")
     :recurrent/dom-$ (ulmus/map
                        (fn [[definitions k8s-props hovered-props hovered-prop]]
                          (let [outer (get definitions (keyword (:kind props)))
                                required (:required outer)
                                prop-names (map (fn [[k v]] (str (name k))) k8s-props)
                                hovered-definitions (remove nil? (map #(with-meta (get k8s-props %) {:prop %}) (sort hovered-props)))]
                            `[:div {:class "key-picker in"}
                              [:div {:class ~(str "button inverse create " (if hovered-prop "show"))} ~(:action props)]
                              [:div {:class "close"} "x"]
                              [:div {:class "content"}
                               [:div {:class "properties"}
                                [:h1 {} ~(:heading props)]
                                [:ul {}
                                 ~@(mapv (fn [p]
                                           (let [prop-name (last (string/split p "."))]
                                             [:li {:data-prop p
                                                   :class (str "property" 
                                                               (if (some #{(keyword p)} hovered-props)
                                                                 " selected"))}
                                              prop-name
                                              (if (some #{prop-name} required)
                                                [:span {} " (required)"])]))
                                         (sort prop-names))]]
                               [:div {:class "description"}
                                ~@(when (not (empty? hovered-definitions))
                                    (map 
                                      (fn [hovered]
                                        [:div {:class "fade in"}
                                         [:br {}]
                                         "--"
                                         [:h4 {}
                                          (name (:prop (meta hovered)))]
                                         [:h4 {} "Description:"]
                                         (:description hovered)
                                         [:h4 {} "Type:"]
                                         (if (:$ref hovered)
                                           (string/replace (:$ref hovered)
                                                           "#/definitions/" "")
                                           (:type hovered))
                                         (if (:$ref hovered)
                                           [:div {}
                                            [:br {}]
                                            (get-in definitions
                                                    [(keyword
                                                       (string/replace (:$ref hovered)
                                                                       "#/definitions/" ""))
                                                     :description])])])
                                      hovered-definitions))
                                [:br {}]
                                [:br {}]
                                [:br {}]
                                [:br {}]]]]))
                        (ulmus/zip
                          (:definitions-$ sources)
                          props-$
                          hovered-props-$
                          hovered-prop-$))}))

(defn ObjectEditor
  [props sources]
  (let [path (str (:parent props) (:property props) ".")
        hovered?-$ (ulmus/map
                     (fn [hovered-path]
                       (= hovered-path path))
                     (:hovered-editor-$ sources))
        key-change-$
        (ulmus/map (fn [e]
                     (let [prev-value (.getAttribute (.-target e) "data-key")
                           value (.-value (.-target e))]
                       [(keyword prev-value) (keyword value)]))
                   (ulmus/merge
                     ((:recurrent/dom-$ sources) ".key-input" "keyup")
                     ((:recurrent/dom-$ sources) ".key-input" "change")))
        value-change-$
        (ulmus/map (fn [e]
                     (let [k (.getAttribute (.-target e) "data-key")
                           v (.-value (.-target e))]
                       [(keyword k) v]))
                   (ulmus/merge
                     ((:recurrent/dom-$ sources) ".value-input" "keyup")
                     ((:recurrent/dom-$ sources) ".value-input" "change")))]
  {:recurrent/state-$ 
     (ulmus/merge
       (ulmus/map (fn [] 
                    (fn [state]
                      (with-meta state
                                 {:order (into #{} (keys state))})))
                  (:recurrent/state-$ sources))
       (ulmus/map (fn [] 
                    (fn [state]
                      (let [empty-keyword (keyword "")]
                        (if (some #{empty-keyword} (keys state))
                          state
                          (with-meta (assoc state (keyword "") "")
                                     {:order
                                      (into #{}
                                        (concat
                                          (:order (meta state))
                                          [(keyword "")]))})))))
                  ((:recurrent/dom-$ sources) 
                   ".add" "click"))
       (ulmus/map (fn [[k new-k]]
                    (fn [state]
                      (let [v (state k)]
                        (let [order (:order (meta state))
                              new-order (into #{} (map (fn [order-k]
                                                         (if (= k order-k)
                                                           new-k
                                                           order-k)) order))]
                          (with-meta
                            (-> state
                                (dissoc k)
                                (assoc new-k v))
                            {:order new-order})))))
                  key-change-$)
       (ulmus/map (fn [[k v]] #(assoc % k v)) value-change-$))

   :recurrent/dom-$
   (ulmus/map
     (fn [[object hovered?]]
       (let [ks (into #{} (keys object))
             order (sets/intersection
                     (or (:order (meta object)) ks)
                     ks)]
       `[:div {:class ~(str "object-editor " (if hovered? "hover")) :data-path ~path}
         [:span {} ~(str (:property props) " (object)")]
         ~(when hovered?
            (add-remove-dom path "kv-editor"))
         ~@(map-indexed
             (fn [i k]
               (let [v (object k)]
                 [:div {:key i
                        :class "element"}
                  [:div {:class "text-input"}
                   [:label {} "Key"]
                   [:input {:data-key (str (name k))
                            :class "key-input"
                            :type "text"
                            :default-value (str (name k))}]]
                  [:div {:class "text-input"}
                   [:label {} "Value"]
                   [:input {:data-key (str (name k))
                            :class "value-input"
                            :type "text"
                            :default-value (str v)}]]
                  [:img {:data-path (str path "." (name k))
                         :class "remove"
                         :src "images/minus.svg"}]]))
             order)]))
     (ulmus/zip
       (:recurrent/state-$ sources)
       hovered?-$))}))

(defn Array
  [props sources]
  (let [path (str (:parent props) (:property props) ".")
        hovered?-$ (ulmus/map
                     (fn [hovered-path]
                       (= hovered-path path))
                     (:hovered-editor-$ sources))]
    ((state/collection-of
       (:element props)
       :collect (fn [props sources children-$]
                  {:recurrent/state-$ 
                   (ulmus/merge
                     (ulmus/pickmerge :recurrent/state-$ children-$)
                     (ulmus/map
                       (fn [action]
                         (fn [prev-state] 
                           (let [prev-state (or prev-state [])]
                             (condp = action
                               :add (conj prev-state (or (:default-value props) {}))
                               :remove (into [] (drop-last 1 prev-state))))))
                       (ulmus/merge
                         (ulmus/map 
                           (constantly :add)
                           ((:recurrent/dom-$ sources)
                            "> .array-add-remove > .add" "click")))))

                   :recurrent/dom-$
                   (ulmus/map
                     (fn [[hovered? children]]
                       `[:div {:class ~(str "array " (if hovered? "hover"))
                               :data-path ~path}

                        ~(when hovered?
                           [:div {:data-path path :class "array-add-remove"}
                            [:img {:data-path path :class "remove" :src "images/minus.svg"}]
                            [:img {:class "add" :src "images/plus.svg"}]])

                         [:span {} ~(str (:property props) " (array)")]
                         ~@children])
                     (ulmus/zip
                       hovered?-$
                       (ulmus/pickzip :recurrent/dom-$ children-$)))}))

     props sources)))


(recurrent/defcomponent Editor
  [props sources]

  (let [path 
        (string/replace
          (str (:parent props) "." (:property props) "." (:recurrent/key props) ".")
          #"\.+" ".")

        hovered?-$ (ulmus/map
                     (fn [hovered-path]
                       (= hovered-path path))
                     (:hovered-editor-$ sources))
        key-picker (KeyPicker (assoc props
                                     :action "Add"
                                     :heading (str "What properties do you want to add to the " (last (string/split (str (name (:kind props))) ".")) "?"))
                              (assoc sources
                                     :hidden-keys-$ (ulmus/map keys (:recurrent/state-$ sources))))

        key-picker-open?-$
        (ulmus/merge
          (ulmus/map (constantly true)
                     (ulmus/merge
                       ((:recurrent/dom-$ sources) "> .add" "click")
                       ((:recurrent/dom-$ sources) "> .obj-add-remove > .add" "click")))
          (ulmus/map (constantly false)
                     (ulmus/merge
                       (:selected-$ key-picker)
                       (:close-$ key-picker))))
        make-child (fn [definitions k]
                     (let [path-to-definition [(keyword (:kind props)) :properties k]
                           spec (get-in 
                                  definitions
                                  path-to-definition)]

                       (cond 
                         (and (:$ref spec)
                              (not (string/includes? (:$ref spec) "IntOrString")))
                         ((state/isolate Editor [k])
                          (assoc props
                                 :parent path
                                 :property (name k)
                                 :kind (last (string/split (:$ref spec) "/")))
                          sources)
                         (= (:type spec) "array")
                         ((state/isolate Array [k])
                          (if (get-in spec [:items :$ref])
                            (assoc props
                                   :default-value {}
                                   :element Editor
                                   :parent path 
                                   :property (name k)
                                   :kind (last (string/split
                                                 (get-in spec
                                                         [:items :$ref])
                                                 "/")))
                            (assoc props
                                   :default-value ""
                                   :label (name k)
                                   :parent path
                                   :property (name k)
                                   :element TextInput))
                          sources)
                         (= (:type spec) "object")
                         ((state/isolate ObjectEditor [k])
                          (assoc props 
                                 :parent path
                                 :property (name k))
                          sources)
                         :else
                         ((state/isolate TextInput [k])
                          {:disabled? (some #{k} [:kind :apiVersion])
                           :error? (not spec)
                           :label (str (name k))
                           :type (keyword (:type spec))}
                          sources))))


        children-$ (ulmus/reduce 
                     (fn [prev-children [definitions state]]
                       (if 
                         (and (empty? prev-children)
                              (or (not state)
                                  (not definitions)))
                         {}
                         (let [make (partial make-child definitions)
                               state-keys (set (keys state))
                               child-keys (set (keys prev-children))
                               new-keys (sets/difference 
                                          state-keys child-keys)
                               lost-keys (sets/difference child-keys state-keys)]

                           (doseq [k lost-keys]
                             (recurrent.core/close! (prev-children k)))

                           (merge
                             (apply dissoc prev-children lost-keys)
                             (into {} (map (fn [k] [k (make k)]) new-keys))))))
                     {}
                     (ulmus/zip
                       (:definitions-$ sources)
                       (:recurrent/state-$ sources)))
        child-components-$ (ulmus/map
                             #(remove nil? (into [] (vals %)))
                             (ulmus/distinct children-$))
        children-dom-$ (ulmus/pickzip :recurrent/dom-$ child-components-$)
        children-state-$ (ulmus/pickmerge :recurrent/state-$ child-components-$)]

    {:recurrent/state-$ 
     (ulmus/merge
       (ulmus/map
         (fn [selected]
           (fn [prev-state]
             (assoc (or prev-state {})
                    (:property selected) 
                    (condp = (:type (:definition selected))
                      "boolean" false
                      "string" ""
                      "integer" 0
                      "array" []
                      "object" {}
                      {}))))
         (:selected-$ key-picker))
       (ulmus/distinct children-state-$))

     :recurrent/dom-$ (ulmus/map 
                        (fn [[hovered? definitions child-dom key-picker-open? key-picker-dom]]
                          `[:div {:class ~(str "editor " (if hovered? "hover"))
                                  :data-path ~path}
                            ~(when (and (:parent props) hovered?)
                               [:div {:key "obj-add-remove" :data-path path :class "obj-add-remove"}
                                [:img {:data-path path :class "remove" :src "images/minus.svg"}]
                                [:img {:class "add" :src "images/plus.svg"}]])
                            ~(when (not (:parent props))
                               [:div {:class "add"} "+"])
                             [:div {:key "tooltip"
                                    :class "kind tooltip"
                                    :data-tooltip ~(:description definitions)}
                              ~(when (not (empty? (:property props)))
                                 [:span {} (str (:property props)
                                             (when (:recurrent/key props)
                                               (str "["(:recurrent/key props)"]")))])
                              ~@child-dom]
                             ~(if key-picker-open? key-picker-dom)])
                        (ulmus/zip
                          hovered?-$
                          (ulmus/map
                            #(get % (keyword (:kind props)))
                            (:definitions-$ sources))
                          (ulmus/distinct 
                            children-dom-$)
                          key-picker-open?-$
                          (:recurrent/dom-$ key-picker)))}))



