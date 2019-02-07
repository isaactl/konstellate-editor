(ns konstellate.editor.style
  (:require
    garden.selectors
    [garden.core :as garden]
    [garden.def :refer [defkeyframes]]
    [konstellate.components.style :as components]))

(def shadow "0 5px 25px 0 rgba(46, 91, 255, 0.4)")
(def primary "#FEA7BD")
(def secondary "#09EDC8")
(def grey "#dfe3e8")
(def highlight "#00a2ff")
(def text "#212b35")

(defkeyframes FadeInAnim
  [:from {:opacity 0
          :transform "scale(0.95, 0.95)"}]
  [:to {:opacity 1
        :transform "scale(1, 1)"}])

(def Reset
  [[:html :body {:width "100%"
                 :height "100%"}]
   [:body {:box-sizing "border-box"
           :font-style "sans-serif"
           :font-family "'Rubik', sans-serif"
           :margin 0
           :padding 0
           :width "100%"
           :height "100%"}]
   [:#app {:width "100%"
           :height "100%"}]
   [:body [:* {:box-sizing "border-box"
               :margin 0
               :padding 0}]]])

(def Main [:.editor-main {:width "100%"
                          :height "100%"
                          :max-height "100vh"
                          :padding "64px 64px 32px"
                          :display "flex"
                          :flex-direction "column"}
           [:h1 {:margin-bottom "32px"}]
           [:.bar {:display "flex"
                   :margin-top "32px"}]])

(def AddRemove
  [:.add-remove {:animation "FadeInAnim 300ms ease"
                 :display "flex"
                 :position "absolute"
                 :right 0
                 :top "16px"}
   [:img {:cursor "pointer"
          :display "block"
          :margin-right "16px"}]])

(def LeftRight [:.left-right
                {:display "grid"
                 :flex 1
                 :min-height 0
                 :grid-gap "16px"
                 :grid-template-columns "1fr 1fr"}
                [:.text-edit {:background "black"
                              :font-size "12px"
                              :font-family "monospace"
                              :color "white"
                              :padding "32px"
                              :border (str "1px solid " grey)
                              :border-radius "8px"}]])

(def TextInput
  [:.text-input
   [:label {:display "block"
            :font-size "12px"
            :font-weight "normal"
            :margin-bottom "4px"
            :opacity "0.5"}]
   [:input {:border (str "1px solid " grey)
            :border-radius "4px"
            :font-size "16px"
            :height "40px"
            :outline "none"
            :padding "0 16px"
            :width "100%"}
    [:&.error {:border "1px solid red"}]]])

(def KeyPicker
  [:.key-picker {:animation "FadeInAnim 300ms ease"
                 :background "white"
                 :position "fixed"
                 :font-size "14px"
                 :top 0
                 :bottom 0
                 :left 0
                 :right 0
                 :z-index 2}
   [:.close {:color "white"
             :cursor "pointer"
             :border "2px solid white"
             :border-radius "50%"
             :font-weight "bold"
             :font-size "18px"
             :position "absolute"
             :top "32px"
             :right "64px"
             :line-height "26px"
             :text-align "center"
             :width "32px"
             :height "32px"}]
   [:.button {:position "absolute"
              :bottom "-64px"
              :right "64px"
              :transition "bottom 300ms ease"}
    [:&.show {:bottom "32px"}]]
   [:.content {:display "grid"
               :grid-template-columns "61% 39%"
               :height "100%"
               :max-height "100%"
               :transition "opacity 300ms ease"}
   [:.description {:color "white"
                   :background "black"
                   :box-shadow "0 2px 4px 0 rgba(0, 0, 0, 0.5), 0 5px 25px 0 rgba(46, 91, 255, 0.4)"
                   :line-height "1.6em"
                   :max-height "100%"
                   :max-width "100%"
                   :overflow-y "auto"
                   :padding "64px"
                   :word-break "break-word"
                   :text-overflow "ellipsis"}
    [:.fade {:opacity 0
             :animation "FadeInAnim 300ms ease forwards"}]]
    [:h4 {:margin-top "16px"}]]
   [:.properties {:color text
                  :max-height "100%"
                  :padding "64px"
                  :overflow-y "auto"}
    [:h1 {:margin-bottom "32px"}]]
   [:.property {:border-radius "4px"
                :border (str "1px solid " grey)
                :cursor "pointer"
                :list-style "none"
                :margin "8px"
                :padding "16px"
                :font-weight "bold"
                :transition "box-shadow 500ms ease, border 500ms ease"}
    [:span {:color highlight}]
    [:&.selected {:border (str "1px solid " highlight)
                  :box-shadow shadow}]
    [:&:hover {:border (str "1px solid " highlight)}]]])

(def Editor
  [:.editor
   {:border (str "1px solid " grey)
    :box-shadow "0px 0px 0px rgba(0,0,0,0)"
    :border-radius "8px"
    :overflow-y "auto"
    :position "relative"
    :padding "16px"
    :width "100%"
    :transition "box-shadow 500ms ease, border 500ms ease"}
   [:&.hover {:border (str "1px solid " highlight)
              :box-shadow shadow}]
   [:.obj-add-remove {:animation "FadeInAnim 300ms ease"
                      :display "flex"
                      :position "absolute"
                      :right 0
                      :top "16px"}
    [:img {:cursor "pointer"
           :display "block"
           :margin-right "16px"}]]
  [(garden.selectors/> "" :.add) {:background "black"
                                  :border-radius "50%"
                                  :box-shadow shadow
                                  :color "white"
                                  :cursor "pointer"
                                  :position "fixed"
                                  :text-align "center"
                                  :line-height "64px"
                                  :width "64px"
                                  :height "64px"
                                  :left "calc(50% - 96px)"
                                  :bottom "128px"
                                  :z-index 2}]
  [:.kind {:padding-right "8px"
           :max-height "100%"
            :font-weight "bold"
            :font-size "10pt"}
    ["> *" {:margin-top "8px"}]
    [(garden.selectors/> "" :span) {:display "block"
                                    :margin-top 0
                                    :padding "8px"
                                    :font-size "16px"
                                    :font-weight "normal"}]]])


(def Array
  [:.array 
   {:border (str "1px solid " grey)
    :border-radius "4px"
    :position "relative"
    :padding "16px"}

   [:&.hover {:border (str "1px solid " highlight)
              :box-shadow shadow}]

   [:.array-add-remove {:animation "FadeInAnim 300ms ease"
                        :display "flex"
                        :position "absolute"
                        :right 0
                        :top "16px"}
    [:img {:cursor "pointer"
           :display "block"
           :margin-right "16px"}]]

    [(garden.selectors/> "" :span) {:display "block"
                                    :padding "8px"
                                    :font-size "16px"
                                    :font-weight "normal"}]
   [:.add-remove
     {:color "white"
      :display "grid"
      :grid-template-columns "1fr 1fr"
      :text-align "center"}
     [:.array-button {:border (str "1px solid " grey)
                      :cursor "pointer"
                      :font-weight "bold"
                      :padding "4px"}]
     [:.add {:background "green"}]
     [:.remove {:background "red"}]]])

(def ObjectEditor
  [:.object-editor {:position "relative"
                    :border (str "1px solid " grey)
                    :border-radius "4px"
                    :padding "16px"}
   [:&.hover {:border (str "1px solid " highlight)
              :box-shadow shadow}]
   [(garden.selectors/> "" :span) {:display "block"
                                   :padding "8px"
                                   :font-size "16px"
                                   :font-weight "normal"}]
   [:.element:hover [:img {:display "block"}]]
   [:.element {:display "grid"
               :justify-items "center"
               :align-items "center"
               :grid-template-columns "auto auto 32px"
               :grid-gap "8px"
               :margin-bottom "8px"}
    [:img {:display "none"
           :position "relative"
           :top "8px"}]]
   [:label {:display "block"}]])


(def styles
  [components/Button
   AddRemove
   FadeInAnim
   Main
   LeftRight
   ObjectEditor
   TextInput
   KeyPicker
   Editor
   Array])

(defn spit-styles!
  []
  (spit "resources/public/css/style.css" (garden/css (concat [Reset] styles))))

