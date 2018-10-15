(ns cljss.builder
  (:require [cljss.media :refer [build-media]]
            [cljss.collect :as c]
            [cljss.utils :as utils]))

(defn status? [[rule value]]
  (and (re-matches #"^.*\?$" (name rule))
       (map? value)))

(defn- build-medias [styles cls idx]
  (reduce (fn [acc [a b]] (conj acc (build-media cls idx {a b}))) [] (seq styles)))

(defn- combine-medias [medias]
  (reduce (fn [[mstatic mvals midx] [s v idx]] [(conj mstatic s) (concat mvals v)] idx) [[] [] 0] medias))

(defn build-styles [cls styles]
  (let [rule-index 0
        pseudo (filterv utils/pseudo? styles)
        nested (->> styles
                    (filterv (comp not utils/pseudo?))
                    (filterv utils/nested?))
        medias (build-medias (:cljss.core/media styles) cls rule-index)
        [mstatics mvals mrule-index] (combine-medias medias)
        rule-index (or mrule-index rule-index)
        styles (dissoc styles :cljss.core/media)
        styles (filterv #(and (not (utils/pseudo? %)) (not (utils/nested? %))) styles)

        [static vals rule-index] (c/collect-styles cls styles rule-index)
        [pstyles rule-index] (c/collect-dynamic-styles
                               rule-index
                               pseudo
                               cls
                               (fn [rule] (subs (name rule) 1)))
        [nstyles rule-index] (c/collect-dynamic-styles
                               rule-index
                               nested
                               cls
                               (fn [rule] (str " " rule)))

        vals (->> pstyles
                  (mapcat second)
                  (into vals)
                  (concat mvals)
                  (into []))
        vals (->> nstyles
                  (mapcat second)
                  (into vals))
        static (into [static] (map first pstyles))
        static (into static (map first nstyles))
        static (vec (concat static mstatics))]
    [cls static vals]))

(comment
  (build-styles
    "hello"
    {"&:first-child" {:color "red"}
     "a" {:color "blue"}}))
