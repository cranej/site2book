(ns site2book.core
  (:require [net.cgrand.enlive-html :as enlive])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-u" "--url URL" "URL of the table of content page, multiple URLs splited by '|'."]
   ["-i" "--item-selector SELECTOR" "CSS selector to select item link in TOC page."
    :default ""]
   ["-t" "--title-selector SELECTOR" "CSS selector to select title from each item page."]
   ["-c" "--content-selector SELECTOR" "CSS selector to select content from each item page."]])

(defn -main [& args]
  (let [{{:keys [url item-selector title-selector content-selector]} :options} (parse-opts args cli-options)]
    (print url item-selector)))

