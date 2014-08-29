(ns site2book.core
  (:require [net.cgrand.enlive-html :as enlive])
  (:require [clojure.string :as cstring])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(defn- parse-selector-part
  "css selectors can be unioned by comma ',', this function parse one of them."
  [selector-str]
  (let [selectors (-> selector-str
                      cstring/trim
                      (cstring/split #"\s+"))]
    (apply vector (map keyword selectors))))

(defn- str->selector
  "parse css selector in string format into enlive selector."
  [selector-str]
  (let [selector-parts (-> selector-str
                           cstring/trim
                           (cstring/split #"\s*,\s*"))]
    (set (map parse-selector-part selector-parts))))

(defn- extract-single-article
  [url-title content-selector & title-selector]
  (let [[url t] url-title
        dom (-> url java.net.URL. enlive/html-resource)
        title (or t
                  (-> dom
                      (enlive/select title-selector)
                      first
                      enlive/text)
                  nil)
        content (apply str (-> dom
                               (enlive/select content-selector)
                               enlive/emit*))]
    [content title]))

(def toc-item-snippet "<a></a><br/>")
(def article-title-snippet "<h1></h1><br/>")
(def page-snippet "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"></head><body></body></html>")

(defn- output-article
  [[content title] i]
  (let [final-title (or title (str "charpter" i))]
    [(str
      (enlive/sniptest toc-item-snippet
                       [:a] (enlive/set-attr :name (str "charpter" i)))
      (enlive/sniptest article-title-snippet
                       [:h1] (enlive/html-content final-title))
      content)
     (enlive/sniptest toc-item-snippet
                      [:a] (enlive/html-content final-title)
                      [:a] (enlive/set-attr :href (str "#charpter" i)))]))

(defn- extract-articles
  "extract article from the URL vector in format [[url title(optional, could be nil)] [url title]...]."
  [url-list content-selector & title-selector]
  (let [articles (map #(extract-single-article % content-selector title-selector) url-list)
        toc-indies (range 0 (count articles))
        articles-output (map output-article
                             articles
                             toc-indies)
        toc-nav (apply str (map second articles-output))
        article-content (apply str (map first articles-output))]
    (enlive/sniptest page-snippet
                     [:body] (enlive/html-content (str toc-nav article-content)))))

(defn- extract-article-list
  "extract article URLs from TOC page."
  [toc-src-list item-selector limit]
  (let [url-list (map #(vector
                        (-> %1 :attrs :href)
                        (-> %1 enlive/text))
                      (apply concat (map #(-> %1
                                              java.net.URL.
                                              enlive/html-resource
                                              (enlive/select item-selector))
                                         toc-src-list)))
        take-number (if (= limit 0) (count url-list) limit)]
    (take take-number url-list)))

(def cli-options
  [["-u" "--url URL" "URL of the table of content page, multiple URLs can be splited by '|'."
    :parse-fn #(cstring/split % #"\s*\|\s*")]
   ["-i" "--item-selector SELECTOR" "CSS selector to select item link in TOC page."
    :default #{[:.articleCell :.atc_title :a]}
    :parse-fn str->selector]
   ["-t" "--title-selector SELECTOR" "CSS selector to select title from each item page."
    :default #{[:h2.titName]}
    :parse-fn str->selector]
   ["-c" "--content-selector SELECTOR" "CSS selector to select content from each item page."
    :default #{[:div.articalContent]}
    :parse-fn str->selector]
   ["-l" "--limit NUMBER" "Only extract top NUMBER articles. 0 means extract all."
    :default 0
    :parse-fn #(Integer/parseInt %)]])

(defn -main [& args]
  (let [{{:keys [url item-selector title-selector content-selector limit]} :options} (parse-opts args cli-options)]
    (spit "1.html" (-> url
                 (extract-article-list item-selector limit)
                 (extract-articles content-selector title-selector)))))

