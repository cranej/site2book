(ns site2book.core
  (:require [net.cgrand.enlive-html :as enlive])
  (:require [clojure.string :as cstring])
  (:require [clojure.java.io :as io])
  (:require [clojure.java.shell :as shell])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

;;common utility functions
(defn download-file [uri file]
  "download uri as file"
  (with-open [in (io/input-stream uri)
              out (io/output-stream file)]
    (io/copy in out)))

(defn md5
  "Generate a md5 checksum for the given string"
  [token]
  (let [hash-bytes
        (doto (java.security.MessageDigest/getInstance "MD5")
          (.reset)
          (.update (.getBytes token)))]
    (.toString
     (new java.math.BigInteger 1 (.digest hash-bytes)) ; Positive and the size of the number
     16)))

(defn- resolve-path
  "if other-path is absolute path, return other-path as a file. Otherwise return (clojure.java.io/file base-path otherwise)"
  [base-path other-path]
  (let [base-file (io/as-file base-path)
        other-file (io/as-file other-path)]
    (if (.isAbsolute other-file)
      other-file
      (io/file base-file other-file))))

;;functions to parse string to enlive selector
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

;;functions to extract articles
(defn- fix-sina-img-src
  "Sina blog hide real image src in attribute real_src, this tranformation fix it."
  [dom]
  (enlive/transform dom
                    [[:img (enlive/attr? :real_src)]]
                    (fn [matched]
                      (let [real-src (-> matched :attrs :real_src)]
                        (assoc-in matched [:attrs :src] real-src)))))

(def file-downloading-instances
  "atom of vector to hold file downloading instances created by `future`"
  (atom []))

(def book-folder
  "temp folder to store downloaded images"
  (let [tmp-folder (io/file (System/getProperty "java.io.tmpdir"))
        book-folder (io/file tmp-folder (str (java.util.UUID/randomUUID)))]
    (.mkdir book-folder)
    book-folder))

(defn- download-images-for-article
  "download images in article to local and replace image links to local ones"
  [dom folder]
  (enlive/transform dom
                    [[:img (enlive/attr? :src)]]
                    (fn [matched]
                      (let [src (-> matched :attrs :src)
                            file-name  (str (md5 src) ".jpg")]
                        (swap! file-downloading-instances
                               conj
                               (future (download-file src (io/file folder file-name))))
                        (assoc-in matched [:attrs :src] file-name)))))

(defn- extract-single-article
  [[url t] content-selector & title-selector]
  (let [dom (-> url java.net.URL. enlive/html-resource)
        title (or t
                  (-> dom
                      (enlive/select title-selector)
                      first
                      enlive/text)
                  nil)
        content (apply str (-> dom
                               (enlive/select content-selector)
                               fix-sina-img-src
                               (download-images-for-article book-folder)
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
  [toc-src-list item-selector limit reverse-toc]
  (let [url-list (map #(vector
                        (-> %1 :attrs :href)
                        (-> %1 enlive/text))
                      (apply concat (map #(-> %1
                                              java.net.URL.
                                              enlive/html-resource
                                              (enlive/select item-selector))
                                         toc-src-list)))
        take-number (if (= limit 0) (count url-list) limit)]
    (take take-number
          (if reverse-toc
            (reverse url-list)
            url-list))))

(def cli-options
  [["-u" "--url URL" "URL of the table of content page, multiple URLs can be splited by '|'."
    :parse-fn #(cstring/split % #"\s*\|\s*")]
   ["-o" "--output OUTPUT" "name of generated book"
    :default "site2book-output.mobi"
    :parse-fn #(if (-> % (.toLowerCase) (.endsWith ".mobi"))
                 %
                 (str % ".mobi"))]
   ["-a" "--author AUTHOR" "specifiy the author of this book."
    :default "佚名"]
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
    :parse-fn #(Integer/parseInt %)]
   ["-r" "--reverse-toc" "Build the table of content of output html in reverse order. Use this option if the input table of content pages list articles in a reverse order."]])

(defn -main [& args]
  (let [options (:options (parse-opts args cli-options))
        {:keys [url output author item-selector title-selector content-selector limit reverse-toc]} options
        book-title (first (cstring/split output #"\.")) ;;set title as output without extension name
        output-full-name (resolve-path (System/getProperty "user.dir")
                                       output)
        html-file-path (.getAbsolutePath (io/file book-folder "1.html"))]
    (spit html-file-path
          (-> url
              (extract-article-list item-selector limit reverse-toc)
              (extract-articles content-selector title-selector)))
    ;;wait for all downloading job finished
    (doseq [job @file-downloading-instances]
      (try
        @job
        (catch Exception e
          (println (str "error while downloading image : "
                        (.getMessage e))))))
    (let [{:keys [exit out err]} (shell/sh "ebook-convert"
                                           (str "\"" html-file-path "\"")
                                           (str "\"" output-full-name "\"")
                                           "--title"
                                           book-title
                                           "--authors"
                                           author)]
      (if (not (empty? err))
        (do
          (println "Something went wrong.")
          (println err)
          (println "Html file generated at " html-file-path))))))

