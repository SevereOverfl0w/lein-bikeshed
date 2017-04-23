(ns bikeshed.core
  "Define all the functionalities of bikeshed"
  (:require [clojure.string :refer [blank? starts-with? trim join]]
            [clojure.java.io :as io]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.tools.namespace.find :as ns-find]
            [clojure.tools.reader :as r])
  (:import (java.io BufferedReader StringReader File)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn bad-fn
  "this is a bad function."
  []
  (with-redefs [+ -]
    (+ 2 2)))

(defn no-docstring
  []
  nil)

(defn empty-docstring
  "" ;; hah! take that lein-bikeshed
  []
  nil)

(defn colliding-arguments
  "Arguments will be colliding"
  ([map])
  ([map first]))

(defn- get-all
  "Returns all the values found for the LOOKED-UP-KEY passed as an argument
  recursively walking the MAP-TO-TRAVERSE provided as argument"
  ([map-to-traverse looked-up-key]
   (let [result (atom [])]
     (doseq [[k v] map-to-traverse]
       (when (= looked-up-key k)
         (swap! result conj v))
       (when (map? v)
         (let [sub-map (get-all v looked-up-key)]
           (when-not (empty? sub-map)
             (reset! result
                     (apply conj @result sub-map))))))
     @result))
  ([map-to-traverse k & ks]
   (mapcat (partial get-all map-to-traverse) (cons k ks))))

(defn load-namespace
  "Reads a file, returning the namespace name"
  [f]
  (try
    (let [ns-dec (ns-file/read-file-ns-decl f)
          ns-name (second ns-dec)]
      (require ns-name)
      ns-name)
    (catch Exception e
      (println (str "Unable to parse " f ": " e))
      nil)))

(defn read-namespace
  "Reads a file, returning a map of the namespace to a vector of maps with
  information about each var in the namespace."
  [f]
  (try
    (let [ns-dec (ns-file/read-file-ns-decl f)
          ns-name (second ns-dec)]
      (require ns-name)
      (->> ns-name
           ns-interns
           vals))
    (catch Exception e
      (println (str "Unable to parse " f ": " e))
      [])))

(defn has-doc
  "Returns a map of method name to true/false depending on docstring occurance."
  [function-name]
  {(str function-name) (and (boolean (:doc (meta function-name)))
                            (not= "" (:doc (meta function-name))))})

(defn has-ns-doc
  "Returns a map of namespace to true/false depending on docstring occurance."
  [namespace-name]
  (let [doc (:doc (meta (the-ns (symbol (str namespace-name)))))]
    {(str namespace-name) (and (boolean doc)
                               (not= "" doc))}))

(defn long-lines
  "Complain about lines longer than <max-line-length> characters.
  max-line-length defaults to 80."
  [source-files & {:keys [max-line-length] :or {max-line-length 80}}]
  (let [msg (format " Line longer than %s characters." max-line-length)]
    (doseq [f source-files]
      (doseq [[idx line] (map-indexed vector (with-open [r (io/reader f)]
                                               (doall (line-seq r))))
              :when (> (count line) max-line-length)]
        (println (join ":" [(.getPath f) (inc idx) 0 msg]))))))

(defn trailing-whitespace
  "Complain about lines with trailing whitespace."
  [source-files]
  (doseq [f source-files]
    (doseq [[idx line] (map-indexed vector (with-open [r (io/reader f)]
                                             (doall (line-seq r))))
            :when (re-seq #"\s+$" line)]
      (println (join ":" [(.getPath f) (inc idx) 0 " Trailing whitespace."])))))

(defn trailing-blank-lines
  "Complain about files ending with blank lines."
  [source-files]
  (doseq [f source-files]
    (with-open [r (io/reader f)]
      (let [lines (map-indexed vector (line-seq r))]
        (doseq [[idx line] (take-while
                             (comp #(re-matches #"^\s*$" %) second)
                             (reverse lines))]
          (println (join ":" [(.getPath f) (inc idx) 0 " Trailing blank line."])))))))

(defn bad-roots
  "Complain about the use of with-redefs."
  [source-files]
  (doseq [f source-files]
    (with-open [r (io/reader f)]
      (let [lines (map-indexed vector (line-seq r))]
        (doseq [[idx line] lines
                :when (re-seq #"\(with-redefs" line)]
          (println (join ":" [(.getPath f) (inc idx) 0 " Use of with-redefs."])))))))

(defn missing-doc-strings
  "Report the percentage of missing doc strings."
  [source-paths verbose]
  (try
    (let [source-files (mapcat #(-> % io/file ns-find/find-sources-in-dir)
                               (flatten source-paths))
          all-namespaces (->> source-files
                              (map load-namespace)
                              (remove nil?))
          all-publics (mapcat read-namespace source-files)
          no-docstrings (->> all-publics
                             (mapcat has-doc)
                             (filter #(= (val %) false)))
          no-ns-doc (->> all-namespaces
                         (mapcat has-ns-doc)
                         (filter #(= (val %) false)))]
      (printf
       "%d/%d [%.2f%%] namespaces have docstrings.\n"
       (- (count all-namespaces) (count no-ns-doc))
       (count all-namespaces)
       (try
         (double
          (* 100 (/ (- (count all-namespaces)
                       (count no-ns-doc))
                    (count all-namespaces))))
         (catch ArithmeticException _ Double/NaN)))
      (printf
       (str "%d/%d [%.2f%%] functions have docstrings.\n"
            (when (not verbose)
              "Use -v to list namespaces/functions without docstrings\n"))
       (- (count all-publics) (count no-docstrings))
       (count all-publics)
       (try
         (double
          (* 100 (/ (- (count all-publics)
                       (count no-docstrings))
                    (count all-publics))))
         (catch ArithmeticException _ Double/NaN)))
      (flush)
      (when verbose
        (println "\nNamespaces without docstrings:")
        (doseq [[ns-name _] (sort no-ns-doc)]
          (println ns-name)))
      (when verbose
        (println "\nMethods without docstrings:")
        (doseq [[method _] (sort no-docstrings)]
          (println method)))
      (or (-> no-docstrings count pos?)
          (-> no-ns-doc count pos?)))
    (catch Throwable t
      (println "Sorry, I wasn't able to read your source files -" t))))

(defn- analysis->args
  [analysis]
  (when (contains? analysis :arglists)
    (into
      []
      cat
      (for [method (get-in analysis [:init :expr :methods])]
        (for [param (:params method)]
          {:binding (:form param)
           :env (select-keys (:env param) [:line :column])})))))

(defn- check-all-arguments-impl
  [f bad-args]
  (let [read-form #(r/read {:eof ::eoferror
                            :read-cond :allow
                            :features #{:clj}}
                           %)
        r (clojure.lang.LineNumberingPushbackReader. (io/reader f))]
    (loop [msgs []
           form (read-form r)]
      (if-not (= form ::eoferror)
        (recur
          (concat msgs
                  (for [bind (analysis->args (ana.jvm/analyze+eval form))
                        :when (contains? bad-args (:binding bind))]
                    (format "%s:%d:%d: \"%s\" is shadowing a core function"
                            (.getPath f)
                            (get-in bind [:env :line])
                            (get-in bind [:env :column])
                            (:binding bind))))
          (read-form r))
        msgs))))

(defn- check-all-arguments
  [source-files]
  (let [core-functions (-> 'clojure.core ns-publics keys set)]
    (doseq [f (filter ns-file/clojure-file? source-files)]
      (doseq [msg (try
                    (check-all-arguments-impl f core-functions)
                    ;; Catch parsing errors
                    (catch Throwable _ []))]
        (println msg)))))

(defn bikeshed
  "Bikesheds your project with totally arbitrary criteria. Returns true if the
  code has been bikeshedded and found wanting."
  [{:keys [max-line-length scan-files]
    :or {max-line-length 80}}]
  (let [source-files scan-files
        long-lines (if (nil? max-line-length)
                     (long-lines source-files)
                     (long-lines source-files :max-line-length max-line-length))
        trailing-whitespace (trailing-whitespace source-files)
        trailing-blank-lines (trailing-blank-lines source-files)
        bad-roots (bad-roots source-files)
        ;; This is quite slow as it searches the whole project! I think a
        ;; little rewrite to indicate information in the current namespace may
        ;; be useful, but I'm uncertain.
        ;; My Bikeshed doesn't care much for docstrings anyway ;)
        ;; bad-methods (missing-doc-strings (or (:source-paths project)) (:verbose options))
        bad-arguments (check-all-arguments source-files)]))

(defn lein-find-files
  [project]
  (remove
    #(starts-with? (.getName %) ".")
    (mapcat
      #(ns-find/find-sources-in-dir
         (io/file %)
         {:extensions [".clj" ".cljs" ".cljc" ".cljx"]})
      (flatten (get-all project :source-paths :test-paths)))))
