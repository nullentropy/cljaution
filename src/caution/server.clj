(ns caution.server
  (:require [caution.session :as sess]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.server :as hk]))

(def host-page
  (str "<!doctype html>\n"
       "<html><head><meta charset=\"utf-8\"/>"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>"
       "<title>caution</title><style>"
       "html,body{margin:0;height:100%;overflow:hidden;background:#16181d}"
       "canvas{display:block;width:100vw;height:100vh}"
       "</style></head><body><canvas id=\"root\"></canvas>"
       "<script type=\"module\" src=\"/app.js\"></script></body></html>\n"))

(defn- query-params [req]
  (into {}
        (comp (remove str/blank?)
              (map #(str/split % #"=" 2))
              (map (fn [[k v]] [k (some-> v (java.net.URLDecoder/decode "UTF-8"))])))
        (str/split (or (:query-string req) "") #"&")))

(defn- num-param [params k]
  (try (some-> (get params k) Double/parseDouble) (catch Exception _ nil)))

(defn- same-origin?
  [req allowed]
  (let [origin (get-in req [:headers "origin"])]
    (cond
      (str/blank? origin) true
      (some #{"*"} allowed) true
      (seq allowed) (boolean (some #{origin} allowed))
      :else (= (some-> origin (str/replace #"^https?://" ""))
               (get-in req [:headers "host"])))))

;; -- the terminal bundle ------------------------------------------------------------

(defn- read-bundle [path]
  (let [f (io/file path)]
    (when (.exists f) {:body (slurp f) :mtime (.lastModified f)})))

(def ^:private embedded-bundle
  (delay (some-> (io/resource "caution/app.js") slurp)))

(def ^:private embedded-wasm-gz
  (delay (when-let [r (io/resource "caution/term.wasm.gz")]
           (with-open [in (io/input-stream r)
                       out (java.io.ByteArrayOutputStream.)]
             (io/copy in out)
             (.toByteArray out)))))

(def ^:private embedded-wasm-raw
  (delay (when-let [gz @embedded-wasm-gz]
           (with-open [in (java.util.zip.GZIPInputStream.
                           (java.io.ByteArrayInputStream. gz))
                       out (java.io.ByteArrayOutputStream.)]
             (io/copy in out)
             (.toByteArray out)))))

(defn wasm-handler [req]
  (let [gzip? (some-> (get-in req [:headers "accept-encoding"])
                      str/lower-case
                      (str/includes? "gzip"))
        b (if gzip? @embedded-wasm-gz @embedded-wasm-raw)]
    (if b
      {:status 200
       :headers (cond-> {"Content-Type" "application/wasm"
                         "Cache-Control" "max-age=3600"
                         "Vary" "Accept-Encoding"}
                  gzip? (assoc "Content-Encoding" "gzip"))
       :body (java.io.ByteArrayInputStream. b)}
      {:status 404 :headers {"Content-Type" "text/plain"}
       :body "caution: term.wasm.gz not on the classpath\n"})))

(defn- bundle-handler [{:keys [app-js]}]
  (let [cache (atom nil)]
    (fn [_req]
      (let [f (io/file app-js)]
        (cond
          (.exists f)
          (let [c @cache]
            (when (or (nil? c) (not= (:mtime c) (.lastModified f)))
              (reset! cache (read-bundle app-js)))
            {:status 200
             :headers {"Content-Type" "text/javascript; charset=utf-8"}
             :body (:body @cache)})

          @embedded-bundle
          {:status 200
           :headers {"Content-Type" "text/javascript; charset=utf-8"}
           :body @embedded-bundle}

          :else
          {:status 500
           :headers {"Content-Type" "text/plain"}
           :body (str "caution: terminal bundle not found at " app-js
                      " and no caution/app.js on the classpath"
                      "\nbuild it with: go run ./cmd/bundle (in a caution checkout)\n")})))))

;; -- websocket ------------------------------------------------------------------------

(defn- ws-handler [{:keys [view init authorize allowed-origins on-mount on-close]} req]
  (if-not (same-origin? req allowed-origins)
    {:status 403 :body "forbidden"}
    (let [identity (try (when authorize (authorize req))
                        (catch Throwable t {::denied (.getMessage t)}))]
      (if (and (map? identity) (::denied identity))
        (do (println "caution: connection rejected:" (::denied identity))
            {:status 403 :body "forbidden"})
        (let [params (query-params req)
              vw (num-param params "vw")
              vh (num-param params "vh")
              resumed (some-> (get params "resume") sess/lookup)
              resuming? (sess/resumable? resumed identity)
              client-seq (when resuming?
                           (some-> (num-param params "seq") long))
              s (if resuming?
                  (do (println (format "caution: session %d resumed" (:sid @resumed)))
                      resumed)
                  (do (when resumed
                        (println "caution: resume denied - identity mismatch"))
                      (sess/new-session {:view view :init init :identity identity
                                         :request req :vw vw :vh vh
                                         :on-mount on-mount :on-close on-close})))]
          (hk/as-channel req
            {:on-open    (fn [ch]
                           (println (format "caution: session %d connected (%s)"
                                            (:sid @s) (:remote-addr req)))
                           (sess/open! s ch client-seq))
             :on-receive (fn [_ch msg] (sess/receive! s msg))
             :on-close   (fn [ch _status] (sess/close! s ch))}))))))

;; -- routing ------------------------------------------------------------------------

(defn- static-handler [prefix dir]
  (fn [req]
    (let [rel (subs (:uri req) (count prefix))
          f (io/file dir rel)]
      (when (and (.exists f) (.isFile f)
                 ;; no climbing out of the asset directory
                 (str/starts-with? (.getCanonicalPath f)
                                   (.getCanonicalPath (io/file dir))))
        {:status 200 :body f}))))

(defn- make-handler [{:keys [routes static] :as opts}]
  (let [bundle (bundle-handler opts)
        statics (mapv (fn [[prefix dir]] [prefix (static-handler prefix dir)]) static)]
    (fn [req]
      (let [uri (:uri req)]
        (or (when-let [h (get routes uri)] (h req))
            (some (fn [[prefix h]] (when (str/starts-with? uri prefix) (h req))) statics)
            (case uri
              "/"       {:status 200
                         :headers {"Content-Type" "text/html; charset=utf-8"}
                         :body host-page}
              "/app.js" (bundle req)
              "/term.wasm" (wasm-handler req)
              "/ws"     (ws-handler opts req)
              {:status 404 :headers {"Content-Type" "text/plain"} :body "not found"}))))))

(defn serve
  "Run a caution app.

    (serve {:port 8787
            :init (fn [] {:count 0})        ; initial per-session state
            :view (fn [state session] ...)  ; pure: state -> hiccup tree
            :authorize (fn [req] identity)  ; optional; throw to reject
            :routes {\"/health\" (fn [_] {:status 200 :body \"ok\"})}
            :static {\"/assets/\" \"assets\"}})

  Returns a zero-arg fn that stops the server."
  [{:keys [port app-js] :or {port 8787 app-js "../caution/dist/app.js"} :as opts}]
  (let [opts (assoc opts :app-js app-js)
        ;; :max-ws caps client frames. Events are small, and anything near 1MB
        ;; is a bug or an attack, and must not balloon server memory.
        stop (hk/run-server (make-handler opts)
                            {:port port :legacy-return-value? false
                             :max-ws (* 1024 1024)})]
    (println (format "caution: listening on :%d" port))
    (fn [] (hk/server-stop! stop))))
