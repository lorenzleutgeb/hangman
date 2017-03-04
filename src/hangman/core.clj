(ns hangman.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.json :refer [wrap-json-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [hangman.facebook :as fb]
            [hangman.bot :as bot]
            ; Dependencies via Heroku Example
            [compojure.handler :refer [site]]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [environ.core :refer [env]]))

(defn respond-gallow [n]
  {:status 200
   :headers {"Content-Type" "image/png"}
   :body (io/input-stream (io/resource (str "gallows/" n ".png")))})

(defn respond-not-found []
  {:status 404
   :headers {"Content-Type" "text/plain"}
   :body "Not Found"})

(defn splash []
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body "Hello Lemming :)"})

(defroutes fb-routes
  (GET "/" [] (splash))
  (POST "/webhook" request
                   (fb/handle-message request bot/on-message bot/on-postback bot/on-attachments)
                   {:status 200})
  (GET "/webhook" request
                  (fb/validate-webhook request))
  (GET "/gallows/:n" [n] (respond-gallow n)))

(def app
  (-> (wrap-defaults fb-routes api-defaults)
      (wrap-keyword-params)
      (wrap-json-params)))

(defn -main [& args]
  (println "Started up"))
