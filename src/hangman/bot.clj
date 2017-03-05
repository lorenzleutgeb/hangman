(ns hangman.bot
  (:gen-class)
  (:require [clojure.string :as s]
            [environ.core :refer [env]]
            [hangman.facebook :as fb]
            [clojure.java.io :as io]))

(def heroku {:app-name (env :heroku-app-name)
             :release-created-at (env :heroku-release-created-at)})

(def heroku-root (str "https://" (get heroku :app-name) ".herokuapp.com"))

(def user-state (atom {}))

; Utility functions for handling the wordlist.

(defn read-lines [rname]
  "Reads lines from a resource file."
  (->> rname
    (clojure.java.io/resource)
    (slurp)
    (clojure.string/split-lines)))

(def wordlist
  "Represents the contents of the wordlist file."
  (read-lines "wordlist.txt"))

(defn random-word []
  "Returns a random word from the wordlist."
  (rand-nth wordlist))

; Utility functions for converting back and forth
; between internal representations of words and
; what is sent to the user.

(defn pimp [word f]
  "Takes a string and a mapping function. Applies the mapping function character wise and then joins everything together separated by spaces."
  (->> word
    (seq)
    (map f)
    (s/join " ")))

(defn to-public [guessed c]
  "Takes a set of guessed character and a character. Returns the caharcter if it is in the set, and an underscore otherwise."
  (if (contains? guessed c) c \_))

(defn mask [generated-word guessed]
  "Takes a word and a set of guessed characters. Pimps the generated word using the to-public transformation with guessed characters."
  (pimp generated-word (partial to-public guessed)))

(defn generated-to-shown [generated-word]
  "Takes a generated word and pimps it using the identity function (basically does nothing but just inserts some spaces between the individual characters of the generated word)."
  (pimp generated-word identity))

; Utility functions that impact game state.

(defn is-correct [guess generated-word]
  "Takes a character (guess) and a word. Returns true iff the character is contained in the word."
  (contains? (set generated-word) guess))

(defn is-finished [generated-word guessed]
  "Takes a word and a set of characters (guesses). Returns true iff all characters of the words are in the set of guesses."
  (every? (partial contains? guessed) (seq generated-word)))

(defn message-to-guesses [message]
  (seq message))

(defn is-new [guess guessed]
  (not (contains? guessed guess)))

(defn state-updater [sender-id]
  (partial assoc @user-state sender-id))

; Interactions.

(defn gallow [n]
  (str heroku-root "/gallows" n))

(defn send-gallow [sender-id n]
  (fb/send-message sender-id (fb/image-message (gallow n))))

(defn on-message [payload]
  (println "on-message payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        message-text (get-in payload [:message :text])
        state (get @user-state (get-in payload [:sender :id]))
        update (state-updater (get-in payload [:sender :id]))]
    ; check core.match for nicer layout, and condp
    (println (str "Message is of length " (count message-text)))
    (cond
      (= (count message-text) 1)
      (let [updated-guesses (conj (get state :guesses) (first (seq message-text)))]
        (do
            (println (str "State is " state))
            (fb/send-message sender-id (fb/text-message (str "OK, carry on: " (mask (get state :word) updated-guesses))))
            (update (assoc state :guesses updated-guesses))))

      ; If no rules apply echo the user's message-text input
      :else
      (fb/send-message sender-id (fb/text-message "Sorry, I do not understand. :(")))))

(defn on-postback [payload]
  (println "on-postback payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        postback (get-in payload [:postback :payload])
        referral (get-in payload [:postback :referral :ref])
        update (state-updater (get-in payload [:sender :id]))
        word (random-word)]
    (cond
      (= postback "GET_STARTED")
      (let [empty-state {:guesses (empty #{}) :word word :errors 0}]
        (do
          (println (str "Updateing state to " empty-state))
          (update empty-state)
          (println (str "Obtained state " (get @user-state sender-id)))
          (fb/send-message sender-id (fb/text-message "Welcome =)"))
          (fb/send-message sender-id (fb/text-message (str "Let's go: " (mask word (empty #{})))))))

      :else
      (fb/send-message sender-id (fb/text-message "Sorry, I don't know how to handle that postback")))))

(defn on-attachments [payload]
  (println "on-attachment payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        attachments (get-in payload [:message :attachments])]
    (fb/send-message sender-id (fb/text-message "Thanks for your attachments :)"))))
