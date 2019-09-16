(ns bxclj.core
  (:use [clojure.pprint])
  (:require [clojure.java.shell :refer [sh]]
            [clojure.data.json :as json]))


(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))


(defn bx [& args]
  (let [res (apply sh (conj args "bx"))]
    (if (= (:exit res) 0)
      (apply str (butlast (:out res)))
      (throw (IllegalStateException. (:err res))))))


(defmacro def-bx [cmd & args] `(defn ~cmd [~@args] (bx ~(str cmd) ~@args)))

(defn create-json-fn [name]
  (let [bx-fn (partial bx name "-f" "json")]
    (fn [& args]
      (json/read-json (apply bx-fn args)))))

(def-bx hd-new entropy)
(def-bx hd-to-public hd-private-key)
(def-bx mnemonic-new seed)

(defn mnemonic-to-seed [& words] (bx "mnemonic-to-seed" words))

(defn seed [bit-length]
  (bx "seed" "-b" (str bit-length)))

(defn- hd-pub-priv [cmd hd-parent-key index hardened?]
  (if hardened?
    (bx cmd hd-parent-key "-i" (str index) "-d")
    (bx cmd hd-parent-key "-i" (str index))))

(defn hd-private 
  [hd-parent-key index hardened?]
  (hd-pub-priv "hd-private" hd-parent-key index hardened?))

(defn hd-public 
  [hd-parent-key index]
  (hd-pub-priv "hd-public" hd-parent-key index false))

(def-bx hd-to-ec hd-key)
(def-bx ec-to-address ec-public-key)

;;online
(def-bx fetch-height)


(defn fetch-header [height-or-hash]
  (let [the-fn (create-json-fn "fetch-header")] 
    (if (number? height-or-hash)
      (the-fn "-t" (str height-or-hash))
      (the-fn "-s" height-or-hash))))

(defn fetch-balance [payment-address]
  ((create-json-fn "fetch-balance") payment-address))

(defn fetch-history [payment-address]
  ((create-json-fn "fetch-history") payment-address))

(defn fetch-tx [base-16-hash]
  ((create-json-fn "fetch-tx") base-16-hash))

(defn fetch-tx-index [base-16-hash]
  ((create-json-fn "fetch-tx-index") base-16-hash))

(defn fetch-stealth [base-2-filter]
  ((create-json-fn "fetch-stealth") base-2-filter))

;;hash commands
(def-bx sha160 base16-value)
(def-bx sha256 base16-value)
(def-bx sha512 base16-value)
(def-bx ripemd160 base16-value)


;;math
(defn btc-to-satoshi [btc-value] (* 1e8 btc-value))

;;useful stuff

(defn- _root-key-of [hd-public-key derivation-path]
  (if (empty? derivation-path)
    hd-public-key
    (let [index (first derivation-path)]
      (_root-key-of 
        (hd-public hd-public-key (read-string (if (.endsWith index "'") (apply str (butlast index)) index))) 
        (rest derivation-path)))))

(defn root-key-of [hd-public-key derivation-path]
  (_root-key-of hd-public-key (rest (.split derivation-path "/"))))

(defmacro def-wallet-root [name derivation-path]
  `(defn ~name [hd-public-key#]
     (let [root# (root-key-of hd-public-key# ~derivation-path)]
       (fn [ix#]
         (hd-public root# ix#)))))
  
(def-wallet-root ledger-nano "m/44'/0'/0'") 

(def-wallet-root trezor-one "m/44'/0'/0'")




