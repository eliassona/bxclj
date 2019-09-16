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

(defmacro def-json-bx [cmd & args]
  `(defn ~cmd [~@args]
     (let [bx-fn# (partial bx ~(str cmd) "-f" "json")]
         (json/read-json (bx-fn# ~@args)))))


(defn create-json-fn [name]
  (let [bx-fn (partial bx name "-f" "json")]
    (fn [& args]
      (json/read-json (apply bx-fn args)))))


;;wallet
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
(def-bx ec-new seed)
(def-bx ec-to-wif ec-private-key)
(defn ec-to-public 
  ([ec-private-key]
    (ec-to-public ec-private-key false))
  ([ec-private-key uncompressed]
    (if uncompressed
      (bx "ec-to-public" "-u" ec-private-key)
      (bx "ec-to-public" ec-private-key))))
    
(def-bx wif-to-ec wif)


(def-bx qrcode payment-address)

;;encryption commands

(def-bx ec-to-ek passphrase ec-private-key)
(def-bx ek-address passphrase-seed)

;;online
(def-bx fetch-height)


(defn fetch-header [height-or-hash]
  (let [the-fn (create-json-fn "fetch-header")] 
    (if (number? height-or-hash)
      (the-fn "-t" (str height-or-hash))
      (the-fn "-s" height-or-hash))))

(def-json-bx fetch-balance payment-address)

(def-json-bx fetch-history payment-address)

(def-json-bx fetch-tx base-16-hash)

(def-json-bx fetch-tx-index base-16-hash)

(def-json-bx fetch-stealth base-2-filter)

;;hash commands
(def-bx sha160 base16-value)
(def-bx sha256 base16-value)
(def-bx sha512 base16-value)
(def-bx ripemd160 base16-value)
(def-bx base16-decode base-16-value)
(def-bx base16-encode data)
(def-bx base58-decode base-58-value)
(def-bx base58-encode base-16-value)
(def-bx base64-decode base-64-value)
(def-bx base64-encode data)
(def-json-bx base58check-decode base-58-check-value)
(def-bx base58check-encode base-16-value)



;;math
(defn btc-to-satoshi [btc-value] (read-string (bx "btc-to-satoshi" (str btc-value))))
(defn satoshi-to-btc [satoshi-value] (read-string (bx "satoshi-to-btc" (str satoshi-value))))
;;encoding commands

(def-json-bx address-decode payment-address)
(def-bx address-encode ripemd-160)
(def-bx address-embed text)

;;tx commands

(def-bx script-decode base-16-script) 
(def-bx script-encode script) 
(def-bx script-to-address script)
(def-json-bx tx-decode base-16-tx)



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


(defn balance-of [hd-public-key]
  (let [the-fn (partial hd-public hd-public-key)]
    (loop [ix 0
           balance 0]
      (let [b (:balance (-> ix the-fn hd-to-ec ec-to-address fetch-balance))
            r (-> b :received read-string)
            s (-> b :spent read-string)]
        
        (if (<= r 0)
          balance
          (recur (inc ix) (- (+ balance r) s)))))))


