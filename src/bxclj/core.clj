(ns bxclj.core
  (:use [clojure.pprint])
  (:require [clojure.java.shell :refer [sh]]
            [clojure.data.json :as json])
  (:import [java.util.concurrent TimeUnit]
           [java.util Calendar]))


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

(defn mnemonic-to-seed 
  ([words] (apply (partial bx "mnemonic-to-seed") (.split words " ")))
  ([words pwd] (apply (partial bx "mnemonic-to-seed" "-p" pwd) (.split words " "))))

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
(defn fetch-height [] (read-string (bx "fetch-height")))


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

(defn validate-tx [base-16-tx]
  (bx "validate-tx" base-16-tx))

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
(defn tx-encode [tx-hash index payment-address sats]
  (let [format-fn (partial format "%s:%s")]
    (bx "tx-encode" "-i" (format-fn tx-hash index) "-o" (format-fn payment-address sats)))) 
   
(def-bx input-sign ec-private-key contract base-16-tx)
(def-bx input-set endorsement-script base-16-tx)    
(defn input-validate [ec-public-key contract endorsement base-16-tx]
  (= 
    (bx "input-validate" ec-public-key contract endorsement base-16-tx)
    "The endorsement is valid."))


;;useful stuff

(defn- _root-key-of [hd-private-key derivation-path]
  (if (empty? derivation-path)
    hd-private-key
    (let [index (first derivation-path)
          hardened (.endsWith index "'")]
      (_root-key-of 
        (hd-private hd-private-key (read-string (if hardened (apply str (butlast index)) index)) hardened) 
        (rest derivation-path)))))

(defn root-key-of [hd-private-key derivation-path]
  (_root-key-of hd-private-key (rest (.split derivation-path "/"))))

(defmacro def-wallet-root [name derivation-path]
  `(defn ~name [hd-private-key#]
     (let [root# (root-key-of hd-private-key# ~derivation-path)]
       (fn [ix#]
         (hd-public root# ix#)))))
  
(def-wallet-root ledger-nano-legacy "m/44'/0'/0'") 
(def-wallet-root ledger-nano-segwit "m/49'/0'/0'") 

(def-wallet-root trezor-one "m/44'/0'/0'")


(defn balance-of [hd-private-key hardened]
  (let [the-fn #(hd-private hd-private-key % hardened)]
    (loop [ix 0
           balance 0]
      (let [b (:balance (-> ix the-fn hd-to-public hd-to-ec ec-to-address fetch-balance))
            r (-> b :received read-string)
            s (-> b :spent read-string)]
        (if (<= r 0)
          balance
          (recur (inc ix) (- (+ balance r) s)))))))




(def btc-start-reward  50)
(def halving 210000)  
(def btc-max-circulation 21e6)
(def reward-period-in-minutes 10)
(def rewards-per-day (* 24 (/ 60 reward-period-in-minutes)))
(def rewards-per-year (* 365 rewards-per-day))

(defn blocks-after-halving [h] (mod h halving))
(defn blocks-until-halving [h] (- halving (blocks-after-halving h)))

(defn blocks [height]
  (let [b (map (fn [_] halving) (range (int (/ height halving))))
        bh (blocks-after-halving height)]
    (if (> bh 0)
      (concat b [bh])
      b)))

(defn rewards 
  ([] (rewards btc-start-reward))
  ([r] (lazy-seq (cons r (rewards (/ r 2))))))

(defn block-and-rewards [height]
  (let [b (blocks height)]
    (map (fn [b r] [b r]) b (take (count b) (rewards)))))


(defn block-and-reward-pair 
  [height]
  (let [bh (reverse (block-and-rewards height))]
    (mapcat (fn [[h r]] (map (fn [_] r) (range h))) bh)))

(defn reward-of [height] (-> height  block-and-rewards last second))

(defn circulation-btc [height]
  (long (reduce (fn [acc [b r]] (+ acc (* b r))) 0 (block-and-rewards height))))
                        
(defn stock->flow-of [height]
  (let [now (circulation-btc height)
        inflation (- now (circulation-btc (max (- height rewards-per-year) 0)))
        ]
    (/ now inflation)))

(defn sf-marketcap-of [sf]
  (* (Math/exp 14.6) (Math/pow sf 3.3)))

(defn sf-price-btc-of [height]
  (/ (-> height stock->flow-of sf-marketcap-of) (circulation-btc height)))

(defn height->time [height]
  (+ (* (- height (fetch-height)) (.toMillis (TimeUnit/MINUTES) reward-period-in-minutes)) 
    (System/currentTimeMillis)))

(defn time-millis->date [time]
  (let [c (Calendar/getInstance)]
    (.setTimeInMillis c time)
    {:year (.get c Calendar/YEAR)
     :month (inc (.get c Calendar/MONTH))
     :day (.get c Calendar/DAY_OF_MONTH)}))
  
