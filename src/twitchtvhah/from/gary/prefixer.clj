(ns twitchtvhah.from.gary.prefixer)
(use '[clojure.string :only (join split)])
   
(defn one-expression? [tokens]
  (if (= 1 (count tokens))
    (let [ch (nth tokens 0)]
      (if (Character/isLetterOrDigit ch)
        true
        (throw (Exception. (format ("This expression has only one element %s that it's not alphanumeric, only operators", ch))))))
    false))


(defn show [exp]
  (println exp))

(defn find-re [token regex]
  (not nil (re-matches regex token)))

(defn is-operator? [token] (find-re token #"[+|-|*|/|(|)]"))
(defn is-positive-number [token] (find-re token #"[0-9]+"))
(defn is-alpha [token] (find-re token #"[a-ZA-Z]"))

(defn is-alphanumeric? [token] 
  (or (is-positive-number token) (is-alpha token)))
  
(defn is-valid [tokens]
  (map (fn[token]
         (if (or (is-alphanumeric? token) (is-operator? token))
           true
           false)) tokens))

(def *operators* ["()" "*/" "+-"])

(defn find-in-operators [op]
  (let [results (map (fn[string] (> (.indexOf string (str op)) -1)))
        result-pos (.indexOf results true)]
    result-pos))
    

;; returns 0 if equal precedence, 1 if op2 is higher precedence than op1, -1 if op2 is lower precedence than op1
(defn precedence? [op1 op2]
  (let [op1-pos (find-in-operators op1)
        op2-pos (find-in-operators op2)]
    (cond (= op1-pos op2-pos) 0
          (> op1-pos op2-pos) 1
          :else -1)))

(defn prefix-valid [tokens]
  (infix is a buffer)
  (for all tokens
    (if is an operator
      push token the stack
     otherwise
      put the token in the infix collection
      
  )

(defn prefix-of [tokens]
  (if (is-valid tokens)
    (prefix-valid tokens)
    (throw (Exception. (format ("Expression %s is invalid", (join " " tokens)))))))

(defn to-prefix [infix-exp]
  (let [tokens (.split " " (trim infix-exp))]
        (if (one-expression? tokens)
          (show tokens)
          (show (prefix-of tokens)))))

