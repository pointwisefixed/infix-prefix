(ns twitchtvhah.from.gary.prefixerm)

(defn find-re [token regex]
  (not (re-matches regex token)))
(defn is-operator? [token] (find-re token #"[+|-|*|/|(|)]"))
(defn is-positive-number [token] (find-re token #"[0-9]+"))
(defn is-alpha [token] (find-re token #"[a-zA-Z]"))

(defn is-alphanumeric? [token] 
  (or (is-positive-number token) (is-alpha token)))

(defn one-expression? [tokens]
  (if (= 1 (count tokens))
    (let [token (nth tokens 0)]
      (if (is-alphanumeric? token)
        true
        (throw (Exception. (format ("This expression has only one element %s that it's not alphanumeric, only operators", token))))))
    false))

(defn is-valid [tokens]
  (map (fn[token]
         (if (or (is-alphanumeric? token) (is-operator? token))
           true
           false)) tokens))

(def operators [")" "*/" "+-","("])

(defn find-in-operators [op]
  (let [results (map (fn[string] (> (.indexOf string (str op)) -1)) operators)
        result-pos (.indexOf results true)]
    result-pos))

;; returns 0 if equal precedence, 
;; 1 if op2 is higher precedence than op1, 
;; -1 if op2 is lower precedence than op1
(defn precedence? [op1 op2]
  (let [op1-pos (find-in-operators op1)
        op2-pos (find-in-operators op2)]
    (cond (= op1-pos op2-pos) 0
          (> op1-pos op2-pos) 1
          :else -1)))

(defn create-expression [op ch1 ch2]
  ["(", op, ch1, ch2, ")"])

(defn pop-until [value stack result]
  (let [top-el (peek stack)
        n (-2 (count result))]
    (if (= top-el value)
      [stack result]
      (pop-until value (pop stack) (conj (take n result) 
                                        (create-expression top-el (first (rest result))
                                                           (first (rest (rest result)))))))))


;;(one-expression? ["3"])
;;(one-expression? ["(","3","+","3",")"])

