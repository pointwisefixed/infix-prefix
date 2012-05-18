(ns twitchtvhah.from.gary.prefixer)
(use '[clojure.string :only (join split)])

(defn show [exp]
  (println exp))

(defn find-re [token regex]
  (not nil (re-matches regex token)))

(defn is-operator? [token] (and (not= nil token)(find-re token #"[+|-|*|/|(|)]")))
(defn is-positive-number [token] (find-re token #"[0-9]+"))
(defn is-alpha [token] (find-re token #"[a-ZA-Z]"))

(defn is-alphanumeric? [token] 
  (and (not= nil token) (or (is-positive-number token) (is-alpha token))))

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

(def *operators* [")" "*/" "+-","("])

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

(defn create-expression [op ch1 ch2]
  ["(", op, ch1, ch2, ")"])

(defn pop-until [value stack result]
  (let [top-el (first stack)
        n (-2 (count result))]
    (if (= top-el value)
      [(vec (rest stack)) result]
      (pop-until value (vec (rest stack)) (conj (vec (take n result))
                                        (create-expression top-el 
                                                           (nth result (- (count result) 2))
                                                           (nth result (- (count result) 1))
                                                           ))))))


(defn prefix-valid [tokens result [& {:keys [op-stack] :or {op-stack []}}]]
  (if (and (empty? tokens) (not (empty? op-stack)))
    (let[newstack-op (vec (rest op-stack))
         top-el (first op-stack)
         expression (create-expression top-el (nth result (- (count result) 2)) 
                                       (nth result (- (count result) 1)))
         n (-2 (count result))]
      (prefix-valid tokens (conj (vec (take n result)) [expression] newstack-op)))
	  (let [token (first tokens)]
	    (if (is-alphanumeric? token)
	      (prefix-valid (vec (rest tokens)) (conj result token) op-stack)
	      (if (is-operator? token)
	        (if (or (empty? op-stack) (= token "(")) 
	          (prefix-valid (vec (rest tokens)) result (into [token] op-stack))
	          (if (= token ")")
	            (let [[newstack-op  new-result](pop-until "(" op-stack result)]
	            (prefix-valid (vec (rest tokens)) new-result newstack-op))
	           ;; if it's lower precedence than the top of the stack 
            (if (= (precedence? token (first op-stack)) 1)
	              (let [newstack-op (into [token] (vec (rest op-stack)))
	                    top-el (first op-stack)
	                    expression (create-expression top-el 
                                                   (nth result (- (count result) 2)) 
                                                   (nth result (- (count result) 1)))]
	                (prefix-valid (vec (rest tokens)) [expression] newstack-op))
	              (if (<= (precedence? token (first op-stack)) 0)
	                (prefix-valid (vec (rest tokens)) result (into [token] op-stack))
	                (first result)))))
         (first result))))))

(defn prefix-of [tokens]
  (if (is-valid tokens)
    (prefix-valid tokens [])
    (throw (Exception. (format ("Expression %s is invalid", (join " " tokens)))))))

(defn to-prefix [infix-exp]
  (let [tokens (.split " " (trim infix-exp))]
        (if (one-expression? tokens)
          (show tokens)
          (show (prefix-of tokens)))))

(defn reduce-exp [prefix]
  ())
