(ns twitchtvhah.from.gary.prefixer)
(use '[clojure.string :only (join split trim)])

(defn find-re [token regex]
  (not= nil (re-matches regex token)))

(defn get-op [op]
  (cond (= (trim op) "+" ) #'+
        (= (trim op) "-" ) #'-
        (= (trim op) "/") #'/
        (= (trim op) "*") #'*
        :else nil))

(defn is-operator? [token] (and (not= nil token)(find-re token #"[+|-|*|/|(|)]")))
(defn is-positive-number? [token] (find-re token #"[0-9]+"))
(defn is-alpha [token] (find-re token #"[a-zA-Z]"))

(defn is-alphanumeric? [token] 
  (and (not= nil token) (or (is-positive-number? token) (is-alpha token))))

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

;; returns 0 if equal precedence, 1 if op2 is higher precedence than op1, -1 if op2 is lower precedence than op1
(defn precedence? [op1 op2]
  (let [op1-pos (find-in-operators op1)
        op2-pos (find-in-operators op2)]
    (cond (= op1-pos op2-pos) 0
          (> op1-pos op2-pos) 1
          :else -1)))

(defn create-expression [op ch1 ch2]
  (list op ch1 ch2))

(defn to-stack [top stack]
  (apply list (cons top stack)))

(defn append [lst el]
  (apply list (concat lst (list el))))

(defn pop-until [value stack result]
  (let [top-el (peek stack)
        n (- (count result) 2)]
    (if (= top-el value)
      [(pop stack) result]
      (pop-until value (pop stack) (append (take n result)
                                        (create-expression top-el 
                                                           (nth result (- (count result) 2))
                                                           (nth result (- (count result) 1))
                                                           ))))))


(defn prefix-valid [tokens result op-stack]
;;  (println)
;;  (prn (format "Tokens are %s" tokens)) 
;;  (prn (format "Result is %s " result)) 
;;  (prn (format "Stack is %s" op-stack))
  (if (and (empty? tokens) (not (empty? op-stack)))
    (let[newstack-op (pop op-stack)
         top-el (peek op-stack)
         expression (create-expression top-el (nth result (- (count result) 2)) 
                                       (nth result (- (count result) 1)))
         n (- (count result) 2)]
      (prefix-valid tokens (append (take n result) expression) newstack-op))
	  (let [token (first tokens)]
	    (if (is-alphanumeric? token)
	      (prefix-valid (pop tokens) (append result token) op-stack)
	      (if (is-operator? token)
	        (if (or (empty? op-stack) (= token "(")) 
	          (prefix-valid (pop tokens) result (to-stack token op-stack))
	          (if (= token ")")
	            (let [[newstack-op  new-result] (pop-until "(" op-stack result)]
	            (prefix-valid (pop tokens) new-result newstack-op))
	           ;; if it's lower precedence than the top of the stack 
            (if (= (precedence? token (peek op-stack)) 1)
	              (let [newstack-op (to-stack token (pop op-stack))
	                    top-el (peek op-stack)
	                    expression (create-expression top-el 
                                                   (nth result (- (count result) 2)) 
                                                   (nth result (- (count result) 1)))]
	                (prefix-valid (pop tokens) (list expression) newstack-op))
	              (if (<= (precedence? token (peek op-stack)) 0)
	                (prefix-valid (pop tokens) result (to-stack token op-stack))
	                (first result)))))
         (first result))))))

(defn prefix-of [tokens]
  (if (is-valid tokens)
    (prefix-valid tokens '() '())
    (throw (Exception. (format ("Expression %s is invalid", (join " " tokens)))))))

(defn to-prefix [infix-exp]
  (let [tokens (apply list (split (trim infix-exp) #"\s"))]
        (if (one-expression? tokens)
          (first tokens)
          (prefix-of tokens)))) 

(defn reduce-exp [prefix]  
  (let [firstval (str (first prefix))]
   (if (is-operator? firstval)
     (let [[op arg1 arg2] prefix
           rarg1 (reduce-exp arg1)
           rarg2 (reduce-exp arg2)]
       (if (and (is-positive-number? (str rarg1)) (is-positive-number? (str rarg2)))
         (let [int1 (Integer/parseInt rarg1)
               int2 (Integer/parseInt rarg2)
               opkey (get-op op)]
           (str (eval (list opkey int1 int2))))
         (list op  rarg1  rarg2 )))
     firstval)))

(defn format-prefix [prefix]
  (if(= 3 (count prefix))
    (let [[op arg1 arg2] prefix
          a1 (format-prefix arg1)
          a2 (format-prefix arg2)]
      (str "(" (join " " (list op a1 a2)) ")"))
    prefix))

;; call like this
;; prefixer filename -r

(defn readfile [[filename & flag]] 
  (let [
        has-filename (not= nil filename)
        _ (if (not has-filename) (throw (Exception. "you forgot to enter a filename")))
        exp (first (split (slurp (trim filename)) #"\r\n"))
        reducing (and (not= empty? flag) (= (first flag) "-r"))
        prefix (to-prefix exp)
        result (format-prefix (if reducing (reduce-exp prefix) prefix))]
    (prn result)))

(readfile *command-line-args*)