(ns twitchtvhah.from.gary.prefixer)
 

(defn tokenize [fst & more & {:keys [start] :or {start 0}}]
  (let [[token i] (parse-infix-token fst start)]
    (cond ((nil token) nil)
          ((nil i) (list token))
          (:else (cons token (tokenize fst i))))))