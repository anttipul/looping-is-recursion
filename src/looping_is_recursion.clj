(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                  (if (zero? n)
                    acc
                  	(recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc n]
                 (if (empty? n)
                   acc
                   (recur (first n) (rest n))))]
  	(helper nil a-seq)))

(defn seq= [seq1 seq2]
  (loop [s1 seq1
         s2 seq2]
    (cond 
      (and (empty? s1) (empty? s2)) true
      (= (first s1) (first s2))
        (seq= (rest s1) (rest s2))
      :else false)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) i
      :else (recur (inc i) (rest seq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [seq a-seq
           sum 0
           c 0]
      (if (empty? seq)
        (/ sum c)
        (recur (rest seq) (+ sum (first seq)) (inc c))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) 
  	(disj a-set elem) 
  	(conj a-set elem)))

(defn parity [a-seq]
  (loop [set #{}
         seq a-seq]
    (if (empty? seq)
      set
      (recur (toggle set (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (loop [n n
         f-2 0
         f-1 1]
    (if (== n 0)
      f-2
      (recur (dec n) f-1 (+ f-2 f-1)))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         res []]
    (if (or (empty? seq) 
    	      (some (partial = (first seq)) res))
      res
      (recur (rest seq) (conj res (first seq)) ))))

