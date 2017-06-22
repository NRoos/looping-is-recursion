(ns looping-is-recursion)

(defn power [base exp]
  (if (= 0 exp)
    1
    (let [helper (fn [curr base round]
                   (if (>= round exp)
                     curr
                     (recur (* curr base) base (+ round 1))))]
      (helper base base 1))))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [helper (fn [x]
                   (if (empty? (rest x))
                     (nth x 0)
                     (recur (rest x))))]
      (helper a-seq))))

(last-element [])      ;=> nil
(last-element [1 2 3]) ;=> 3
(last-element [2 5])   ;=> 5


(defn seq= [seq1 seq2]
  (if (not (= (count seq1) (count seq2)))
    false
    (let [helper (fn [n]
                   (if (empty? seq1)
                     true
                     (if (= (nth seq1 n) (nth seq2 n))
                       (if (= n 1)
                         true
                         (recur (+ n 1)))
                       false)))]
      (helper 0))))


(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [] [])             ;=> true
(seq= [1 2 nil] [1 2])   ;=> false
(seq= [1 4 2] [1 2 4])   ;=> false
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(defn find-first-index [pred a-seq]
  (let [helper (fn [n]
                 (if (pred (nth a-seq n))
                   n
                   (if (= (count a-seq) (+ n 1))
                     nil
                     (recur (+ n 1)))))]
    (if (= (count a-seq) 0)
      nil
      (helper 0))))


(defn equal-to [n]
  (fn [k] (== k n)))


(find-first-index (equal-to 4) [12 2 3 1 2 3 4])

(defn avg [a-seq]
  (let [helper (fn [total n]
                 (if (= (count a-seq) n)
                   (/ total n)
                   (recur (+ total (nth a-seq n)) (+ n 1))))]
    (helper 0 0)))

(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         a-seq a-seq]
    (if (empty? a-seq)
      a-set
      (recur (toggle a-set (first a-seq)) (rest a-seq)))))

(defn parity [a-seq]
  (loop [a-set #{}
         a-seq a-seq]
    (if (empty? a-seq)
      a-set
      (recur (toggle a-set (first a-seq)) (rest a-seq)))))


(defn fast-fibo [n]
  (if (<= n 0)
    0
    (loop [curr 1
           prev 0
           round 1]
      (if (= round n)
        curr
        (recur (+ curr prev) curr (+ round 1))))))

(fast-fibo 92)

(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8


(defn cut-at-repetition [a-seq]
  (loop [used #{}
         b-seq []
         n 0]
    (if (= n (count a-seq))
      b-seq
      (if (contains? used (nth a-seq n))
        b-seq
        (recur (conj used (nth a-seq n)) (conj b-seq (nth a-seq n)) (+ n 1))))))

(count [0 1 2 3 4 5])


(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]
