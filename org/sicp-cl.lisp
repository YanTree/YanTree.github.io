;;ackermann funtion
(defun a(x y)
	   (cond ((= y 0) 0)
		 ((= x 0) (* 2 y))
		 ((= y 1) 2)
		 (t (a (- x 1)
		       (a x (- y 1))))))

;;习题1.3
(defun my-square (x)
  (* x x))
(defun bigger (x y)
  (if (> x y)
      x
      y))
(defun smaller (x y)
  (if (> x y)
      y
      x))
(defun sum-of-square (x y z)
  (+ (my-square (bigger x y))
     (my-square (bigger (smaller x y) z))))

;;实例：换零钱
(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(t (+ (cc amount
		  (- kinds-of-coins 1))
	      (cc (- amount
		     (first-denomination kinds-of-coins))
		  kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(defun count-change (amount)
  (cc amount 5))

(defun move (n a b c)
  (cond ((if (= n 0) 'done))))

;;汉诺塔
(defun hanoi (n from to spare)
  (if (= n 1)
      (list (list from to spare))
      (append
       (hanoi (- n 1) from spare to)
       (hanoi 1 from to spare)
       (hanoi (- n 1) spare to from))))
;;计录移动次数
(defun hanoi-00(from to spare n)
  (if (= n 1)
      1
      (+ (hanoi-00 from spare to (- n 1))
	 (hanoi-00 from to spare 1)
	 (hanoi-00 spare to from (- n 1)))))

(defun sum(a b)
  (if (> a b)
      0
      (+ a
	 (sum (1+ a) b))))

(defun sum-sq(a b)
  (if (> a b)
      0
      (+ (* a a)
	 (sum-sq (1+ a) b))))

(defun pi-sum(a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2)))
	 (pi-sum (+ a 4) b))))

;;a pattern
;;(defun <name> (a b)
;;(if (> a b)
;;  0
;;(+ (<term> a)
;; (<name> (<next> a) b))))
;;牛顿法求平方根
(defun my-sqrt(guess x)
  (if (good-enough? guess x)
      guess
      (my-sqrt (improve guess x)
	       x)))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun good-enough? (guess x)
  (< (* guess (+ 1 (/ 1 x))) 0.00005))

(defun improve (guess x)
  (/ (+ guess (/ x guess)) 2))

(defun square (x)
  (* x x))

(defun sqrt-01(x)
  (defun improve (guess)
    (/ (+ guess (/ x guess)) 2))
  (defun good-enough? (guess)
    (< (abs (- (square guess) x)) 0.001))
  (defun my-sqrt(guess)
    (if (good-enough? guess)
	guess
	(my-sqrt (improve guess))))
  (my-sqrt 1.0))

(defun lifanggen (guess x)
  (if (good-enough-02? guess x)
      guess
      (lifanggen (improve-02 guess x)
	      x)))

(defun lifang(x)
  (* x x x))

(defun good-enough-02? (guess x)
  (< (abs (- (lifang guess) x)) 0.001))

(defun improve-02 (guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))


;;帕斯卡三角形
(defun posi(a b)
  (cond ((< a b) 'error)
	((or (= a b) (= b 1)) 1)
	(t (+ (posi (- a 1) (- b 1))
	      (posi (- a 1) b)))))

(defun posi-00(a b)
  (if (< a b)
      'error
      (/ (n!-01 a) (* (n!-01 b) (n!-01 (- a b))))))

;;row!/col!*(row-col)!

;;n! two ways
;;the first
(defun n! (n)
  (if (< n 2)
      1
      (* n (n! (- n 1)))))

;;the second
(defun n!-01 (n)
  (labels ((n! (p c)
	     (if (> c n)
		 p
		 (n! (* c p)
		     (1+ c)))))
    (n! 1 1)))

;;; 10-ackermann.scm
(defun A (x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (t (A (- x 1)
		(A x (- y 1))))))

;;fib

(defun fib-01 (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (fib-01 (- n 1))
	      (fib-01 (- n 2))))))

(defun fib(n)
  (labels ((fib-item (a b count)
	     (if (= count 0)
		 b
		 (fib-item (+ a b) a (- count 1)))))
    (fib-item 1 0 n)))

(defun fib (n)
  (labels ((fi (a b count)
	     (if (= count n)
		 a
		 (fi b (+ a b) (1+ count)))))
    (fi 0 1 1)))

;;plus f(x-1)+2*f(n-2)+3*f(n-3)
(defun f (n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* (f (- n 2)) 2)
	 (* (f (- n 3)) 3))))


(defun f-i (n)
    (f-iter 2 1 0 0 n))

(defun f-iter (a b c i n)
    (if (= i n)
        c
        (f-iter (+ a (* 2 b) (* 3 c))   ; new a
                a                       ; new b
                b                       ; new c
                (+ i 1)
                n)))

(defun f-02(n)
  (labels ((f-01 (a b c i)
	     (if (= i n)
		 c
		 (f-01 (+ a (* 2 b) (* 3 c)) a b (1+ i)))))
    (f-01 2 1 0 0)))

(defun f (n)
  (labels ((f-00 (a b c count)
                 (if (= count n)
                     a
                   (f-00 b c (+ (* 3 a) (* 2 b) c) (1+ count))))) ;;逻辑的正确性
    (f-00 0 1 2 0)))


;;1-15
(defun cube(x)
  (* x x x))

(defun p (x)
  (- (* 3 x) (* 4 (cube x))))

(defun sine (angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;;求幂
(defun my-expt(b n)
  (if (= n 0)
      1
      (* b (- n 1))))

(defun expt-01(b n)
  (labels ((expt-00 (base count)
	     (if (= count n)
		 base
		 (expt-00 (* base b) (1+ count)))))
    (expt-00 b 1)))

(defun expt-03(b n)
  (labels ((expt-item (product count)
	     (if (= count 0)
		 product
		 (expt-item (* b product) (1+ count)))))
    (expt-item b 1)))

(defun fast-expt(b n)
  (cond((= n 0) 1)
       ((evenp n) (square (fast-expt b (/ n 2))))
       (t (* b (fast-expt b (- n 1))))))

(defun expt-02(b n)
  (labels ((expt-00 (product count)
	     (cond ((= count n) product)
		   ((and (evenp count) (< (* 2 count) n)) (expt-00 (* product product) (* 2 count)))
		   (t (expt-00 (* b product) (1+ count))))))
    (expt-00 1 0)))

(defun fast-expt-00(b n)
  (labels ((fast-expt-01 (b count product)
	     (cond ((= count 0) product)
		   ((evenp count) (fast-expt-01 (square b) (/ count 2) product))
		   ((oddp count) (fast-expt-01 b (- n 1) (* b product))))))
    (fast-expt-01 b n 1)))

(defun fast-expt (b n)
  (expt-iter b n 1))

(defun expt-iter (b n a)
  (cond ((= n 0)
	 a)
	((evenp n)
	 (expt-iter (square b)
		    (/ n 2)
		    a))
	((oddp n)
	 (expt-iter b
		    (- n 1)
		    (* b a)))))

(defun fast (b n)
  (labels ((fast00 (bi count a)
	     (cond ((= count 0) a)
		   ((evenp count) (fast00 (square bi)
					  (/ count 2)
					  a))
		   ((oddp count) (fast00 bi
					 (- count 1)
					 (* bi a))))))
    (fast00 b n 1)))

(defun double(x)
  (* 2 x))

(defun halve(x)
  (if(evenp x)
     (/ x 2)))

(defun fast-plus())

(defun mi(b n)
  (labels ((m (bi a count)
	     (cond ((= count 0) a)
		   ((evenp count) (m (square bi) a (/ count 2)))
		   ((oddp count) (m bi (* a bi) (- count 1))))))
    (m b 1 n)))

(defun cheng(m n)
  (labels ((eng (mi a count)
	     (cond((= count 0) a)
		  ((evenp count) (eng (* 2 mi) a (/ count 2)))
		  ((oddp count) (eng mi (+ a mi) (- count 1))))))
    (eng m 0 n)))
;;最大公约数
(defun my-gcd (a b)   ;;gcd已经内置在common lisp中
  (if (= b 0)
      a
      (gcd b (rem a b))))

;;判断素数
(defun panduan(n)
  )
