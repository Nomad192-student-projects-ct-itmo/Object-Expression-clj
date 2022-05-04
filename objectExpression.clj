(defn pget [obj key]
	(cond 
		(contains? obj key) (obj key)
		(contains? obj :proto) (pget (obj :proto) key)
		:else nil))

(defn pcall [obj key & args]
	(apply (pget obj key) obj args))

(defn method [key]
	#(apply pcall % key %&))

(def evaluate 	(method :evaluate))
(def toString 	(method :toString))
(def diff 		(method :diff	 ))

(defn d [& items] 
	(try (apply / items)
		(catch ArithmeticException _
			(/ (first items) 0.0))))

(def n (partial - 0))

(defn Constant [c] 
	{
		:diff 	  (fn [this t] (Constant 0))
		:toString (fn [this] (str c))
		:evaluate (fn [this args] c)})
(defn Variable [x]
	{
		:diff 	  (fn [this t]
			(if (= x t) (Constant 1)
				(Constant 0)))
		:toString (fn [this] x)
		:evaluate (fn [this args] (get args x))})

(defn my-exp [x] (Math/exp x))
(defn my-ln  [x] (Math/log (Math/abs x)))

(defn my-log [b x] (/ (Math/log (Math/abs x)) (Math/log (Math/abs b))))
(defn my-pow [x y] (Math/pow x y))

(defn my-mean [& items] 
	(double (apply + (mapv (partial * (/ 1 (count items))) items))))

(defn my-varn [& items]
	(- 
		(apply + (mapv (partial * (/ 1 (count items))) 
					   (mapv * items items))) 
		(Math/pow (apply my-mean items) 2)))

(defn my-sumexp [& items] 
 	(apply + (mapv (partial #(Math/exp %)) items)))

(defn my-softmax [& items] 
    (/ (Math/exp (nth items 0)) (apply my-sumexp items)))

(def op-str {+			'+
			 -			'-
			 *			'*
			 d			'/
			 n			'negate
			 my-exp		'exp
			 my-ln		'ln
			 my-pow		'pow
			 my-log		'log
			 my-mean	'mean
			 my-varn	'varn
			 my-sumexp	'sumexp
			 my-softmax	'softmax
			})

(declare str-op)

(def ExprPrototype 
	{
		:diff (fn [this t] (let [df (:df this)		
								 args (:items this)]	 
			(df t args)
			))
		:toString (fn [this] 
			(str 
				(apply str 
					"(" 
					(get op-str (:f this)) 
					(if (empty? (:items this)) " ") 
					(mapv (fn [item] (str " " (toString item))) 
						(:items this)))
				")"))
		:evaluate (fn [this args] 
			(apply (:f this) 
				(mapv evaluate 
					(:items this) 
					(iterate identity args))))
	 })

(defn Expr [f df]
	(fn [& items]
		{:proto ExprPrototype
		 :f f
		 :df df
		 :items items}))

(declare Add Subtract Multiply Divide Negate)
(declare Exp Ln)
(declare Pow Log)
(declare Mean Varn)
(declare Sumexp Softmax)

(defn diff-mult [t items] (letfn [(diff-mult-bin [t u v] (Add (Multiply (diff u t) v) (Multiply u (diff v t))))]
	(if (empty? (rest items))
		(diff (first items) t)
		(diff-mult-bin t (first items) (apply Multiply (rest items)))
	)))

(defn diff-dvid [t items] (letfn [(diff-dvid-bin [t u v] (Divide (Subtract (Multiply (diff u t) v) (Multiply u (diff v t))) (Multiply v v)))]
	(if (empty? (rest items))
		(diff-dvid-bin t (Constant 1) (first items))
		(diff-dvid-bin t (first items) (apply Multiply (rest items)))
	)))

(defn diff-add 		[t args] (apply Add 		(mapv diff args  (iterate identity t))))
(defn diff-subtract	[t args] (apply Subtract 	(mapv diff args  (iterate identity t))))
(defn diff-negate 	[t args] (apply Negate 		(mapv diff args  (iterate identity t))))

(defn diff-exp 		[t args] (Multiply (apply Exp args) (diff (first args) t)))
(defn diff-ln		[t args] (Divide (diff (first args) t) (first args)))

(defn diff-pow 		[t args] (Multiply (apply Pow args) (diff (Multiply (second args) (Ln (first args))) t)))
(defn diff-log		[t args] (diff (Divide (Ln (second args)) (Ln (first args))) t))

(defn diff-mean 	[t args]
 (Divide (apply Add (mapv diff args (iterate identity t))) (Constant (count args))) )
(defn diff-varn 	[t args]
 (diff-mean t (mapv (fn [x m] (Multiply (Subtract x m) (Subtract x m))) args (iterate identity (apply Mean args)))) )

(defn diff-sumexp 	[t args] (diff (apply Add (mapv Exp args)) t) )
(defn diff-softmax 	[t args] (diff (Divide (Exp (first args)) (apply Sumexp args)) t) )

(def Add 		(Expr + 			diff-add		))
(def Subtract 	(Expr - 			diff-subtract	))
(def Multiply 	(Expr * 			diff-mult 		))
(def Divide 	(Expr d 			diff-dvid 		))
(def Negate 	(Expr n 			diff-negate 	))
(def Exp 		(Expr my-exp 		diff-exp		))
(def Ln 		(Expr my-ln			diff-ln 		))
(def Pow  		(Expr my-pow		diff-pow		))
(def Log 		(Expr my-log		diff-log 		))
(def Mean 		(Expr my-mean		diff-mean		))
(def Varn 		(Expr my-varn 		diff-varn		))
(def Sumexp 	(Expr my-sumexp		diff-sumexp		))
(def Softmax  	(Expr my-softmax	diff-softmax	))


(def str-op {'+ 		 Add
        	 '- 		 Subtract
         	 '* 		 Multiply
        	 '/ 		 Divide
        	 'negate 	 Negate
			 'exp  		 Exp        	 
			 'ln    	 Ln
			 'pow 		 Pow
			 'log 		 Log
			 'mean 		 Mean
			 'varn   	 Varn
			 'sumexp  	 Sumexp
			 'softmax 	 Softmax
         	})

(defn parse [result]
      (cond
        (number? result) (Constant result)
        (symbol? result) (Variable (str result))
        (list? result) (apply (get str-op (first result)) (mapv parse (rest result)))
      ))

(defn parseObject [line] (parse (read-string line)))