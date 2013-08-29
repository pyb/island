(load "util")

(defstruct player
  n
  name
  islands
  gold)

(defclass island ()
  ((n :initarg :n)
   (pop :initform 0)
   (bato :initform 0)
   (agri :initform 100)
   (mine :initform 0)))

(defmethod print-object ((i island) stream)
  (format stream "Island ~A ~A (p~A b~A a~A m~A)" 
	  (slot-value i 'n)
	  (awhen (proprio i)
	    (player-name it))
	  (slot-value i 'pop)
	  (slot-value i 'bato)
	  (slot-value i 'agri)
	  (slot-value i 'mine)))

(defun generate-islands (n)
  (let (islands)
    (dotimes (i n)
      (push (make-instance 'island :n (1+ i))
	    islands))
    (nreverse islands)))
			 
(defparameter *islands*
  (generate-islands 20))
  
(defparameter *player-list* 
  (list (make-player :name "Alain" :n 1 :gold 10000 :islands (list (first *islands*)))
	(make-player :name "Pyb" :n 2 :gold 10000 :islands (list (second *islands*)))))

(defparameter *players*
  *player-list*)

(defun current-player ()
  (first *players*))

(defun proprio (island)
  (find island *player-list* :key #'player-islands :test #'member))

(defun display-island (island player)
  (with-slots (n pop agri bato mine) 
      island
    (format t "Island ~A " n)
    (if (eql player
	     (proprio island))
	(progn
	  (format t "pop ~A " pop)
	  (format t "agri ~A " agri)
	  (format t "bato ~A " bato)
	  (format t "mine ~A " mine))
	(progn
	  (awhen (proprio island)
	    (format t "*~A*" (player-name it)))))
    (terpri)))
  
(defun ui-island (islands player)
  (dolist (island islands)
    (display-island island player)))

(defun ui (&optional (player (current-player)))
  (format t "Player *~A*  ---  £~A~%" (player-name player) (player-gold player))
  (ui-island *islands* player))

(defun find-island (n)
  (find n *islands*
	:key (lambda (i)
	       (slot-value i 'n))))

(defun init (&optional (players *player-list*))
  (setf *players* players)
  (dolist (p players)
    (let ((i (first (player-islands p))))
      (with-slots (pop bato)
	  i
	(setf pop 100
	      bato 20)))))


;;;;-------------------------------------------

(defparameter *prices*
  '((pop  (500 0) nil)
    (mine (3000 10) nil)
    (bato (100 0) 50)
    (agri (50 0) 25)))

(defparameter *merc-cost* 500)
    


;;;;-------------------------------------------

(defparameter *attacks* nil) ; dynamic

(defun display-and-convert-command (command)
  (when (listp command)
    (let ((c (first command))
	  (dest (second command))
	  (a3 (third command))
	  (a4 (fourth command))
	  (good t))
      (let ((str
	     (case c
	       ((e end) "End turn")
	       ((b buy) (if a4
			    (prog1
				(format nil "Buy ~A ~A on island ~A" a4 a3 dest)
			      (setf (second command)
				    (find-island dest)))
			    (progn (setf good nil) "Buy what ?")))
	       ((s sell) (if a4 
			     (prog1
				 (format nil "Sell ~A ~A from island ~A" a4 a3 dest)
			       (setf (second command)
				     (find-island dest)))
			     (progn (setf good nil) "Sell what ?")))
	       ((m move) (if dest 
			     (if a4
				 (prog1
				     (format nil "Move/attack with ~A troops from ~A to ~A" a4 a3 dest)
				   (setf command `(move ,(find-island dest) :source ,(find-island a3) :n ,a4)))
				 (let ((n a3))
				   (if n
				       (prog1
					   (format nil "~A mercs to island ~A" n dest )
					 (setf command `(move ,(find-island dest) :n ,n)))
				       (progn (setf good nil) "How many mercs?"))))
			     (progn (setf good nil) "No destination")))
	       (t (setf good nil)))))
	(values good str command)))))

(defun next-command (player)
  (format t "What now ? (Buy / Sell / Move / End) ")
  (let ((command (read-from-string (concatenate 'string 
						"(" (read-line) ")"))))
    (Format t "C : ~A~%" command)
    (multiple-value-bind (good str command)
	(display-and-convert-command command)
      (format t "~A (~A)~%" str command)
      (when good
	(format t "Are you sure (y/n) ?")
	(when (eql 'Y (read))
	  (apply (second (assoc (first command)
				*command-table*
				:test #'member))
		   (rest command)))))))

(defun buy (island item qty)
  (when (member island
		(player-islands (current-player)))
    (with-slots (pop mine)
	island
      (bind (item (buy-price buy-pop) sell-price)
	  (assoc item *prices*)
	(when (and item
		   (>= (player-gold (current-player)) (* qty buy-price))
		   (>= pop (* qty buy-pop))
		   (or (<= mine 5) (not (eql 'mine item))))
	  (prog1 t
	    (decf (player-gold (current-player)) (* qty buy-price))
	    (decf pop (* qty buy-pop))
	    (incf (slot-value island item)
		  qty)))))))
	      
(defun sell (island item qty)
  (when (member island
		(player-islands (current-player)))
    (bind (item (buy-price buy-pop) sell-price)
	(assoc item *prices*)
      (when (and item
		 sell-price
		 (<= qty
		     (slot-value island item)))
	(prog1 t
	  (incf (player-gold (current-player)) (* qty sell-price))
	  (decf (slot-value island item)
		qty))))))

(defun move (dest &key source n)
  (format t "Moving : ~A ~A ~A~%" dest source n)
  (cond ((eql dest source)
	 (format t "Same destination~%"))
	((null source)
	 (when (>= (player-gold (current-player)) (* n *merc-cost*))
	   (decf   (player-gold (current-player)) (* n *merc-cost*))
	   (push (list dest (current-player) n 0)
		 *attacks*)))
	(t (when (member source
			 (player-islands (current-player)))
	     (with-slots (pop bato)
		 source
	       (when (and (>= pop n)
			  (>= bato n))
		 (decf bato n)
		 (decf pop n)
		 (push (list dest (current-player) n n)
		       *attacks*)))))))
 
(defun end ()
  (prog1 t
    (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%")
    (format t "~A , Please pass to next player (Y)~%" (player-name (current-player)))
    (while (not (eql 'Y (read))))
    (setf *players*
	  (rest *players*))
    (unless *players*
      (format t "About to resolve conflicts... (Y)~%") 
      (while (not (eql 'Y (read))))
      (resolve-attacks *attacks*)
      (update-resources *islands*)
      (dolist (p *player-list*)
	(unless (player-islands p)
	  (destroy-player p)))
      (setf *attacks* nil)
      (setf *players*
	    *player-list*)
      (format t "Next turn (Y)~%") 
      (while (not (eql 'Y (read)))))))


(defparameter *mine-yield* 2000)
	
(defun update-resources (islands)
  (dolist (i islands)
    (with-slots (pop mine agri)
	i
      (awhen (proprio i)
	 (incf (player-gold it)
	       (* mine *mine-yield*))
	 (setf pop
	       (min (floor (* 1.1 pop))
		    (floor (* 1.1 agri))))))))

(defun merge-attacks (at1 at2)
  (list (first at1) ; dest
	(second at2) ; player
	(+ (third at1) (third at2)) ; n
	(+ (fourth at1) (fourth at2)))) ; bato

;; should be on same island
(defun merge-all-attacks (group)
  (when group
    (let ((at1 (first group)))
      (let ((at2 (find (second at1)
		       (rest group)
		       :key #'second)))
	(if at2
	    (merge-all-attacks (cons (merge-attacks at1 at2)
				     (remove at2 (rest group))))
	    (cons at1
		  (merge-all-attacks (rest group))))))))
  ;; resolve fights and moves
  
(defparameter *command-table*
  `(((b buy) ,#'buy)
    ((s sell) ,#'sell)
    ((m move a attack) ,#'move)
    ((e end) ,#'end)))



;;;-------------------------

;; resolve attacks including on empty islands (ie conquests)

;; For 1 island
(defun resolve-fight (island attacks) ; attacks = list of  (dest player n bato?)
  (format t "Merging ~A~%" attacks)
  (let ((attacks (merge-all-attacks attacks)))
    (format t "Resolving ~A~%" attacks)
    (with-slots (pop bato) 
	island
      (let ((attacks (sort (mapcar #'rest attacks)
			   #'> :key #'second))
	    (defense pop)
	    (defender (proprio island)))
	(let ((winner (if (and (second attacks)
			       (= (second (first attacks))
				  (second (second attacks))))  ; attaques egales
			  nil
			  (first (first attacks))))
	      (total (reduce #'+
			     attacks
			     :key #'second)))
	  (format t "winner ~A total ~A defense ~A defender ~A~%" winner total defense defender)
	  (if (<= total defense)
	      (progn
		(decf pop
		      total))
	      (progn
		(if (null winner)
		    (progn
		      (setf pop 0))
		    (progn
		      (when defender
			(setf (player-islands defender)
			      (remove island
				      (player-islands defender))))
		      (setf (player-islands winner)
			    (cons island
				  (player-islands winner)))
		      (let ((second-attack (aif (second attacks)
						(second it)
						0))
			    (attack-pop (second (first attacks))))
			(setf pop
			      (min (- total defense)
				   (- attack-pop second-attack)))
			(incf bato
			      pop)))))))))))

(defun regroup (list &key (key #'identity))
  (when list
    (let ((item (funcall key (first list))))
      (cons (remove item list :key key :test-not #'eql)
	    (regroup (remove item list :key key) :key key)))))

;;attack : (dest player n bato?)
;; Group attacks by island, merge from same attacker
(defun resolve-attacks (attacks)
  (format t "Grouping ~A~%" attacks)
  (dolist (attack-group (regroup attacks :key #'first))
    (let ((island (first (first attack-group))))
      (resolve-fight island attack-group))))

(defun destroy-player (p)
  (format t "Player ~A just died.~%" p)
  (setf *players* (remove p *players*)
	*player-list* (remove p *player-list*)))
  
(defun game ()
  (loop 
     (ui (current-player))
     (next-command (current-player))
     (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%")))

