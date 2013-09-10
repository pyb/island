(eval-when (:compile-toplevel :load-toplevel :execute)
;
(require 'sb-bsd-sockets)
(use-package 'sb-bsd-sockets)
;
(require 'cffi)
(require 'bordeaux-threads)
(load "util")
(load "unix")
;
)

(defstruct player
  n
  name
  islands
  gold
  fd
  socket)

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

(defparameter *mine-yield* 2000)

;;;;-------------------------------------------

(defparameter *attacks* nil) ; dynamic

(defparameter *messages* nil)

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
	(message str)
	(values good command)))))

(defun process-command (command player)
  (apply (second (assoc (first command)
			*command-table*
			:test #'member))
	 (rest command)))

(defun process-next-command (player)
  (setf *messages* nil)
  (let ((command (next-command player)))
    (multiple-value-bind (good command)
	(display-and-convert-command command)
      (when good
	(process-command command player)
	(show-messages (reverse *messages*) player)))))

(defun show-messages (messages player)
  (dolist (m messages)
    (write-message m)))


(defun buy (island item qty)
  (when (member island
		(player-islands (current-player)))
    (with-slots (pop mine)
	island
      (bind (item (buy-price buy-pop) sell-price)
	  (assoc item *prices*)
	(if (and item
		   (>= (player-gold (current-player)) (* qty buy-price))
		   (>= pop (* qty buy-pop))
		   (or (<= mine 5) (not (eql 'mine item))))
	  (prog1 t
	    (message "Buying...")
	    (decf (player-gold (current-player)) (* qty buy-price))
	    (decf pop (* qty buy-pop))
	    (incf (slot-value island item)
		  qty))
	  (message "Cannot buy !"))))))

(defun sell (island item qty)
  (when (member island
		(player-islands (current-player)))
    (bind (item (buy-price buy-pop) sell-price)
	(assoc item *prices*)
      (if (and item
		 sell-price
		 (<= qty
		     (slot-value island item)))
	(prog1 t
	  (message "Selling...")
	  (incf (player-gold (current-player)) (* qty sell-price))
	  (decf (slot-value island item)
		qty))
	(message "Could not sell !")))))


(defun message (&rest args)
  (push (apply #'format nil args)
	*messages*))

(defun move (dest &key source n)
  (message "Moving : ~A ~A ~A~%" dest source n)
  (cond ((eql dest source)
	 (message "Same destination !~%"))
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
  



;;-----------------------------------------------------------

(defun display-island (island player)
  (with-slots (n pop agri bato mine) 
      island
    (write-message (format nil "Island ~A " n) player)
    (if (eql player
	     (proprio island))
	(progn
	  (write-message (format nil "pop ~A " pop) player)
	  (write-message (format nil "agri ~A " agri) player)
	  (write-message (format nil "bato ~A " bato) player)
	  (write-message (format nil "mine ~A " mine) player))
	(progn
	  (awhen (proprio island)
	    (write-message (format t "*~A*" (player-name it)) player))))
    (write-message (format nil "~%") player)))

(defun ui-island (islands player)
  (dolist (island islands)
    (display-island island player)))

(defun print-status (&optional (player (current-player)))
  (write-message (format nil "Player *~A*  ---  £~A~%" (player-name player) (player-gold player)) 
		 player)
  (ui-island *islands* player))


;;-----------------------------------------------------------

;; TCP server

(defvar *clients* nil)

(defvar *srv-socket* nil)
(defvar *srv-socket-fd* nil)

(define-constant +server-port+ 76)
(define-constant +tcp-backlog+ 128)

(defvar *server-port* 1076)

(defun transport-init ()
  (setf *srv-socket*    (make-instance 'inet-socket :protocol :tcp :type :stream)
	*srv-socket-fd* (socket-file-descriptor *srv-socket*))
  (setf (non-blocking-mode *srv-socket*)
	t)
  (socket-bind *srv-socket*
	       #(127 0 0 1)
	       *server-port*)
  (socket-listen *srv-socket*
		 +tcp-backlog+))

(defun transport-thread ()
  (let ((fdsets (list (posix-fdset) (posix-fdset) (posix-fdset))))  ; permanent storage for select 
    (loop
       (bind (readfds writefds exceptfds)
	   (posix-select :readfds (list* *srv-socket-fd*
					 (remove nil
						 (mapcar #'player-fd
							 *players*)))
			 :timeout? nil
			 :fdsets fdsets)
	 (cond ((member *srv-socket-fd* readfds) 
		(open-new-connection *srv-socket*))
	       (t
		(dolist (player *players*)
		  (when (member (player-fd player)
				readfds)
		    (read-command player)
		    (process-next-command player)))))))))

(let ((n 0))		      
  (defun open-new-connection (srv-socket)
    (if (> n 1)
	(error "Too many connections")
	(mvbind (socket address port)
	     (socket-accept srv-socket)
	   (when socket
	     (setf (non-blocking-mode socket)
		   t)
	     (format t "Opening new conn with fd ~A~%" (socket-file-descriptor socket))
	     (let ((player (elt *players*
				n)))
	       (setf (player-socket player) socket
		     (player-fd     player) (socket-file-descriptor socket))
	       (incf n)))))))

(defun write-message (message player)
  (socket-send (player-socket player)
	       (format nil "~A~%" message)
	       nil))

(define-constant +max-command-length+ 100)

(defparameter *command* nil)

(defun next-command (player)
  (prog1 *command*
    (setf *command* nil)))

(defun read-command (player)
  (let ((command (socket-receive (player-socket player)
				 nil  ; buffer
				 +max-command-length+
				 :element-type '(unsigned-byte 8))))
    (format t "Command received ~S~%" command)
    (setf *command* command)))
