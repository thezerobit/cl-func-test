(asdf:operate 'asdf:load-op 'x.fdatatypes)
(asdf:operate 'asdf:load-op 'funds)
(asdf:operate 'asdf:load-op 'fset)

;; interface

(defclass functional-hash () ())

(defgeneric hash-add (m key val))

(defgeneric hash-get (m key))

;; implementations

(defclass xf-map (functional-hash)
  ((internal-map :initform (x.fdatatypes:ftab)
                :initarg :internal-map
                :accessor internal-map)))

(defmethod hash-add ((m xf-map) key val)
  (make-instance 'xf-map
    :internal-map (x.fdatatypes:add (internal-map m) key val)))

(defmethod hash-get ((m xf-map) key)
  (x.fdatatypes:ref (internal-map m) key))



(defclass funds-map (functional-hash)
  ((internal-map :initform (funds:make-dictionary :test #'equal)
                :initarg :internal-map
                :accessor internal-map)))

(defmethod hash-add ((m funds-map) key val)
  (make-instance 'funds-map
    :internal-map (funds:dictionary-add (internal-map m) key val)))

(defmethod hash-get ((m funds-map) key)
  (funds:dictionary-lookup (internal-map m) key))



(defclass fset-map (functional-hash)
  ((internal-map :initform (fset:empty-map)
                :initarg :internal-map
                :accessor internal-map)))

(defmethod hash-add ((m fset-map) key val)
  (make-instance 'fset-map
    :internal-map (fset:with (internal-map m) key val)))

(defmethod hash-get ((m fset-map) key)
  (fset:lookup (internal-map m) key))



;; not actually functional, will mutate original, for testing only
(defclass cl-map (functional-hash)
  ((internal-map :initform (make-hash-table :test #'equal)
                :initarg :internal-map
                :accessor internal-map)))

(defmethod hash-add ((m cl-map) key val)
  (make-instance 'cl-map
    :internal-map (progn (setf (gethash key (internal-map m)) val)
                         (internal-map m))))

(defmethod hash-get ((m cl-map) key)
  (gethash key (internal-map m)))

;; testing

(defun str-key-from-int (i)
  (format nil "key~a" i))

(defun make-big-hash (n klass)
  (let ((test-hash (make-instance klass)))
    (loop for x upto (1- n) do
          (setf test-hash
                (hash-add test-hash (str-key-from-int x) x)))
    test-hash))

(defun lookup-lots (h n)
  (loop for x upto (1- n) do
        (let ((result (hash-get h (str-key-from-int x))))
          (assert (equal result x)))))

(defparameter *hash-classes*
  (list 'xf-map 'funds-map 'fset-map 'cl-map))

(dolist (klass *hash-classes*)
  (format t "Testing ~a...~%" klass)
  (let ((n 100000)
        (big-hash))
    (format t "Building hash with ~D entries.~%" n)
    (time (setq big-hash (make-big-hash n klass)))
    (format t "Doing ~D lookups.~%" n)
    (time (lookup-lots big-hash n)))
  (let ((num-hashes 1000)
        (size 100)
        (hashes))
    (format t "Building ~D hashes with ~D entries each.~%" num-hashes size)
    (time
      (setq hashes
            (loop
              for x upto (1- num-hashes)
              collecting (make-big-hash size klass))))))

