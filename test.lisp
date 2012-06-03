(asdf:operate 'asdf:load-op 'x.fdatatypes)
(asdf:operate 'asdf:load-op 'funds)
(asdf:operate 'asdf:load-op 'fset)


(defun str-key-from-int (i)
  (format nil "key~a" i))

(defun make-big-hash (n fn-mk-hash fn-add-to-hash)
  (let ((test-hash (funcall fn-mk-hash)))
    (loop for x upto (1- n) do
          (setf test-hash
                (funcall fn-add-to-hash test-hash (str-key-from-int x) x)))
    test-hash))

(defun lookup-lots (h n lookup-fn)
  (loop for x upto (1- n) do
        (let ((result (funcall lookup-fn h (str-key-from-int x))))
          (assert (equal result x)))))

(defun x-make-big-hash (n)
  (make-big-hash n #'x.fdatatypes:ftab #'x.fdatatypes:add))

(defun x-lookup-lots (h n)
  (lookup-lots h n #'x.fdatatypes:ref))

(defun funds-make-big-hash (n)
  (make-big-hash
    n #'(lambda () (funds:make-dictionary :test #'equal))
    #'funds:dictionary-add))

(defun funds-lookup-lots (h n)
  (lookup-lots h n #'funds:dictionary-lookup))

(defun fset-make-big-hash (n)
  (make-big-hash
    n #'(lambda () (fset:wb-map))
    #'(lambda (m k v) (fset:with m k v))))

(defun fset-lookup-lots (h n)
  (lookup-lots h n #'fset:lookup))

(defun cl-make-big-hash (n)
  (make-big-hash
    n #'(lambda () (make-hash-table :test #'equal))
    #'(lambda (m k v) (progn (setf (gethash k m) v) m))))

(defun cl-lookup-lots (h n)
  (lookup-lots h n #'(lambda (m k) (gethash k m))))

