(in-package #:waste-env)

;;; The defiitions of the sample environments.  To avoid the computation overhead for the
;;; shortest paths algorithm, the SHORTEST-PATH and NEXT-NODES hash tables are precomputed.
;;; This means that they need to be updated if the definition of the array changes.

;;; This is taken from SBCL:
(defun stuff-hash-table (hash-table alist)
  (dolist (x alist)
    (setf (gethash (car x) hash-table) (cdr x)))
  hash-table)

(defun make-waste-env-0 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-1 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (setf (aref world 0 2) 'wall
          (aref world 1 2) 'wall)
    (apply #'make-instance '<waste-env>
           :world-map world 
           initargs)))

(defun make-waste-env-2 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(8 8) :initial-element 'road)))
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-3 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(8 8) :initial-element 'road)))
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-4 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(20 20) :initial-element 'road)))
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-5 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(20 20) :initial-element 'road)))
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-6 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(10 10) :initial-element 'road))
        (walls '((0 3)
                 (1 3) (1 4) (1 5) (1 6) (1 7) (1 9)
                 (2 0) (2 1) (2 3) (2 5)
                 (3 3) (3 5) (3 6) (3 7) (3 8)
                 (4 1) (4 2) (4 3) (4 5)
                 (5 5) (5 7) (5 8) (5 9)
                 (6 0) (6 1) (6 2) (6 3) (6 5)
                 (7 1) (7 5) (7 6)
                 (8 1) (8 3) (8 5) (8 7)
                 (9 3))))
    (mapc (lambda (wall)
            (setf (aref world (first wall) (second wall)) 'wall))
          walls)
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))
