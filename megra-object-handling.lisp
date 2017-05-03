;;(require 'closer-mop)

;; the slots of the basic event class should of course not be overwritten ...
;; manual makeshift solution
(defparameter *protected-slots* '(source value-combine-function tags backends))

;; GENERIC

;; slots are equal if their name is equal ... period.
(defun slot-eq (a b)
  (eq (slot-definition-name a) (slot-definition-name b)))

;; not quite sure why this works, but it does ... 
;; http://stackoverflow.com/questions/17002816/lisp-clos-adding-a-slot-to-the-process-class
(defun direct-slot-defn->initarg (slot-defn)
  (list :name (sb-mop::slot-definition-name slot-defn)
        :readers (sb-mop::slot-definition-readers slot-defn)
        :writers (sb-mop::slot-definition-writers slot-defn)
        :initform (sb-mop::slot-definition-initform slot-defn)
        :initargs (sb-mop::slot-definition-initargs slot-defn)
        :initfunction (sb-mop::slot-definition-initfunction slot-defn)))

(defun add-slot-to-class (class name &key (initform nil) accessors readers writers
				       initargs (initfunction (constantly nil)))
  (check-type class symbol)
  (let ((new-slots (list (list :name name
                               :readers (union accessors readers)
                               :writers (union writers
                                               (mapcar #'(lambda (x)
                                                           (list 'setf x))
                                                       accessors)
                                               :test #'equal)
                               :initform initform
                               :initargs initargs
                               :initfunction initfunction))))
    (dolist (slot-defn (sb-mop::class-direct-slots (find-class class)))
      (push (direct-slot-defn->initarg slot-defn)
            new-slots))
    (sb-mop::ensure-class class :direct-slots new-slots)))

