(in-package :egl)

(defmacro with-display ((&key (display '(get-display :default-display))
                           (major-var (gensym))
                           (minor-var (gensym))) &body body)
  (alexandria:once-only (display)
    `(multiple-value-bind (,major-var ,minor-var) (initialize ,display)
       (declare (ignorable ,major-var ,minor-var))
       (unwind-protect
            (progn ,@body)
         (terminate ,display)))))

