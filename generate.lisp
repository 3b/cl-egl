;;; rough tool for generating bindings from
;;; https://www.khronos.org/registry/EGL/api/egl.xml

;;; tries to generate output that doesn't need too much editing, but
;;; not 100% there.

(ql:quickload '(alexandria cl-ppcre split-sequence cxml xpath cxml-stp
                cl-json))

(defvar *xml* (cxml:parse-file
               (asdf:system-relative-pathname 'cl-egl "egl.xml")
               (cxml:make-whitespace-normalizer
                (stp:make-builder))))

;; these are defined manually, but check to see if the list changed
;; and we need manual fixes
(defvar *base-types*
  '("khrplatform"
    "eglplatform"
    "khronos_utime_nanoseconds_t"
    "khronos_stime_nanoseconds_t"
    "khronos_uint64_t"
    "khronos_ssize_t"
    "EGLNativeDisplayType"
    "EGLNativePixmapType"
    "EGLNativeWindowType"
    "EGLint"
    "NativeDisplayType"
    "NativePixmapType"
    "NativeWindowType"
    "Bool"))

(defvar *builtin-types*
  (alexandria:plist-hash-table
   '("unsigned int" ":unsigned-int"
     "int" ":int"
     "void" ":void"
     "void *" "(:pointer :void)"
     "const void" ":void"
     "__eglMustCastToProperFunctionPointerType" ":pointer")
   :test 'equal))

(defparameter *struct-types* nil)

(let ((new-types (set-difference
                  (xpath:map-node-set->list
                   'xpath:string-value
                   (xpath:evaluate "/registry/types/type/@name" *xml*))
                  *base-types*
                  :test 'string=)))
  (when new-types
    (error "egl.xml has unknown types: ~s" new-types)))


(defparameter *types* nil)

(defun trim (s)
  (string-trim '(#\space #\newline #\tab #\;) s))

(defun map-type (type)
  (or (gethash type *builtin-types*)
      (when (member type *struct-types* :test 'string=)
        (format nil "(:struct ~a)" type))
      type))

(defun parse-slot (s)
  (cond
    ((position #\[ s)
     (error "arrays in structs not supported yet"))
    ((position #\* s)
     (let ((p (position #\* s :from-end t)))
       `(,(trim (subseq s (1+ p)))
         (":pointer" ,(map-type (trim (subseq s 0 p)))))))
    (t
     (let ((s (mapcar 'trim (ppcre:split "\\s" s))))
       `(,(second s)
         ,(map-type (first s)))))))

(defun parse-struct (type)
  (or (ppcre:register-groups-bind (slots)
          ("^struct\\s+\\w+\\s*{([^}]+)};$" type)
        (let ((s (split-sequence:split-sequence #\; slots)))
          (loop for i in s
                for trimmed = (trim i)
                unless (string= trimmed "")
                  collect (parse-slot trimmed))))
      (list)))

(defun parse-func (type)
  (list ":pointer" (format nil "#| ~a |#" type)))

(defun parse-typedef (type name)
  (setf type (trim (subseq type (length "typedef"))))
  (cond
    ((position #\( type)
     (list name (parse-func type)))
    ((position #\[ type)
     (error "arrays in typedefs not supported yet"))
    ((position #\* type)
     (let ((p (position #\* type :from-end t)))
       (assert (= 1 (count #\* type)))
       `(,(trim (subseq type (1+ p)))
         (":pointer" ,(map-type (trim (subseq type 0 p)))))))
    (t
     (let ((p (position #\space type :from-end t)))
       `(,(trim (subseq type (1+ p)))
         ,(map-type (trim (subseq type 0 p))))))))


(loop for (n type) in (xpath:map-node-set->list
                       (lambda (a)
                         (list (xpath:string-value
                                (xpath:evaluate "name" a))
                               (xpath:string-value a)))
                       (xpath:evaluate "/registry/types/type/name/.." *xml*))
      for tag = (subseq type 0 (position #\space type))
      do (assert (member tag '("struct" "typedef") :test 'string=))
         (when (string= tag "struct")
           (pushnew n *struct-types* :test 'string=))
         (if (string= tag "struct")
             (format t "(defcstruct ~a~{~%  ~a~})~%~%"
                     n
                     (parse-struct type))
             (format t "(defctype ~{~a ~a~})~%~%" (parse-typedef type n))))


(defun parse-value (v)
  (cond
    ((alexandria:starts-with-subseq "0x" v)
     (parse-integer v :start 2 :radix 16))
    ((ignore-errors (parse-integer v)))
    ((string= v "EGL_CAST(EGLint,-1)")
     -1)
    ((string= v "EGL_CAST(EGLContext,0)")
     0)
    ((string= v "EGL_CAST(EGLDeviceEXT,0)")
     0)
    ((string= v "EGL_CAST(EGLDisplay,0)")
     0)
    ((string= v "EGL_CAST(EGLImage,0)")
     0)
    ((string= v "EGL_CAST(EGLImageKHR,0)")
     0)
    ((string= v "EGL_CAST(EGLNativeDisplayType,0)")
     0)
    ((string= v "EGL_CAST(EGLNativeFileDescriptorKHR,-1)")
     -1)
    ((string= v "EGL_CAST(EGLOutputLayerEXT,0)")
     0)
    ((string= v "EGL_CAST(EGLOutputPortEXT,0)")
     0)
    ((string= v "EGL_CAST(EGLStreamKHR,0)")
     0)
    ((string= v "EGL_CAST(EGLSurface,0)")
     0)
    ((string= v "EGL_CAST(EGLSync,0)")
     0)
    ((string= v "EGL_CAST(EGLSyncKHR,0)")
     0)
    ((string= v "EGL_CAST(EGLSyncNV,0)")
     0)
    ((string= v "EGL_CAST(EGLConfig,0)")
     0)
    ((string= v "EGL_CAST(EGLnsecsANDROID,-2)")
     -2)
    ((string= v "EGL_CAST(EGLnsecsANDROID,-1)")
     -1)
    (v (break v))))

(defun trim-enum (s)
  (setf s (trim s))
  (when (alexandria:starts-with-subseq "EGL_" s)
    (setf s (subseq s 4)))
  (substitute #\- #\_ s))

(loop with egl = nil
      for (ns type comment enums)
        in (xpath:map-node-set->list
            (lambda (a)
              (list (xpath:string-value
                     (xpath:evaluate "@namespace" a))
                    (xpath:string-value
                     (xpath:evaluate "@type" a))
                    (xpath:string-value
                     (xpath:evaluate "@comment" a))
                    (xpath:map-node-set->list
                     (lambda (b)
                       (list (parse-value
                              (xpath:string-value
                               (xpath:evaluate "@value" b)))
                             (xpath:string-value
                              (xpath:evaluate "@name" b))))
                     (xpath:evaluate "enum" a))))
            (xpath:evaluate "/registry/enums" *xml*))
      do (assert (member type '("bitmask" "") :test 'string=))
      when (string= type "bitmask")
           (format t "(defbitfield ~a" ns)
           (loop for (v e) in (sort enums '< :key 'car)
                 do (format t "~%  (:~(~a~) #x~x)" (trim-enum e) v))
           (format t ")~%~%")
      when (string= type "")
        do (assert (string= ns "EGL"))
           (setf egl (append enums egl))
      finally
         (progn
           (format t "(defcenum (eglenum EGLint)")
           (loop for (v e) in (sort egl '< :key 'car)
                 do (format t "~%  (:~(~a~) #x~x)" (trim-enum e) v))
           (format t ")~%")))




(defun parse-param (a)
  (let* ((sv (xpath:string-value a))
         (pointer (position #\* sv))
         (text1 (xpath:string-value (xpath:evaluate "text()[1]" a)))
         (ptype (xpath:string-value (xpath:evaluate "ptype" a)))
         (text2 (xpath:string-value (xpath:evaluate "text()[2]" a)))
         (name (xpath:string-value (xpath:evaluate "name" a))))
    (when (string= ptype "")
      (setf ptype text1))
    (when pointer
      (assert (or (string= (trim text2) "*")
                  (string= (trim text1) "*")
                  (char= #\* (char ptype (1- (length ptype))))))
      (when (char= #\* (char ptype (1- (length ptype))))
        (setf ptype (trim (subseq ptype 0 (1- (length ptype))))))
      (when (or (string= ptype "char")
                (string= ptype "const char"))

        (setf pointer nil)
        (setf ptype ":string")))

    (assert (not (position #\[ sv)))
    (list name
          (if pointer
              (list ":pointer" (map-type ptype))
              (map-type ptype)))))

(defparameter *fix-arg-types*
  (alexandria:plist-hash-table
   '("eglClientWaitSync" (2 "EGLSyncFlagsKHR")
     "eglClientWaitSyncKHR" (2 "EGLSyncFlagsKHR")
     "eglClientWaitSyncNV" (1 "EGLSyncFlagsNV")
     "eglWaitSync" (2 "EGLSyncFlagsKHR")
     "eglWaitSyncKHR" (2 "EGLSyncFlagsKHR")
     "eglWaitSyncNV" (1 "EGLSyncFlagsKHR")
     "eglSurfaceAttrib" (2 "EGLEnum")
     "eglQueryString" (1 "EGLEnum")
     "eglGetConfigAttrib" (2 "EGLEnum")
     "eglQuerySurface" (2 "EGLEnum")
     )
   :test 'equal))

(defun fix-args-types (n p)
  (let ((f (gethash n *fix-arg-types*)))
    (if f
        (let ((p (copy-seq p)))
          (loop for (i type) on f by #'cddr
                do (setf (elt p i)
                         (list (first (elt p i)) type)))
          p)
        p)))

(defun parse-function (a)
  (let* ((proto (xpath:first-node
                 (xpath:evaluate "proto" a)))
         (pointer-ret (position #\* (xpath:string-value proto)))
         (ret (xpath:string-value (xpath:evaluate "ptype" proto)))
         (name (xpath:string-value (xpath:evaluate "name" proto)))
         (params (xpath:evaluate "param" a)))
    (when (string= ret "")
      (setf ret (trim (xpath:string-value (xpath:evaluate "text()[1]" proto)))))

    (when pointer-ret
      (assert (string= "const char *"
                       (trim
                        (first (xpath:map-node-set->list
                                'xpath:string-value
                                (xpath:evaluate "proto//text()" a))))))
      (setf ret ":string"))
    (list name ret
          (fix-args-types
           name
           (xpath:map-node-set->list
            'parse-param
            params)))))

(defun wrapp (p)
  (loop for (nil type) in p
          thereis (and (consp type)
                       (not (or (string= (second type) "(:pointer :void)")
                                (string= (second type) ":void"))))))

(defvar *names*
  (alexandria:plist-hash-table
   '()
   :test 'equal))

(defun translate-name (n)
  (assert (alexandria:starts-with-subseq "egl" n))
  (or (gethash n *names*)
      (progn
        (setf n (subseq n 3))
        (cffi:translate-camelcase-name
         n :special-words '("API" "EXT" "KHR" "NV" "MESA" "ANDROID"
                            "HI" "DRM" "FD" "GL" "NOK" "ANGLE")))))


;; some functions mix enum and int in same parameter, so need to wrap
;; them
(defparameter *force-wrap* '("eglSurfaceAttrib"))

(defun print-function (n r p)
  (let* ((wrap (or (member n *force-wrap* :test 'string=)
                   (wrapp p)))
         (name (if wrap
                   (format nil "\"~a\"" n)
                   (format nil "(\"~a\" ~(~a~))" n (translate-name n)))))
    (format t "(defcfun ~a ~a~{~%  ~a~})~%~%"
            name (map-type r) p)))

(loop for x in
            (xpath:map-node-set->list
             'parse-function
             (xpath:evaluate "/registry/commands/command" *xml*))
      do (apply #'print-function x))
