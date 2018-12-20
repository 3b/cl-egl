(in-package :egl)

(defun initialize (display)
  (with-foreign-objects
      ((major 'EGLint 1)
       (minor 'EGLint 1))
    (when (= (eglInitialize display major minor) 0)
      (terminate display)
      (error "Failed to initialize EGL with code ~d" (get-error)))
    (format t "~A~%" (get-error))
    (values (mem-aref major 'EGLint)
	    (mem-aref minor 'EGLint))))

(defparameter *attribute-bitfield-types*
  (alexandria:plist-hash-table
   '(:surface-type EGLSurfaceTypeMask
     :renderable-type EGLRenderableTypeMask
     :lock-usage-hint-khr EGLLockUsageHintKHRMask
     :native-buffer-usage-android EGLNativeBufferUsageFlags
     :drm-buffer-use-mesa EGLDRMBufferUseMESAMask
     :context-flags-khr EGLContextFlagMask
     :context-profile-mask EGLContextProfileMask)))

(defun translate-attribute (attribute value)
  (let* ((a (if (numberp attribute)
                (foreign-enum-keyword 'eglenum attribute)
                attribute))
         (b (gethash a *attribute-bitfield-types*)))
    (if b
        (foreign-bitfield-value b value)
        (foreign-enum-value 'eglenum value))))

(defmacro with-attributes ((attribs pointer-var) &body body)
  (alexandria:once-only (attribs)
    (alexandria:with-gensyms (i last attrib)
      `(with-foreign-objects ((,pointer-var 'EGLint (length ,attribs)))
         (loop :for ,i :from 0
               :for ,last = nil :then ,attrib
               :for ,attrib :in ,attribs
               :do (setf (mem-aref ,pointer-var 'EGLint ,i)
		         (if (keywordp ,attrib)
		             (if (evenp ,i)
                                 ;; even indices indicate attribute being set
                                 (foreign-enum-value 'eglenum ,attrib)
                                 ;; odd indicies are values, which
                                 ;; might be an enum or bitfield
                                 (translate-attribute ,last ,attrib))
		             ,attrib)))
         ,@body))))

(defun choose-config (display config-size &rest config-attribs)
  (with-foreign-objects ((available-configs '(:pointer EGLConfig) 1)
                         (num-configs 'EGLint 1))
    (with-attributes (config-attribs requested-attribs)
      (eglchooseconfig display requested-attribs available-configs
                       config-size num-configs))
    (loop :for i :from 0 :below (mem-aref num-configs 'EGLint)
          :collecting (mem-aref available-configs :pointer i))))

(defun create-context (display config share-context &rest attribs)
  (with-attributes (attribs requested-attribs)
    (eglcreatecontext display config share-context requested-attribs)))

