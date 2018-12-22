(in-package :egl)

(defun initialize (display)
  (with-foreign-objects
      ((major 'EGLint 1)
       (minor 'EGLint 1))
    (when (= (eglInitialize display major minor) 0)
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
     :context-profile-mask EGLContextProfileMask
     :conformant EGLRenderableTypeMask)))

(defparameter *attribute-enums*
  (alexandria:plist-hash-table
    '(:bind-to-texture-rgb t
      :bind-to-texture-rgba t
      :color-buffer-type t
      :config-caveat t
      :renderable t
      :transparent-type t
      :native-renderable t)))

;; todo: option to add error check to all functions automatically?
(defmacro check (call)
  `(prog1 ,call
    (let ((e (get-error)))
      (unless (or (eql e #x3000)
                  (eql e :success))
        (error "egl error ~s (~s)"
               (if (numberp e)
                   (foreign-enum-keyword 'eglenum e :errorp nil)
                   e)
               e)))))

(defun translate-attribute (attribute value)
  (let* ((a (if (numberp attribute)
                (foreign-enum-keyword 'eglenum attribute)
                attribute))
         (b (gethash a *attribute-bitfield-types*)))
    (if b
        (foreign-bitfield-value b value)
        (foreign-enum-value 'eglenum value))))

(defmacro with-attribute-list ((attribs pointer-var) &body body)
  (alexandria:once-only (attribs)
    (alexandria:with-gensyms (i last attrib attribs*)
      ;; make attribute list ends in :none
      `(let ((,attribs* (if (and (oddp (length ,attribs))
                                (eql :none (car (last ,attribs))))
                           ,attribs
                           (append ,attribs '(:none)))))
         (with-foreign-objects ((,pointer-var 'EGLint (length ,attribs*)))
          (loop :for ,i :from 0
                :for ,last = nil :then ,attrib
                :for ,attrib :in ,attribs*
                :do (setf (mem-aref ,pointer-var 'EGLint ,i)
                          (if (keywordp ,attrib)
                              (if (evenp ,i)
                                  ;; even indices indicate attribute being set
                                  (foreign-enum-value 'eglenum ,attrib)
                                  ;; odd indicies are values, which
                                  ;; might be an enum or bitfield
                                  (translate-attribute ,last ,attrib))
                              ,attrib)))
          ,@body)))))

(defun choose-config (display config-size &rest config-attribs)
  (with-foreign-objects ((available-configs '(:pointer EGLConfig) 1)
                         (num-configs 'EGLint 1))
    (with-attribute-list (config-attribs requested-attribs)
      (check (eglchooseconfig display requested-attribs available-configs
                              config-size num-configs)))
    (loop :for i :from 0 :below (mem-aref num-configs 'EGLint)
          :collecting (mem-aref available-configs :pointer i))))

(defun choose-config* (display &rest config-attribs)
  (with-foreign-objects ((num-configs 'EGLint))
    (with-attribute-list (config-attribs requested-attribs)
      (check (eglchooseconfig display requested-attribs (cffi:null-pointer)
                              0 num-configs))
      (let ((n (cffi:mem-ref num-configs 'eglint)))
        (with-foreign-objects ((available-configs 'EGLConfig n))
          (check (eglchooseconfig display requested-attribs available-configs
                                  n num-configs))
          (loop :for i :from 0 :below (mem-aref num-configs 'EGLint)
                :collecting (mem-aref available-configs :pointer i)))))))

(defun create-context (display config share-context &rest attribs)
  (with-attribute-list (attribs requested-attribs)
    (check (eglcreatecontext display config share-context requested-attribs))))

(defun create-window-surface (display config win &rest attribs)
  (with-attribute-list (attribs ap)
    (check (eglcreatewindowsurface display config win ap))))

(defun get-config-attrib (display config attrib)
  (with-foreign-objects ((value 'eglint))
    (check (eglgetconfigattrib display config attrib value))
    (if (gethash attrib *attribute-bitfield-types*)
        (cffi:mem-ref value (gethash attrib *attribute-bitfield-types*))
        (let ((v (cffi:mem-ref value 'eglint)))
          (if (gethash attrib *attribute-enums*)
              (case v
                (0 :false)
                (1 :true)
                (otherwise (cffi:foreign-enum-keyword 'eglenum v)))
              v)))))

(defun get-config-attribs (display config)
  (loop
    for i in '(:red-size
               :green-size
               :blue-size
               :alpha-size
               :depth-size
               :color-buffer-type
               :config-caveat
               :alpha-mask-size
               :bind-to-texture-rgb
               :bind-to-texture-rgba
               :buffer-size
               :config-id
               :conformant
               :level
               :luminance-size
               :max-pbuffer-width
               :max-pbuffer-height
               :max-pbuffer-pixels
               :max-swap-interval
               :min-swap-interval
               :native-renderable
               :native-visual-id
               :native-visual-type
               :renderable-type
               :sample-buffers
               :samples
               :stencil-size
               :surface-type
               :transparent-type
               :transparent-red-value
               :transparent-green-value
               :transparent-blue-value)
    collect  i
    collect (get-config-attrib display config i)))

