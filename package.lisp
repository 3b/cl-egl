;;;; package.lisp

(defpackage :egl
  (:use :common-lisp :cffi)
  (:export
   #:get-display
   #:initialize
   #:bind-api
   #:choose-config
   #:create-context
   #:create-window-surface
   #:make-current
   #:swap-buffers
   #:destroy-surface
   #:destroy-context
   #:terminate
   #:bind-tex-image
   #:client-wait-sync
   #:copy-buffers
   #:destroy-image
   #:destroy-sync
   #:get-current-context
   #:get-current-display
   #:get-current-surface
   #:get-error
   #:query-api
   #:query-device-string-ext
   #:query-output-layer-string-ext
   #:query-output-port-string-ext
   #:query-string
   #:release-tex-image
   #:release-thread
   #:stream-flush-nv
   #:swap-interval
   #:wait-client
   #:wait-gl
   #:wait-native
   #:wait-sync
   #:get-config-attribs
   #:get-config-attrib
   #:query-surface
   #:get-platform-display-ext
   #:get-platform-display))


