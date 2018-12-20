(in-package :egl)

(define-foreign-library libegl
  (t (:default "libEGL")))

(use-foreign-library libegl)
