(in-package :egl)

(defcfun ("eglBindAPI" bind-api) EGLBoolean
  (api EGLenum))

(defcfun ("eglBindTexImage" bind-tex-image) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (buffer EGLint))

(defcfun "eglChooseConfig" EGLBoolean
  (dpy EGLDisplay)
  (attrib_list (:pointer EGLint))
  (configs (:pointer EGLConfig))
  (config_size EGLint)
  (num_config (:pointer EGLint)))

(defcfun "eglClientSignalSyncEXT" EGLBoolean
  (dpy EGLDisplay)
  (sync EGLSync)
  (attrib_list (:pointer EGLAttrib)))

(defcfun ("eglClientWaitSync" client-wait-sync) EGLint
  (dpy EGLDisplay)
  (sync EGLSync)
  (flags EGLSyncFlagsKHR)
  (timeout EGLTime))

(defcfun ("eglClientWaitSyncKHR" client-wait-sync-khr) EGLint
  (dpy EGLDisplay)
  (sync EGLSyncKHR)
  (flags EGLSyncFlagsKHR)
  (timeout EGLTimeKHR))

(defcfun ("eglClientWaitSyncNV" client-wait-sync-nv) EGLint
  (sync EGLSyncNV)
  (flags EGLSyncFlagsNV)
  (timeout EGLTimeNV))

(defcfun ("eglCopyBuffers" copy-buffers) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (target EGLNativePixmapType))

(defcfun "eglCreateContext" EGLContext
  (dpy EGLDisplay)
  (config EGLConfig)
  (share_context EGLContext)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreateDRMImageMESA" EGLImageKHR
  (dpy EGLDisplay)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreateFenceSyncNV" EGLSyncNV
  (dpy EGLDisplay)
  (condition EGLenum)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreateImage" EGLImage
  (dpy EGLDisplay)
  (ctx EGLContext)
  (target EGLenum)
  (buffer EGLClientBuffer)
  (attrib_list (:pointer EGLAttrib)))

(defcfun "eglCreateImageKHR" EGLImageKHR
  (dpy EGLDisplay)
  (ctx EGLContext)
  (target EGLenum)
  (buffer EGLClientBuffer)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreateNativeClientBufferANDROID" EGLClientBuffer
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreatePbufferFromClientBuffer" EGLSurface
  (dpy EGLDisplay)
  (buftype EGLenum)
  (buffer EGLClientBuffer)
  (config EGLConfig)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreatePbufferSurface" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreatePixmapSurface" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (pixmap EGLNativePixmapType)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreatePixmapSurfaceHI" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (pixmap (:pointer (:struct EGLClientPixmapHI))))

(defcfun "eglCreatePlatformPixmapSurface" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (native_pixmap (:pointer :void))
  (attrib_list (:pointer EGLAttrib)))

(defcfun "eglCreatePlatformPixmapSurfaceEXT" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (native_pixmap (:pointer :void))
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreatePlatformWindowSurface" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (native_window (:pointer :void))
  (attrib_list (:pointer EGLAttrib)))

(defcfun "eglCreatePlatformWindowSurfaceEXT" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (native_window (:pointer :void))
  (attrib_list (:pointer EGLint)))

(defcfun ("eglCreateStreamFromFileDescriptorKHR" create-stream-from-file-descriptor-khr) EGLStreamKHR
  (dpy EGLDisplay)
  (file_descriptor EGLNativeFileDescriptorKHR))

(defcfun "eglCreateStreamKHR" EGLStreamKHR
  (dpy EGLDisplay)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreateStreamAttribKHR" EGLStreamKHR
  (dpy EGLDisplay)
  (attrib_list (:pointer EGLAttrib)))

(defcfun "eglCreateStreamProducerSurfaceKHR" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (stream EGLStreamKHR)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreateStreamSyncNV" EGLSyncKHR
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (type EGLenum)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreateSync" EGLSync
  (dpy EGLDisplay)
  (type EGLenum)
  (attrib_list (:pointer EGLAttrib)))

(defcfun "eglCreateSyncKHR" EGLSyncKHR
  (dpy EGLDisplay)
  (type EGLenum)
  (attrib_list (:pointer EGLint)))

(defcfun "eglCreateSync64KHR" EGLSyncKHR
  (dpy EGLDisplay)
  (type EGLenum)
  (attrib_list (:pointer EGLAttribKHR)))

(defcfun "eglCreateWindowSurface" EGLSurface
  (dpy EGLDisplay)
  (config EGLConfig)
  (win EGLNativeWindowType)
  (attrib_list (:pointer EGLint)))

(defcfun "eglDebugMessageControlKHR" EGLint
  (callback EGLDEBUGPROCKHR)
  (attrib_list (:pointer EGLAttrib)))

(defcfun ("eglDestroyContext" destroy-context) EGLBoolean
  (dpy EGLDisplay)
  (ctx EGLContext))

(defcfun ("eglDestroyImage" destroy-image) EGLBoolean
  (dpy EGLDisplay)
  (image EGLImage))

(defcfun ("eglDestroyImageKHR" destroy-image-khr) EGLBoolean
  (dpy EGLDisplay)
  (image EGLImageKHR))

(defcfun ("eglDestroyStreamKHR" destroy-stream-khr) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR))

(defcfun ("eglDestroySurface" destroy-surface) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface))

(defcfun ("eglDestroySync" destroy-sync) EGLBoolean
  (dpy EGLDisplay)
  (sync EGLSync))

(defcfun ("eglDestroySyncKHR" destroy-sync-khr) EGLBoolean
  (dpy EGLDisplay)
  (sync EGLSyncKHR))

(defcfun ("eglDestroySyncNV" destroy-sync-nv) EGLBoolean
  (sync EGLSyncNV))

(defcfun ("eglDupNativeFenceFDANDROID" dup-native-fence-fd-android) EGLint
  (dpy EGLDisplay)
  (sync EGLSyncKHR))

(defcfun "eglExportDMABUFImageMESA" EGLBoolean
  (dpy EGLDisplay)
  (image EGLImageKHR)
  (fds (:pointer :int))
  (strides (:pointer EGLint))
  (offsets (:pointer EGLint)))

(defcfun "eglExportDMABUFImageQueryMESA" EGLBoolean
  (dpy EGLDisplay)
  (image EGLImageKHR)
  (fourcc (:pointer :int))
  (num_planes (:pointer :int))
  (modifiers (:pointer EGLuint64KHR)))

(defcfun "eglExportDRMImageMESA" EGLBoolean
  (dpy EGLDisplay)
  (image EGLImageKHR)
  (name (:pointer EGLint))
  (handle (:pointer EGLint))
  (stride (:pointer EGLint)))

(defcfun ("eglFenceNV" fence-nv) EGLBoolean
  (sync EGLSyncNV))

(defcfun "eglGetConfigAttrib" EGLBoolean
  (dpy EGLDisplay)
  (config EGLConfig)
  (attribute EGLint)
  (value (:pointer EGLint)))

(defcfun "eglGetConfigs" EGLBoolean
  (dpy EGLDisplay)
  (configs (:pointer EGLConfig))
  (config_size EGLint)
  (num_config (:pointer EGLint)))

(defcfun ("eglGetCurrentContext" get-current-context) EGLContext)

(defcfun ("eglGetCurrentDisplay" get-current-display) EGLDisplay)

(defcfun ("eglGetCurrentSurface" get-current-surface) EGLSurface
  (readdraw EGLint))

(defcfun ("eglGetDisplay" get-display) EGLDisplay
  (display_id EGLNativeDisplayType))

(defcfun ("eglGetError" get-error) EGLint)

(defcfun "eglGetNativeClientBufferANDROID" EGLClientBuffer
  (buffer (:pointer (:struct AHardwareBuffer))))

(defcfun "eglGetOutputLayersEXT" EGLBoolean
  (dpy EGLDisplay)
  (attrib_list (:pointer EGLAttrib))
  (layers (:pointer EGLOutputLayerEXT))
  (max_layers EGLint)
  (num_layers (:pointer EGLint)))

(defcfun "eglGetOutputPortsEXT" EGLBoolean
  (dpy EGLDisplay)
  (attrib_list (:pointer EGLAttrib))
  (ports (:pointer EGLOutputPortEXT))
  (max_ports EGLint)
  (num_ports (:pointer EGLint)))

(defcfun "eglGetPlatformDisplay" EGLDisplay
  (platform EGLenum)
  (native_display (:pointer :void))
  (attrib_list (:pointer EGLAttrib)))

(defcfun "eglGetPlatformDisplayEXT" EGLDisplay
  (platform EGLenum)
  (native_display (:pointer :void))
  (attrib_list (:pointer EGLint)))

(defcfun ("eglGetProcAddress" get-proc-address) :pointer
  (procname :string))

(defcfun ("eglGetStreamFileDescriptorKHR" get-stream-file-descriptor-khr) EGLNativeFileDescriptorKHR
  (dpy EGLDisplay)
  (stream EGLStreamKHR))

(defcfun "eglGetSyncAttrib" EGLBoolean
  (dpy EGLDisplay)
  (sync EGLSync)
  (attribute EGLint)
  (value (:pointer EGLAttrib)))

(defcfun "eglGetSyncAttribKHR" EGLBoolean
  (dpy EGLDisplay)
  (sync EGLSyncKHR)
  (attribute EGLint)
  (value (:pointer EGLint)))

(defcfun "eglGetSyncAttribNV" EGLBoolean
  (sync EGLSyncNV)
  (attribute EGLint)
  (value (:pointer EGLint)))

(defcfun ("eglGetSystemTimeFrequencyNV" get-system-time-frequency-nv) EGLuint64NV)

(defcfun ("eglGetSystemTimeNV" get-system-time-nv) EGLuint64NV)

(defcfun "eglInitialize" EGLBoolean
  (dpy EGLDisplay)
  (major (:pointer EGLint))
  (minor (:pointer EGLint)))

(defcfun ("eglLabelObjectKHR" label-object-khr) EGLint
  (display EGLDisplay)
  (objectType EGLenum)
  (object EGLObjectKHR)
  (label EGLLabelKHR))

(defcfun "eglLockSurfaceKHR" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (attrib_list (:pointer EGLint)))

(defcfun ("eglMakeCurrent" make-current) EGLBoolean
  (dpy EGLDisplay)
  (draw EGLSurface)
  (read EGLSurface)
  (ctx EGLContext))

(defcfun ("eglOutputLayerAttribEXT" output-layer-attrib-ext) EGLBoolean
  (dpy EGLDisplay)
  (layer EGLOutputLayerEXT)
  (attribute EGLint)
  (value EGLAttrib))

(defcfun ("eglOutputPortAttribEXT" output-port-attrib-ext) EGLBoolean
  (dpy EGLDisplay)
  (port EGLOutputPortEXT)
  (attribute EGLint)
  (value EGLAttrib))

(defcfun ("eglPostSubBufferNV" post-sub-buffer-nv) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (x EGLint)
  (y EGLint)
  (width EGLint)
  (height EGLint))

(defcfun ("eglPresentationTimeANDROID" presentation-time-android) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (time EGLnsecsANDROID))

(defcfun ("eglGetCompositorTimingSupportedANDROID" get-compositor-timing-supported-android) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (name EGLint))

(defcfun "eglGetCompositorTimingANDROID" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (numTimestamps EGLint)
  (names (:pointer EGLint))
  (values (:pointer EGLnsecsANDROID)))

(defcfun "eglGetNextFrameIdANDROID" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (frameId (:pointer EGLuint64KHR)))

(defcfun ("eglGetFrameTimestampSupportedANDROID" get-frame-timestamp-supported-android) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (timestamp EGLint))

(defcfun "eglGetFrameTimestampsANDROID" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (frameId EGLuint64KHR)
  (numTimestamps EGLint)
  (timestamps (:pointer EGLint))
  (values (:pointer EGLnsecsANDROID)))

(defcfun ("eglQueryAPI" query-api) EGLenum)

(defcfun "eglQueryContext" EGLBoolean
  (dpy EGLDisplay)
  (ctx EGLContext)
  (attribute EGLint)
  (value (:pointer EGLint)))

(defcfun "eglQueryDebugKHR" EGLBoolean
  (attribute EGLint)
  (value (:pointer EGLAttrib)))

(defcfun "eglQueryDeviceAttribEXT" EGLBoolean
  (device EGLDeviceEXT)
  (attribute EGLint)
  (value (:pointer EGLAttrib)))

(defcfun ("eglQueryDeviceStringEXT" query-device-string-ext) :string
  (device EGLDeviceEXT)
  (name EGLint))

(defcfun "eglQueryDevicesEXT" EGLBoolean
  (max_devices EGLint)
  (devices (:pointer EGLDeviceEXT))
  (num_devices (:pointer EGLint)))

(defcfun "eglQueryDisplayAttribEXT" EGLBoolean
  (dpy EGLDisplay)
  (attribute EGLint)
  (value (:pointer EGLAttrib)))

(defcfun "eglQueryDisplayAttribKHR" EGLBoolean
  (dpy EGLDisplay)
  (name EGLint)
  (value (:pointer EGLAttrib)))

(defcfun "eglQueryDisplayAttribNV" EGLBoolean
  (dpy EGLDisplay)
  (attribute EGLint)
  (value (:pointer EGLAttrib)))

(defcfun "eglQueryDmaBufFormatsEXT" EGLBoolean
  (dpy EGLDisplay)
  (max_formats EGLint)
  (formats (:pointer EGLint))
  (num_formats (:pointer EGLint)))

(defcfun "eglQueryDmaBufModifiersEXT" EGLBoolean
  (dpy EGLDisplay)
  (format EGLint)
  (max_modifiers EGLint)
  (modifiers (:pointer EGLuint64KHR))
  (external_only (:pointer EGLBoolean))
  (num_modifiers (:pointer EGLint)))

(defcfun "eglQueryNativeDisplayNV" EGLBoolean
  (dpy EGLDisplay)
  (display_id (:pointer EGLNativeDisplayType)))

(defcfun "eglQueryNativePixmapNV" EGLBoolean
  (dpy EGLDisplay)
  (surf EGLSurface)
  (pixmap (:pointer EGLNativePixmapType)))

(defcfun "eglQueryNativeWindowNV" EGLBoolean
  (dpy EGLDisplay)
  (surf EGLSurface)
  (window (:pointer EGLNativeWindowType)))

(defcfun "eglQueryOutputLayerAttribEXT" EGLBoolean
  (dpy EGLDisplay)
  (layer EGLOutputLayerEXT)
  (attribute EGLint)
  (value (:pointer EGLAttrib)))

(defcfun ("eglQueryOutputLayerStringEXT" query-output-layer-string-ext) :string
  (dpy EGLDisplay)
  (layer EGLOutputLayerEXT)
  (name EGLint))

(defcfun "eglQueryOutputPortAttribEXT" EGLBoolean
  (dpy EGLDisplay)
  (port EGLOutputPortEXT)
  (attribute EGLint)
  (value (:pointer EGLAttrib)))

(defcfun ("eglQueryOutputPortStringEXT" query-output-port-string-ext) :string
  (dpy EGLDisplay)
  (port EGLOutputPortEXT)
  (name EGLint))

(defcfun "eglQueryStreamKHR" EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attribute EGLenum)
  (value (:pointer EGLint)))

(defcfun "eglQueryStreamAttribKHR" EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attribute EGLenum)
  (value (:pointer EGLAttrib)))

(defcfun ("eglQueryStreamMetadataNV" query-stream-metadata-nv) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (name EGLenum)
  (n EGLint)
  (offset EGLint)
  (size EGLint)
  (data (:pointer :void)))

(defcfun "eglQueryStreamTimeKHR" EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attribute EGLenum)
  (value (:pointer EGLTimeKHR)))

(defcfun "eglQueryStreamu64KHR" EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attribute EGLenum)
  (value (:pointer EGLuint64KHR)))

(defcfun ("eglQueryString" query-string) :string
  (dpy EGLDisplay)
  (name EGLint))

(defcfun "eglQuerySurface" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (attribute EGLint)
  (value (:pointer EGLint)))

(defcfun "eglQuerySurface64KHR" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (attribute EGLint)
  (value (:pointer EGLAttribKHR)))

(defcfun ("eglQuerySurfacePointerANGLE" query-surface-pointer-angle) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (attribute EGLint)
  (value (:pointer (:pointer :void))))

(defcfun ("eglReleaseTexImage" release-tex-image) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (buffer EGLint))

(defcfun ("eglReleaseThread" release-thread) EGLBoolean)

(defcfun ("eglResetStreamNV" reset-stream-nv) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR))

(defcfun ("eglSetBlobCacheFuncsANDROID" set-blob-cache-funcs-android) :void
  (dpy EGLDisplay)
  (set EGLSetBlobFuncANDROID)
  (get EGLGetBlobFuncANDROID))

(defcfun "eglSetDamageRegionKHR" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (rects (:pointer EGLint))
  (n_rects EGLint))

(defcfun ("eglSetStreamAttribKHR" set-stream-attrib-khr) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attribute EGLenum)
  (value EGLAttrib))

(defcfun ("eglSetStreamMetadataNV" set-stream-metadata-nv) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (n EGLint)
  (offset EGLint)
  (size EGLint)
  (data (:pointer :void)))

(defcfun ("eglSignalSyncKHR" signal-sync-khr) EGLBoolean
  (dpy EGLDisplay)
  (sync EGLSyncKHR)
  (mode EGLenum))

(defcfun ("eglSignalSyncNV" signal-sync-nv) EGLBoolean
  (sync EGLSyncNV)
  (mode EGLenum))

(defcfun ("eglStreamAttribKHR" stream-attrib-khr) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attribute EGLenum)
  (value EGLint))

(defcfun ("eglStreamConsumerAcquireKHR" stream-consumer-acquire-khr) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR))

(defcfun "eglStreamConsumerAcquireAttribKHR" EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attrib_list (:pointer EGLAttrib)))

(defcfun ("eglStreamConsumerGLTextureExternalKHR" stream-consumer-gl-texture-external-khr) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR))

(defcfun "eglStreamConsumerGLTextureExternalAttribsNV" EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attrib_list (:pointer EGLAttrib)))

(defcfun ("eglStreamConsumerOutputEXT" stream-consumer-output-ext) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (layer EGLOutputLayerEXT))

(defcfun ("eglStreamConsumerReleaseKHR" stream-consumer-release-khr) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR))

(defcfun "eglStreamConsumerReleaseAttribKHR" EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR)
  (attrib_list (:pointer EGLAttrib)))

(defcfun ("eglStreamFlushNV" stream-flush-nv) EGLBoolean
  (dpy EGLDisplay)
  (stream EGLStreamKHR))

(defcfun "eglSurfaceAttrib" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (attribute EGLEnum)
  (value EGLint))

(defcfun ("eglSwapBuffers" swap-buffers) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface))

(defcfun "eglSwapBuffersWithDamageEXT" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (rects (:pointer EGLint))
  (n_rects EGLint))

(defcfun "eglSwapBuffersWithDamageKHR" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (rects (:pointer EGLint))
  (n_rects EGLint))

(defcfun "eglSwapBuffersRegionNOK" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (numRects EGLint)
  (rects (:pointer EGLint)))

(defcfun "eglSwapBuffersRegion2NOK" EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface)
  (numRects EGLint)
  (rects (:pointer EGLint)))

(defcfun ("eglSwapInterval" swap-interval) EGLBoolean
  (dpy EGLDisplay)
  (interval EGLint))

(defcfun ("eglTerminate" terminate) EGLBoolean
  (dpy EGLDisplay))

(defcfun ("eglUnlockSurfaceKHR" unlock-surface-khr) EGLBoolean
  (dpy EGLDisplay)
  (surface EGLSurface))

(defcfun "eglUnsignalSyncEXT" EGLBoolean
  (dpy EGLDisplay)
  (sync EGLSync)
  (attrib_list (:pointer EGLAttrib)))

(defcfun ("eglWaitClient" wait-client) EGLBoolean)

(defcfun ("eglWaitGL" wait-gl) EGLBoolean)

(defcfun ("eglWaitNative" wait-native) EGLBoolean
  (engine EGLint))

(defcfun ("eglWaitSync" wait-sync) EGLBoolean
  (dpy EGLDisplay)
  (sync EGLSync)
  (flags EGLSyncFlagsKHR))

(defcfun ("eglWaitSyncKHR" wait-sync-khr) EGLint
  (dpy EGLDisplay)
  (sync EGLSyncKHR)
  (flags EGLSyncFlagsKHR))

(defcfun "eglCompositorSetContextListEXT" EGLBoolean
  (external_ref_ids (:pointer EGLint))
  (num_entries EGLint))

(defcfun "eglCompositorSetContextAttributesEXT" EGLBoolean
  (external_ref_id EGLint)
  (context_attributes (:pointer EGLint))
  (num_entries EGLint))

(defcfun "eglCompositorSetWindowListEXT" EGLBoolean
  (external_ref_id EGLint)
  (external_win_ids (:pointer EGLint))
  (num_entries EGLint))

(defcfun "eglCompositorSetWindowAttributesEXT" EGLBoolean
  (external_win_id EGLint)
  (window_attributes (:pointer EGLint))
  (num_entries EGLint))

(defcfun ("eglCompositorBindTexWindowEXT" compositor-bind-tex-window-ext) EGLBoolean
  (external_win_id EGLint))

(defcfun ("eglCompositorSetSizeEXT" compositor-set-size-ext) EGLBoolean
  (external_win_id EGLint)
  (width EGLint)
  (height EGLint))

(defcfun ("eglCompositorSwapPolicyEXT" compositor-swap-policy-ext) EGLBoolean
  (external_win_id EGLint)
  (policy EGLint))
