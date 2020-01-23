(define (batch-resize pattern width height)
(let* ((filelist (cadr (file-glob pattern 1))))
(gimp-message-set-handler 1)
(while (not (null? filelist))
(let* ((filename (car filelist))
(image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
(drawable (car (gimp-image-get-active-layer image)))
(str-w-0 (number->string (car (gimp-image-width image))))
(str-h-0 (number->string (car (gimp-image-height image)))))
(gimp-image-crop image width height 0 0)
(gimp-file-save RUN-NONINTERACTIVE image drawable filename filename)
(gimp-message (string-append
               filename " processed "
               str-w-0 "x" str-h-0 " -> "
               (number->string (car (gimp-image-width image)))
               "x"
               (number->string (car (gimp-image-height image))))) 
(gimp-image-delete image))
(set! filelist (cdr filelist)))))
;; gimp -i -b '(batch-resize "*.jpg" 900 600)' -b '(gimp-quit 0)'
