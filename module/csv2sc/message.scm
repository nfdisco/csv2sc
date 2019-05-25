(define-module (csv2sc message)
  #:use-module (csv2sc program)
  #:export     (msg-info msg-warning msg-error))

(define (message port suffix msg . args)
  "Write message to PORT."
  (apply format
         port
         (string-append "~a~a" msg "\n")
         (if (program-name)
             (string-append (program-name) ": ")
             "")
         (if suffix
             (string-append suffix ": ")
             "")
         args))

(define (msg-info msg . args)
  (apply message (current-output-port) #f msg args))

(define (msg-warning msg . args)
  (apply message (current-error-port) "warning" msg args))

(define (msg-error msg . args)
  (apply message (current-error-port) "error" msg args))


