#lang racket
(require "../main.rkt")

(module+ worker
  (define tmp (make-temporary-file))
  (start-simple-worker (λ (x) (sleep .2) (path->string tmp)))
  ;; Clean up when server closes:
  (delete-file tmp)
  ;; At this point, the output port has been closed, so attempting to write to it
  ;; will raise an error like:
  ;;   error writing to stream port
  ;;     system error: Broken pipe; errno=32
  #;(println "Don't do this!")
  ;; Writing to the error-port is still permitted, and is redirected to the server's error port.
  ;; (but this breaks the rackunit checks, so we don't do it here.)
  #;(eprintf "This you can do\n"))

(module+ test
  (require rackunit)

  ;; start-simple-server
  (let ()
    (define tmp-h (make-hash))
    (define n-workers 2)
    (start-simple-server #:worker-file (this-file)
                         #:data-list '(a b c)
                         #:n-workers n-workers
                         #:process-result
                         (λ (data tmp)
                           (check-true (file-exists? tmp))
                           (hash-set! tmp-h tmp #t)))

    (check-equal? (hash-count tmp-h) n-workers)
    (for ([tmp (in-dict-keys tmp-h)])
      (check-false (file-exists? tmp))))

  ;; make-server, server-start, server-close
  (let ()
    (define tmp-h (make-hash))
    (define n-workers 2)
    (define server (make-server #:worker-file (this-file) #:n-workers n-workers))
    (server-start server
                  #:data-list '(a b c)
                  #:process-result
                  (λ (data tmp)
                    (check-true (file-exists? tmp))
                    (hash-set! tmp-h tmp #t)))
    (for ([tmp (in-dict-keys tmp-h)])
      (check-pred file-exists? tmp))
    (server-close server) ; waits for the workers to terminate gracefully.
    
    (check-equal? (hash-count tmp-h) n-workers)
    (for ([tmp (in-dict-keys tmp-h)])
      (check-false (file-exists? tmp)))))
