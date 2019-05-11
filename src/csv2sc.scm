(use-modules (rnrs io ports))

(define (csv-error msg)
  "Raise a csv-error."
  (throw 'csv-error msg))

(define (csv-split-record str field-sep quote-char ignore-ws)
  "Split a CSV record into fields."
  (define (iter lst quoted field record)
    (cond
     ((null? lst)
      (cond
       (quoted
        (csv-error "mismatched quotation character"))
       (else
        (reverse (cons (reverse-list->string field) record)))))
     (else
      (let ((char (car lst)))
        (cond
         ((and quote-char
               (char=? char quote-char))
          (iter (cdr lst) (not quoted) field record))
         ((and ignore-ws
               (not quoted)
               (char-whitespace? char))
          (iter (cdr lst) quoted field record))
         ((and (not quoted)
               (char=? char field-sep))
          (iter (cdr lst) #f (list)
                (cons (reverse-list->string field) record)))
         (else
          (iter (cdr lst) quoted (cons char field) record)))))))
  (iter (string->list str) #f (list) (list)))

(define (csv-field->number str dec-sep-char)
  "Parse a numeric CSV field.  Return #f if field is non-numeric."
  (cond
   ((or (string-null? str)
        (let ((char (string-ref str 0)))
          (not (or (char-set-contains? char-set:digit char)
                   (char=? char #\-)
                   (char=? char #\+)
                   (char=? char (or dec-sep-char #\.))))))
    #f)
   (else
    (let ((i (and dec-sep-char (string-index str dec-sep-char))))
      (string->number (if i
                          (string-replace str "." i (1+ i) 0 1)
                          str))))))

(define (sc-cell-coords row-num col-num)
  "Return the coordinates of a cell in alphanumeric notation, e.g. \"Z42\"."
  (define (column-name)
    (define (iter x digits)
      (let ((q (quotient x 26))
            (d (integer->char (+ (remainder x 26) 65))))
        (cond
         ((zero? q)
          (cons d digits))
         (else
          (iter (1- q) (cons d digits))))))
    (list->string (iter (1- col-num) (list))))
  (string-append (column-name) (number->string row-num)))

(define (sc-put-cell str row-num col-num dec-sep-char right-align)
  "Write a cell to the standard output in SC format."
  (let ((place (sc-cell-coords row-num col-num))
        (value (or (csv-field->number str dec-sep-char) str)))
    (cond
     ((number? value)
      (format #t "let ~A = ~A\n" place value))
     ((not (string-null? value))
      (format #t
              "~A ~A = ~S\n"
              (if right-align "rightstring" "leftstring")
              place
              value)))))

(define (csv->sc field-sep-char quote-char ignore-ws dec-sep-char right-align)
  "Read CSV-formatted data from the standard input and write it in SC format
to the standard output."
  (define (iter row-num)
    (define (put-cells str)
      (define (iter lst col-num)
        (when (not (null? lst))
              (sc-put-cell (car lst) col-num row-num dec-sep-char right-align)
              (iter (cdr lst) (1+ col-num))))
      (iter (csv-split-record str field-sep-char quote-char ignore-ws) 1))
    (let ((line (get-line (current-input-port))))
      (when (not (eof-object? line))
            (catch 'csv-error
                   (lambda ()
                     (put-cells line))
                   (lambda (key msg)
                     (format (current-error-port)
                             "warning: skipping line no. ~A: ~A\n"
                             row-num
                             msg)))
            (iter (1+ row-num)))))
    (iter 1))

(csv->sc #\, #\" #f #f #f)
