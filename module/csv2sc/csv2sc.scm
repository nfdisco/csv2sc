(define-module (csv2sc csv2sc)
  #:use-module (csv2sc message)
  #:use-module (rnrs io ports)
  #:export     (csv->sc))

(define (csv-error msg)
  "Raise a csv-error."
  (throw 'csv-error msg))

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

(define (csv-parse-record str field-sep dec-sep-char quote-char ignore-ws)
  "Parse CSV record, return list of fields."
  (define (iter lst quoted current fields)
    (cond
     ((null? lst)
      (cond
       (quoted
        (csv-error "mismatched quotation character"))
       (else
        (reverse (cons (reverse-list->string current) fields)))))
     (else
      (let ((char (car lst)))
        (cond
         ((and quote-char
               (char=? char quote-char))
          (iter (cdr lst) (not quoted) current fields))
         ((and ignore-ws
               (not quoted)
               (char-whitespace? char))
          (iter (cdr lst) quoted current fields))
         ((and (not quoted)
               (char=? char field-sep))
          (iter (cdr lst)
                #f
                (list)
                (cons (reverse-list->string current) fields)))
         (else
          (iter (cdr lst) quoted (cons char current) fields)))))))
  (map (lambda (field)
         (or (csv-field->number field dec-sep-char)
             field))
       (iter (string->list str) #f (list) (list))))

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

(define (sc-put-cell value row-num col-num right-align)
  "Write a cell to the standard output in SC format."
  (let ((place (sc-cell-coords row-num col-num)))
    (cond
     ((number? value)
      (format #t "let ~A = ~A\n" place value))
     ((not (string-null? value))
      (format #t
              "~A ~A = ~S\n"
              (if right-align "rightstring" "leftstring")
              place
              value)))))

(define (sc-put-record values row-num right-align)
  "Write record to the standard output in SC format."
  (define (iter values col-num)
    (when (not (null? values))
          (sc-put-cell (car values) row-num col-num right-align)
          (iter (cdr values) (1+ col-num))))
  (iter values 1))

(define (csv->sc field-sep-char quote-char ignore-ws dec-sep-char right-align)
  "Read CSV-formatted data from the standard input and write it in SC format
to the standard output."
  (define (iter row-num)
    (let ((line (get-line (current-input-port))))
      (when
       (not (eof-object? line))
       (catch 'csv-error
              (lambda ()
                (sc-put-record (csv-parse-record line field-sep-char
                  dec-sep-char quote-char ignore-ws) row-num right-align))
              (lambda (key msg)
                (msg-warning "skipping line no. ~A: ~A\n" row-num msg)))
       (iter (1+ row-num)))))
  (iter 0))

