#!/usr/bin/guile \
-e main -s
!#

(use-modules (rnrs io ports))
(use-modules (ice-9 getopt-long))

(define program-name (make-parameter "csv2sc"))

(define (message port suffix fmt . args)
  "Write message, prepended with the program name, to PORT."
  (format port "~a: ~a~a\n" (program-name) suffix (apply format #f fmt args)))

(define (fatal-error fmt . args)
  "Write message to the standard error and exit."
  (apply message (current-error-port) "ERROR: " fmt args)
  (exit 1))

(define (warning fmt . args)
  "Write warning message to the standard error."
  (apply message (current-error-port) "WARNING: " fmt args))

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
                     (warning "skipping line no. ~A: ~A\n" row-num msg)))
            (iter (1+ row-num)))))
    (iter 1))

(define (apply-command-line-arguments args)
  "Apply command-line arguments to csv->sc."
  (define (char-arg arg)
    (if (char? arg)
        arg
        (and (string? arg)
             (= (string-length arg) 1)
             (string-ref arg 0))))

  (define option-spec
    `((field-sep      (single-char #\s) (value #t) (predicate ,char-arg))
      (decimal-sep    (single-char #\d) (value #t) (predicate ,char-arg))
      (quotation-char (single-char #\q) (value #t) (predicate ,char-arg))
      (ignore-ws      (single-char #\w) (value #f))
      (right-align    (single-char #\r) (value #f))
      (help           (single-char #\h) (value #f))))

  (define options (getopt-long args option-spec))

  (define (display-help)
    (format #t "Usage: ~a [OPTION]...\n" (program-name))
    (format #t "\
Read CSV data from the standard input and write it in SC (\"spreadsheet
calculator\") format to the standard output.

  -s, --field-sep=CHAR      use CHAR as field separator (\",\" by default)
  -d, --decimal-sep=CHAR    use CHAR as decimal separator (\".\" by default)
  -q, --quotation-char=CHAR use CHAR as quotation character
  -w, --ignore-ws           ignore whitespace
  -r, --right-align         right-justify strings
  -h, --help                display this help and exit
"))
  
  (let* ((field-sep (char-arg (option-ref options 'field-sep #\,)))
         (decimal-sep (char-arg (option-ref options 'decimal-sep #f)))
         (quotation-char (char-arg (option-ref options 'quotation-char #f)))
         (ignore-ws (option-ref options 'ignore-ws #f))
         (right-align (option-ref options 'right-align #f))
         (help (option-ref options 'help #f)))
    (cond
     ((option-ref options 'help #f)
      (display-help))
     ((char=? field-sep (or decimal-sep #\.))
      (fatal-error "decimal separator clashes with field separator"))
     ((and quotation-char
           (char=? quotation-char field-sep))
      (fatal-error "quotation character clashes with field separator"))
     ((and quotation-char
           (char=? quotation-char (or decimal-sep #\.)))
      (fatal-error "quotation character clashes with decimal separator"))
     (else
      (csv->sc field-sep quotation-char ignore-ws decimal-sep right-align)))))

(define (main args)
  "Call apply-command-line-arguments."
  (parameterize ((program-name (car args)))
                (apply-command-line-arguments args)))


