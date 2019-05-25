(define-module (csv2sc cli)
  #:use-module (csv2sc program)
  #:use-module (csv2sc message)
  #:use-module (csv2sc csv2sc)
  #:use-module (ice-9 getopt-long)
  #:export     (main))

(define (char-arg arg)
  (if (char? arg)
      arg
      (and (string? arg)
           (= (string-length arg) 1)
           (string-ref arg 0))))

(define (display-help)
  (format #t "\
Usage: ~a [OPTION] ...\n
Read CSV data from the standard input and write it in SC (\"spreadsheet
calculator\") format to the standard output.

  -s, --field-sep=CHAR      use CHAR as field separator (\",\" by default)
  -d, --decimal-sep=CHAR    use CHAR as decimal separator (\".\" by default)
  -q, --quotation-char=CHAR use CHAR as quotation character
  -w, --ignore-ws           ignore whitespace
  -r, --right-align         right-justify strings
  -h, --help                display this help and exit
" (program-name)))

(define option-spec
  `((field-sep      (single-char #\s) (value #t) (predicate ,char-arg))
    (decimal-sep    (single-char #\d) (value #t) (predicate ,char-arg))
    (quotation-char (single-char #\q) (value #t) (predicate ,char-arg))
    (ignore-ws      (single-char #\w) (value #f))
    (right-align    (single-char #\r) (value #f))
    (help           (single-char #\h) (value #f))))

(define (%main args)
  (let* ((options (getopt-long args option-spec))
         (field-sep (char-arg (option-ref options 'field-sep #\,)))
         (decimal-sep (char-arg (option-ref options 'decimal-sep #f)))
         (quotation-char (char-arg (option-ref options 'quotation-char #f)))
         (ignore-ws (option-ref options 'ignore-ws #f))
         (right-align (option-ref options 'right-align #f)))
    (when (option-ref options 'help #f)
          (display-help)
          (exit 0))
    (when (char=? field-sep (or decimal-sep #\.))
          (msg-error "decimal separator clashes with field separator")
          (exit 1))
    (when (and quotation-char
               (char=? quotation-char field-sep))
          (msg-error "quotation character clashes with field separator")
          (exit 1))
    (when (and quotation-char
               (char=? quotation-char (or decimal-sep #\.)))
          (msg-error "quotation character clashes with decimal separator")
          (exit 1))
    (when (not (null? (option-ref options '() #f)))
          (msg-error "non-option argument")
          (exit 1))
    (csv->sc field-sep quotation-char ignore-ws decimal-sep right-align)))

(define (main args)
  (parameterize ((program-name (basename (car args))))
                (%main args)))
