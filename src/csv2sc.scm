#!/usr/bin/guile \
-e main -s
!#

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

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
     ((not (null? (option-ref options '() #f)))
      (fatal-error "non-option argument"))
     (else
      (csv->sc field-sep quotation-char ignore-ws decimal-sep right-align)))))

(define (main args)
  "Call apply-command-line-arguments."
  (parameterize ((program-name (basename (car args))))
                (apply-command-line-arguments args)))

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "guile"
;; End:
