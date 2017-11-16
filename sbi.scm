#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.2 2015-09-23 17:11:09-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an
;;    SBIR program, which is the executed.
;;

;;function call map
;;
;;global var
;;*stderr*
;;*run_file*
;;*function-table*
;;*label-table*
;;*variable-table*
;;
;;       <-------------result----------------------------
;; (main)<----------program---------------------------- |
;;  | | |                                             | |
;;  | | --filename-->(readlist-from-inputfile)--------| |
;;  | |                |  ^--------------------------   |
;;  | |                --program-->(process-labels)-|   |
;;  | -->(load-sbir-functions)                          |
;;  |      |                                            |
;;  |      -->(add-function)-->*function-table*         |
;;  -->program-->(convert)------------------------------|
;;                | ^  | ^----------line----
;;                | |  --index--->program--|
;;                | ----index------------------------------
;;                --(cdr line)-->*function-table*-->      |
;;                           |-->*variable-table*-->      |
;;                           |-->*label-table*----->(fn)--|
;;                                                   |
;;                                                [output]

;; *stderr*
;; This defines the stderr, to which we will be printing our error
;; messages
(define *stderr* (current-error-port))

;; *run-file*
;; This lets us retrive the name of this file
;; especially helpful for printing errors
(define *run-file*
    (let-values
        (((dirpath basepath root?)
                (split-path (find-system-path 'run-file))))
        (path->string basepath)))

;; *function-table*
;; create a hash-table to store the function calls
(define *function-table* (make-hash))

;; *label-table*
;; create a hash-table to store the labels for goto statements
(define *label-table* (make-hash))

;; *variable-table*
;; create a hash-table to store variables and arrays
(define *variable-table* (make-hash))

;; die
;; Input: a string describing the triggered error(s)
;;
;; Output:
;;
;; Print out the string describing the error(s) 
;; kill the program with a failed exit status
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; usage-exit
;; no input or output
;; kills the program
;; prints out a message to tell the user how to run the program properly
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; readlist-from-inputfile
;; Input: name of a file to be read
;;
;; Output: returns a list of the contents of the file
;; where one line in the file is one entry in the list
;; While reading the file, also populate *label-table* by
;; calling process-labels on the output list before returning it
(define (readlist-from-inputfile filename)
    (let 
         ;; open the file with the given filename
         ((inputfile (open-input-file filename)))
         ;; if the file failed to open properly
         (if (not (input-port? inputfile))
             ;; kill the program and print error
             (die `(,*run-file* ": " ,filename ": open failed"))
             ;; otherwise:
             ;; read the contents of the file and store them in a
             ;; list called program which is returned to the calling 
             ;; function
             (let 
                 ((program (read inputfile)))
                 (close-input-port inputfile);; close the file
                                             ;; before finishing
                 (process-labels program);; populate *label-table*
                                         ;; so we can execute goto
                                         ;; statements later
                 program))))

;; write-program-by-line
;; Input: the name of the file from which the instructions come
;;        list containing the contents of the file
;;
;; Output: prints out the name of this file, the file being interpreted
;;         each line from the list program surrounded by ()
;;
;; exists primarily for debugging
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
)

;; process-labels
;; Input: A list containing code from sbir file
;;
;; Output: store labels (for use by goto statements) into a hash table
(define (process-labels code)
    (for-each (lambda (line) ;; for each line in the list code
        (if (pair? (cdr line));; if the line contains at least 3 items
                              ;; ex:(lineno label: (instruction))
                              ;; here (label: (instruction)) is a pair
            (if (pair? (cadr line)) 0
                ;; store line in *label-table*
                (hash-set! *label-table* (cadr line) line))0))code))

;; add-function
;; Input: tag is the name of a sbir instruction,
;;        function is a function either in this program
;;        or from scheme that will execute the given instruction
;; 
;; Output: none
(define (add-function tag function)
    ((lambda(f t)(hash-set! *function-table* t f)) function tag)
)

;;two_arg_fn
;;helper function, calls a function from the hash table
;;and provides it with two args
(define (two_arg_fn op arg1 arg2)
    ((hash-ref *function-table* op) (+ arg1 0.0) (+ arg2 0.0))
)

;;one_arg_fn
;;helper funciton, like two_arg_fn, see above
(define (one_arg_fn op arg1)
    ((hash-ref *function-table* op) (+ arg1 0.0))
)

;;var_by_sym
;;helper function, returns a variable from the hash table
;;associated with the symbol passed in
(define (var_by_sym sym)
    (hash-ref *variable-table* (symbol->string sym))
)

;;series of checks
;; 1st create local variables for op and arg1 since everything 
;; passed here has an op and an arg
;; 2nd check if there is a second arg
;; 3rd check if any args are lists (ie: requrie extra evaluation)
;; 4th check if any args are variables instead of numbers
(define (eval_exp arg) 
    (let ([op (symbol->string(car arg))] [arg1 (cadr arg)])
        (if (not (empty? (cddr arg)))
            (let ([arg2 (caddr arg)])
                (if (and (not (list? arg1)) (not (list? arg2)))
                    (if (and (number? arg1) (number? arg2))
                        (two_arg_fn op arg1 arg2)
                        (if (number? arg1)
                            (two_arg_fn op arg1 (var_by_sym arg2))
                            (if (number? arg2)
                                (two_arg_fn op (var_by_sym arg1) arg2)
                               (two_arg_fn op (var_by_sym arg1) 
                                              (var_by_sym arg2)))))
                    (if (not (list? arg2))
                        (let ([exp1 (eval_exp arg1)])
                            (if (number? arg2)
                                (two_arg_fn op exp1 arg2)
                                (two_arg_fn op exp1 (var_by_sym arg2))))
                        (if (not (list? arg1))
                            (let ([exp2 (eval_exp arg2)])
                                 (if (number? arg1)
                                     (two_arg_fn op arg1 exp2)
                                     (two_arg_fn op 
                                        (var_by_sym arg1) exp2)))
                            (let ([exp1 (eval_exp arg1)] 
                                  [exp2 (eval_exp arg2)])
                                (two_arg_fn op exp1 exp2))))))
            (if (number? arg1)
                (one_arg_fn op arg1)
                (if (list? arg1)
                    (let ([exp1 (eval_exp arg1)])
                        (one_arg_fn op exp1))
                   (if (hash-has-key? *function-table* op)
                       (one_arg_fn op (var_by_sym arg1))
                       (vector-ref (hash-ref *variable-table* op) 
                        (inexact->exact (- (var_by_sym arg1) 1)))))))))

;; printString
;; Input: a string representing either a string, number or variable 
;;        to be printed
;;
;; Output: the string representation of the argument is printed
(define (printString args)
    (if (empty? args)
        0
        (for-each (lambda (arg)
            (if (list? arg)
                (display (eval_exp arg))
                (if (or (string? arg) (number? arg))
                    (display arg)
                    (display (var_by_sym arg)))))args))
    (printf "~n"))

;; goto
;; Input: the line containing a label which can be targeted by a 
;;        goto statement
;;
;; Output: returns a number value representing the index of the
;;         the line in the list code/program that contains the
;;         label pointed at by this goto statement
(define (goto line)
    ;; we want to return the car of this because, if you look at
    ;; process-labels, you'll notice that the entire line is stored
    ;; in the map but we just want the line number
    (car (hash-ref *label-table* (car line))))

;; createVar
;; Input: a name for a variable and a value to store in it
;;
;; Output: none, but the variable can be accessed from the
;; *variable-table* for later use
(define (createVar args)
    (if (pair? args);; confirm that there are two arguments,
                    ;; a variable name and a value
        (if (not (pair? (car args)))
            (let ([var-name (symbol->string(car args))] 
                  [val (cadr args)])
                (if (pair? val)
                    (hash-set! *variable-table* var-name (eval_exp val))
                    (if (number? val)
                        (hash-set! *variable-table* var-name val)
                        (hash-set! *variable-table* var-name 
                            (var_by_sym val)))))
            (let ([var-name (car (car args))] 
                  [ind (cadr (car args))] [val (cadr args)])
                (if (number? val)
                    (if (number? ind)
                        (vector-set! (var_by_sym var-name) 
                         (inexact->exact (- ind 1)) val)
                        (vector-set! (var_by_sym var-name) 
                         (inexact->exact (- (var_by_sym ind) 1)) val))
                    (if (number? ind)
                        (vector-set! (var_by_sym var-name) 
                         (inexact->exact (- ind1)) 
                         (inexact->exact(var_by_sym val)))
                        (vector-set! (var_by_sym var-name) 
                         (inexact->exact (- (var_by_sym ind) 1)) 
                         (inexact->exact (var_by_sym val)))))))0))

;; createArray
;; Input: a list which contains the name the array will be 
;; stored under in the variable table
;;
;; Output: none
(define (createArray line)
    (if (pair? line)
        (if (number? (cadr (car line)))
            (hash-set! *variable-table* 
                (symbol->string (car (car line))) 
                (make-vector(cadr (car line)) 0))
            (hash-set! *variable-table* 
                (symbol->string (car (car line)))
                (make-vector(var_by_sym (cadr (car line))) 0)))0))

;; input
;; Input: the name under which the newly minted variable will be 
;; stored
;;
;; Output: none, just stores any number of variables with user 
;; provided values via the stdin, also stores a variable 
;; "inputcount" which stores the number of variables created the 
;; last time this method was called, or -1 if an EOF is recieved 
(define (input line)
   (let ([init (hash-count *variable-table*)])
      (if (pair? (cdr line))
         (for-each (lambda (line) 
           (hash-set! *variable-table* (symbol->string line)
           (string->number (read-line (current-input-port) 'any))))line)
         (hash-set! *variable-table* (symbol->string (car line)) 
           (string->number (read-line (current-input-port) 'any))))
      (let*([fin (hash-count *variable-table*)]
            [inputcount (- fin init)])
          (hash-set! *variable-table* "inputcount" inputcount))))

;; conditional_jump
;; Input: a list of arguments, the first is an expression that will 
;;        be evaluated as true or false
(define (conditional_jump args)
    (if (eval_exp (car args))
        (let ([rtn (goto (cdr args))]) rtn)
        0))

;; not_equal
;; Input: two arguments
;;
;; Output: a true if the two args aren't equivalent, false otherwise
(define (not_equal arg1 arg2)
    (not (= arg1 arg2))
)

;; load-sbir-functions
;; no input or output
;; purpose: loads all sbir functions into hash tabel
(define (load-sbir-functions)
    (add-function "print" printString)
    (add-function "input" input)
    (add-function "dim" createArray)
    (add-function "let" createVar)
    (add-function "if" conditional_jump)
    (add-function "goto" goto)
    (add-function "+" +)
    (add-function "-" -)
    (add-function "*" *)
    (add-function "/" /)
    (add-function "^" expt)
    (add-function "log" log)
    (add-function "sqrt" sqrt)
    (add-function "sin" sin)
    (add-function "cos" cos) 
    (add-function "tan" tan) 
    (add-function "asin" asin) 
    (add-function "acos" acos) 
    (add-function "atan" atan) 
    (add-function "ceil" ceiling) 
    (add-function "floor" floor)
    (add-function "round" round)
    (add-function "abs" abs)
    (add-function "exp" exp)
    (add-function ">" >)
    (add-function ">=" >=)
    (add-function "=" =)
    (add-function "<=" <=)
    (add-function "<" <)
    (add-function "<>" not_equal))

;; function-call
;; Input: the line number of the instruction to be processed
;;        the line containing an instruction to be processed
;;
;; Output: none directly, will return the index (re: line number)
;;         to the calling function, which will be used to find the 
;;         next instruction, this will be either the same index passed
;;         in or it may be changed if a goto instruction is processed
;;         indirectly: depends on the function called
;; takes the instruction given and uses *function-table* to 
;; call the appropriate function to execute said instruction
(define (function-call index instruction args)
    ;; if instruction is in *function-table*; then we find the function 
    ;; that will execute that instruction pass the rest of the line as 
    ;; the arguments of the instruction
    (let ([instr (symbol->string instruction)])
    ((hash-ref *function-table* instr) args)
    ;; if the instruction was goto or if, then we need to use them
    ;; to return a new line number for the calling being made in convert
    (if (or (equal? instr "goto") 
            (equal? instr "if"))
        ;; if goto or if return 0, then we return the line of the next
        ;; instruction in sequence as normal, otherwise we send back the
        ;; line number of the label pointed at by the function
        ;; (actually one less due to how indexing in lists is off-set)
        (if (= 0 ((hash-ref *function-table* instr) args))
            (+ index 1)
            (- ((hash-ref *function-table* instr) args) 1))
        (+ 1 index))))

;; convert
;; Input: list containing the program
;;
;; Output: no direct output, but this will call functions
;; that will print to the stdout
(define (convert code index)
    (let* 
        ;; grab a line from the list of instructions
        ((rawline (list-ref code index)) 
         (line (cond 
                   ;; if the given line is empty then we process nothing
                   [(empty? (cdr rawline)) '()]
                   ;;otherwise, chop off the lineno before interpreting
                   [(list? (cadr rawline)) (cadr rawline)]
                   ;;we may also need to chop off the label too
                   [(not (empty? (cddr rawline))) (caddr rawline)]
                   ;; this is incase the given line is empty we only 
                   ;; include the else for syntax reasons because we 
                   ;; covered this possibilty in an earlier condition
                   [else '()])))
        ;; if the remaining line contains nothing ie: there is no 
        ;; instruction to interpret then
        (if (empty? line)
            ;;if there are no more lines of input to interpret
            (if (empty? (cdr (list-tail code index)))
                0;; then we want to terminate the program 
                ;; if there are more lines to interpret then continue to
                ;; iterate through the list code
                (convert code (+ index 1)))
            (let ([instruction (car line)] [args (cdr line)])
                ;; if the line contains interpretable code and is the
                ;; last line in the list code
                (if (empty? (cdr (list-tail code index)))
                    (if (< (function-call index instruction args) index)
                        ;; interpret that last line, and check to see 
                        ;; if it will pass back a goto line that may 
                        ;; make the execution continue
                        (convert code 
                             (function-call index instruction args)) 0 )
                    ;; if this isn't the last line, we can use the 
                    ;; fn-call to iterate through the list code
                    (convert code 
                            (function-call index instruction args)))))))
      
;; the main method
;; Input: if the program does not have exactly one argument exit and
;; report usage error
;;
;; Output: prints each line from the input file
(define (main arglist)
    ;;set defaults for tables
    (hash-set! *variable-table* "pi" pi)
    (hash-set! *variable-table* "e" (exp 1))
    (load-sbir-functions)
    ;; if arglist has exactly 1 arguement then exit the program w/
    ;; (usage-exit)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        ;; initialize variable with the filename of the input file
        ;; initialize list program with the contents of input file
        ;; pass this list to convert which will then interpret the
        ;; sbir instructions
        (let* 
            ((sbprogfile (car arglist));; get filename
            (program (readlist-from-inputfile sbprogfile));; read file
            (result (convert program 0))) 0 )))

;; run main
(main (vector->list (current-command-line-arguments)))
