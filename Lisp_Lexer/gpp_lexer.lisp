;;Can BEYAZNAR
;;161044038

(defvar Keywords '(   "and" "or" "not" "equal" "less" "nil" "list"
                    "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp"
                    "true" "false" ))

(defvar operators '( "+" "-" "/" "*" "(" ")" "**" "''" "," ))
(defvar KW_Tokens '(  "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST"
                    "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF"
                    "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" ))

(defvar OP_Tokens '(  "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP"
                    "OP_OC" "OP_CC" "OP_COMMA" "OP_DBLMULT"))

(defvar CVI_Tokens '("COMMENT" "VALUE" "IDENTIFIER")) ;; Comment Value Identifier

(defvar Integers '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(defvar Letters '(    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))


(defvar tokenList '()) ;; global variable for lexer function 
(defvar output-file "parsed_lisp.txt")
(defvar output-path nil)

(defun is-in-list-string (element list)
    (let ((index 0))
    (dolist (eachElem list)
        (if (string= element eachElem) (return-from is-in-list-string index))
        (setf index (+ index 1))
    )
    (return-from is-in-list-string NIL)
    )
)

(defun print-tokenlist-to-file ()
    (let ( (list-len (length tokenList)) (i 0))
        (loop
        (when (>= i list-len) (return ))
        (format output-path "~D~%" (nth i tokenList))
        (setq i (+ i 1)))
    )
)

(defun gppinterpreter (filename)

    (let ((eachLine '()))
    (if (eq filename nil)
        (progn ;; read from terminal
            (let ((user-input nil) )
                (loop
                    (setq user-input (read-line ))
                    (if (string=  user-input "") (return-from gppinterpreter nil))
                    (setq eachLine '())
                    (let ((len-input (length user-input)) (i 0) (tempChar nil))
                        (loop 
                            (when (>= i len-input) (return))
                            (setq tempChar (char user-input i))
                            (cond 
                            (
                                (char= tempChar #\newline)
                                ;;(if (eq NIL (lexer eachLine)) (return-from gppinterpreter NIL) )  
                                (lexer eachLine)
                                (setf eachLine '())                   
                            )
                            (   
                                (string= tempChar "(" )
                                (setq eachLine (append eachLine (list tempChar)))
                                (setq eachLine (append eachLine (list #\space)))                             
                            )

                            (
                                (string= tempChar ")" )
                                (setq eachLine (append eachLine (list #\space)))
                                (setq eachLine (append eachLine (list tempChar)))
                            )
                            
                            (t (setq eachLine (append eachLine (list tempChar))))
                            ) 
                            (setq i (+ i 1))
                        )
                        ;;(if (eq NIL (lexer eachLine)) (return-from gppinterpreter NIL) )
                        (lexer eachLine)
                        (print-tokenlist-to-file )
                        (setq tokenList '())
                    )
                    


                )
            )
        )
        (progn ;; read from file
            (setq eachLine '())
            (let ((in (open filename :if-does-not-exist nil)) (read-string nil) (tempChar nil) )

                ;(setq read-string (string-downcase (string (read-line))))
                ;;(if )
                (when in
                    (loop for tempChar = (read-char in nil)
                        while tempChar do
                        (cond 
                            (
                                (char= tempChar #\newline)
                                ;;(if (eq NIL (lexer eachLine)) (return-from gppinterpreter NIL) )  
                                (lexer eachLine)                                
                                (setf eachLine '())                   
                            )
                            (   
                                (string= tempChar "(" )
                                (setq eachLine (append eachLine (list tempChar)))
                                (setq eachLine (append eachLine (list #\space)))                             
                            )

                            (
                                (string= tempChar ")" )
                                (setq eachLine (append eachLine (list #\space)))
                                (setq eachLine (append eachLine (list tempChar)))
                            )
                            
                            (t 
                            (setq eachLine (append eachLine (list tempChar))) 
                            )
                        )            
                    )			
                    (close in)
                )
                ;(if (eq NIL (lexer eachLine)) (return-from gppinterpreter NIL) )
                (lexer eachLine)
                (print-tokenlist-to-file )
                (setq tokenList '())
            )
            (return-from gppinterpreter NIL)
        
        )
    
    
    )

    )
    

)

(defun lexer(line)
    (let ((tempList '()) (sizeLine (length line)) (index 0) (tempChar nil) (wordList '()))
        (loop 
        (when (>= index sizeLine) (return ))
        
        (setq tempChar (nth index line))
        (cond

            ( (equal tempChar #\newline) (return-from lexer t) )
            
            ;;for comment line
            (
                
                (equal t (and (string= tempChar ";" ) (string= (nth (+ index 1) line) ";" )))
                
                (setq tokenList (append tokenList (list (nth 0 CVI_Tokens))))
                (setq index (+ index 1))
                (return-from lexer t)
            )

            ;;for operators
            (
                (not (equal (is-in-list-string tempChar operators)  NIL))
                (setq temp (is-in-list-string tempChar operators))
                
                (if (and (string= (nth index line) "*") (string= (nth (+ index 1) line) "*") )
                    (progn
                        (setq tokenList (append tokenList (list (nth 9 OP_Tokens))))
                        (setq index (+ index 1))
                    )
                    (setq tokenList (append tokenList (list (nth temp OP_Tokens))))  
                )
                         
            )

            ;;for integers
            (
                (not (equal (is-in-list-string tempChar Integers)  NIL))
                (setq error NIL)
                (setq secondLoopIndex 0)
                (setq valueList '())
                
                (loop 
                    (when (= (+ secondLoopIndex index) sizeLine) (return ))
                    
                    (setf tempCharLoop (nth (+ secondLoopIndex index) line))
                    (if (or (equal tempCharLoop #\Space) (equal tempCharLoop NIL)) (return ))

                    ;;there will be syntax error for example "123a45"
                    (if (equal (is-in-list-string tempCharLoop Integers)  NIL)(setf error t))
       
                    (setf valueList (append valueList (list tempCharLoop)))
                    (setf secondLoopIndex (+ secondLoopIndex 1))
                )
                
                (if (eq t error) 
                    (progn 
                        (let ((error-msg "SYNTAX_ERROR ") (error-msg-sec " cannot be tokenized"))
                            (setq error-msg (concatenate 'string error-msg (concatenate 'string valueList)))
                            (setq error-msg (concatenate 'string error-msg error-msg-sec))
                            (setf tokenList (append tokenList (list error-msg)))
                        )
                        ;(format output-path "SYNTAX_ERROR ~S cannot be tokenized~%" (concatenate 'string valueList))
                        
                        (return-from lexer NIL)
                    )
                )
                
                (setf tokenList (append tokenList (list (nth 1 CVI_Tokens))))
                (setf index (+ index (- secondLoopIndex 1)))
            )

            (
                ;;for identifiers and keywords
                (not (equal (is-in-list-string tempChar Letters)  NIL))
                (setq wordList '())
                
                (setf secondLoopIndex 0)
                (loop 
                    (when (= (+ secondLoopIndex index) sizeLine) (return ))                   
                    (setq tempChar (nth (+ secondLoopIndex index) line))

                    (if (or (equal tempChar #\space) (equal tempChar #\newline)) (return ) )
                    (if (and (eq (is-in-list-string tempChar Letters ) nil) 
                            (eq (is-in-list-string tempChar Integers ) nil)) (
                        progn
                        (let ((error-msg "SYNTAX_ERROR ") (error-msg-sec " cannot be tokenized"))
                            (setq error-msg (concatenate 'string error-msg (string tempChar)))
                            (setq error-msg (concatenate 'string error-msg error-msg-sec))
                            (setf tokenList (append tokenList (list error-msg)))
                        )
                        ;(format output-path "SYNTAX_ERROR ~S cannot be tokenized~%" (concatenate 'string valueList))
                        
                        (return-from lexer NIL)

                    ))
                    
                    (setf wordList (append wordList (list tempChar)) )
                    (setf secondLoopIndex (+ secondLoopIndex 1))
                )
                (setf index (+ index (- secondLoopIndex 1)))
                (setq wordListString (concatenate 'string wordList))

                (setq control (is-in-list-string wordListString Keywords))
                (if (not (equal control NIL)) 
                    (setf tokenList (append tokenList (list (nth control KW_Tokens))))
                    (setf tokenList (append tokenList (list (nth 2 CVI_Tokens))))
                )    
            )
            (t 
                (if (not (string= tempChar " "))
                    (progn
  
                        (let ((error-msg "SYNTAX_ERROR ") (error-msg-sec " cannot be tokenized"))
                            (setq error-msg (concatenate 'string error-msg (string tempChar)))
                            (setq error-msg (concatenate 'string error-msg error-msg-sec))
                            (setf tokenList (append tokenList (list error-msg)))
                        )
                        
                        (return-from lexer NIL)
                    )
                )
                
            )   
        )
        (setq index (+ index 1))
    )
    
    (return-from lexer t)
    )
    
)
(defun get-parameter-from-terminal ()

(with-open-file (str output-file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)               
(setq output-path str)
(if (eq nil *args*) 
    (progn 
        ;; read from terminal
    (gppinterpreter nil))
    (progn   ;;interpreter with file input
        (gppinterpreter (nth 0 *args*)))
)))

(get-parameter-from-terminal)