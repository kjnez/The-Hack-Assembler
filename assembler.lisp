;;;; assembler.lisp

(in-package #:assembler)

(defparameter *dest-table*
  '(("null" . "000")
    ("M"    . "001")
    ("D"    . "010")
    ("MD"   . "011")
    ("A"    . "100")
    ("AM"   . "101")
    ("AD"   . "110")
    ("AMD"  . "111")))

(defparameter *jump-table*
  '(("null" . "000")
    ("JGT"  . "001")
    ("JEQ"  . "010")
    ("JGE"  . "011")
    ("JLT"  . "100")
    ("JNE"  . "101")
    ("JLE"  . "110")
    ("JMP"  . "111")))

(defparameter *comp-table*
  '(
    ;; When a=0
    ("0"   . "0101010")
    ("1"   . "0111111")
    ("-1"  . "0111010")
    ("D"   . "0001100")
    ("A"   . "0110000")
    ("!D"  . "0001101")
    ("!A"  . "0110001")
    ("-D"  . "0001111")
    ("-A"  . "0110011")
    ("D+1" . "0011111")
    ("A+1" . "0110111")
    ("D-1" . "0001110")
    ("A-1" . "0110010")
    ("D+A" . "0000010")
    ("D-A" . "0010011")
    ("A-D" . "0000111")
    ("D&A" . "0000000")
    ("D|A" . "0010101")
    ;; When a=1
    ("M"   . "1110000")
    ("!M"  . "1110001")
    ("-M"  . "1110011")
    ("M+1" . "1110111")
    ("M-1" . "1110010")
    ("D+M" . "1000010")
    ("D-M" . "1010011")
    ("M-D" . "1000111")
    ("D&M" . "1000000")
    ("D|M" . "1010101")))

(defun initialize-symbols ()
  (add-entry "SP"     #x0000)
  (add-entry "LCL"    #x0001)
  (add-entry "ARG"    #x0002)
  (add-entry "THIS"   #x0003)
  (add-entry "THAT"   #x0004)
  (add-entry "R0"     #x0000)
  (add-entry "R1"     #x0001)
  (add-entry "R2"     #x0002)
  (add-entry "R3"     #x0003)
  (add-entry "R4"     #x0004)
  (add-entry "R5"     #x0005)
  (add-entry "R6"     #x0006)
  (add-entry "R7"     #x0007)
  (add-entry "R8"     #x0008)
  (add-entry "R9"     #x0009)
  (add-entry "R10"    #x000a)
  (add-entry "R11"    #x000b)
  (add-entry "R12"    #x000c)
  (add-entry "R13"    #x000d)
  (add-entry "R14"    #x000e)
  (add-entry "R15"    #x000f)
  (add-entry "SCREEN" #x4000)
  (add-entry "KBD"    #x6000))

(defun remove-comment (str)
  "Remove comment and trailing spaces."
  (let ((idx (search "//" str)))
    (if idx
	(string-trim " " (subseq str 0 idx))
	(string-trim " " str))))

(defun get-instructions (file)
  (let ((lines (uiop:read-file-lines file)))
    (mapcar #'remove-comment
	    (remove-if
	     #'(lambda (str) (or (string= "" str) (string= (elt str 0) "/")))
	     lines))))

(defun write-file (binary-list filename)
  (with-open-file (stream filename :direction :output)
    (loop for str in binary-list do
	  (format stream (concatenate 'string str "~%")))))

(defun command-type (command)
  "Returns A-COMMAND, C-COMMAND or L-COMMAND."
  (cond ((string= (elt command 0) "@")
	 'A-COMMAND)
	((string= (elt command 0) "(")
	 'L-COMMAND)
	(t 'C-COMMAND)))

(defun get-symbol (command)
  "Return the symbol or decimal Xxx of the current command @Xxx or (Xxx). Should be called only when command-type is A-COMMAND or L-COMMAND"
  (if (string= (elt command 0) "@")
      (subseq command 1)
      (subseq command 1 (1- (length command)))))

(defun get-dest (command)
  "Return the dest mnemonic in the current C-COMMAND."
  (let ((idx (search "=" command)))
    (if idx
	(subseq command 0 idx)
	"null")))

(defun get-comp (command)
  "Return the comp mnemonic in the current C-COMMAND."
  (let* ((idx-eq (search "=" command))
	 (idx-semi (search ";" command))
	 (start (or idx-eq -1))
	 (end (or idx-semi (length command))))
    (subseq command (1+ start) end)))

(defun get-jump (command)
  "Return the jump mnemonic in the current C-COMMAND."
  (let ((idx-semi (search ";" command)))
    (if idx-semi
	(subseq command (1+ idx-semi))
	"null")))

(defun get-binary (str table)
  (cdr (assoc str table :test #'string=)))

(defun c-instruction-to-binary (command)
  (let ((comp (get-binary (get-comp command) *comp-table*))
	(dest (get-binary (get-dest command) *dest-table*))
	(jump (get-binary (get-jump command) *jump-table*)))
    (concatenate 'string "111" comp dest jump)))

(defun command-to-binary-without-symbol (command)
  (case (command-type command)
    (a-command (a-instruction-to-binary-without-symbol command))
    (c-command (c-instruction-to-binary command))
    (otherwise (error "Not Implemented!"))))

(defun assembler-without-symbols (file)
  (let ((commands (get-instructions file)))
    (mapcar #'command-to-binary-without-symbol commands)))

(defparameter *symbol-table* (make-hash-table :test 'equal))

(defun add-entry (symbol address)
  "Add (symbol address) pair to *symbol-table*."
  (setf (gethash symbol *symbol-table*) address))

(defun get-address (symbol)
  "Check if *symbol-table* contains symbol."
  (gethash symbol *symbol-table*))

(defun first-pass (commands)
  (loop with i = 0
	for command in commands
	if (equal (command-type command) 'l-command)
	  do (add-entry (get-symbol command) i)
	else
	  do (incf i)))

(defun print-hash-table (table)
  (loop for k being the hash-keys in table using (hash-value v)
	do (format t "~a:~t~a~%" k v)))

(defun a-instruction-to-binary-without-symbol (command)
  (let ((decimal-str (get-symbol command)))
    (decimal-str-to-binary decimal-str)))

(defun decimal-str-to-binary (str)
  (let* ((decimal (parse-integer str :radix 10))
	 (bin-str (write-to-string decimal :base 2))
	 (bin-len (length bin-str)))
    (concatenate 'string "0"
		 (make-string (- 15 bin-len) :initial-element #\0)
		 bin-str)))

(defun symbol-str-to-binary (str ram-address)
  (let ((address (get-address str)))
    (if address
	(decimal-str-to-binary (write-to-string address :base 10))
	(progn
	  (add-entry str ram-address)
	  (decimal-str-to-binary (write-to-string ram-address :base 10))))))

(defun a-instruction-to-binary (command ram-address)
  (let ((symbol-str (get-symbol command)))
    (if (every #'digit-char-p symbol-str)
	(decimal-str-to-binary symbol-str)
	(symbol-str-to-binary symbol-str ram-address))))

(defun command-to-binary (command ram-address)
  (case (command-type command)
    (a-command (a-instruction-to-binary command ram-address))
    (c-command (c-instruction-to-binary command))
    (otherwise nil)))

(defun second-pass (commands)
  (loop with i = 15
	for command in commands
	if (and (equal (command-type command) 'a-command)
		(not (get-address (get-symbol command)))
		(not (every #'digit-char-p (get-symbol command))))
	  do
	     (incf i)
	if (command-to-binary command i)
	  collect it))

(defun assembler (file)
  (initialize-symbols)
  (let ((commands (get-instructions file)))
    (first-pass commands)
    (second-pass commands)))

(defun from-assembly-to-binary (file output-path)
  (write-file (assembler file) output-path))
