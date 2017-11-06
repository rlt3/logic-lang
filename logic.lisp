(defmacro return-first (expr &rest more)
  "Evaluates all given expressions but returns the first given expression."
  `(funcall (lambda (e) ,@more e) ,expr))

(let ((input nil) (lines 1) (lookahead nil))
    (defun open-input (source)
        (setf input (open source :if-does-not-exist nil))
        (setf lines 1)
        (setf lookahead nil)
        t)

    (defun close-input ()
        (close input)
        (setf input nil)
        t)

    (defun peek ()
        (peek-char t input nil nil))

    (defun discard-newline ()
        (if (and (peek) (char= (peek) #\Newline))
            (progn (incf lines) (discard))))

    (defun expected (s)
        (close-input)
        (error "Expected `~A' on line ~D" s lines))

    (defun reserved (stream char)
      "Catch any macro character reads as parse errors. Setting macro 
      characters tells the read|peek|read-char suite to treat that char as a
      delimiter."
        (declare (ignore stream))
        (close-input)
        (error "Unexpected symbol `~S' on line ~D" char lines))

    (defun discard ()
      "Discard next char. Look ahead and discard any newlines as well because
      newlines are often right after a discarded char."
        (read-char input nil nil)
        (discard-newline))

    (defun next ()
      "Get the next symbol from input. If there's a lookahead symbol, return
      that and discard it. Otherwise read next symbol from the buffer."
      (discard-newline)
      (if lookahead
          (return-first
              lookahead
              (setq lookahead nil))
          (read input nil nil)))

    (defun next? (sym)
      "Test the lookahead against `sym'. If there's no lookahead, set it as
      the next symbol read from the input buffer."
      (if (not lookahead)
          (setq lookahead (read input nil nil)))
      (eq lookahead sym)))

(defun p-assertion ()
  "<assertion> ::= <symbol> is <symbol>."
  (let ((obj (next)))
    (if (not (eq (next) 'is))
        (expected "is"))
    (let ((fact (next)))
        (if (not (char= (peek) #\.))
            (expected "."))
        (progn
            (discard)
            `(assertion ',obj ',fact)))))

(defun p-question ()
  "<question> ::= is <symbol> <symbol>?"
  (next) ; skip 'is'
  (let ((obj (next))
        (fact (next)))
    (if (not (char= (peek) #\?))
       (expected "?"))
    (progn
        (discard)
        `(question ',obj ',fact))))

(defun p-expr ()
  "<expr> ::= <assertion> | <question>"
  (cond ((next? 'is) (p-question))
        (t (p-assertion))))

(defun parse (source)
  "Parse an input file into an AST."
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\Newline 'reserved)
    (set-macro-character #\. 'reserved)
    (set-macro-character #\; 'reserved)
    (set-macro-character #\? 'reserved)
    (open-input source)
    (return-first
      (loop
        while (peek)
        collect (p-expr))
      (close-input))))
