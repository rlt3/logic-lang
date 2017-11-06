(defmacro return-first (expr &rest more)
  "Evaluates all given expressions but returns the first given expression."
  `(funcall (lambda (e) ,@more e) ,expr))

(let ((input nil)
      (lines 1))

    (defun open-input (source)
        (setf input (open source :if-does-not-exist nil))
        (setf lines 1)
        t)

    (defun close-input ()
        (close input)
        (setf input nil)
        t)

    (defun ++line-number ()
        (incf lines))

    (defun line-number ()
        lines)

    (defun peek ()
        (peek-char t input nil nil))

    (defun discard-newline ()
        (if (and (peek) (char= (peek) #\Newline))
            (progn (++line-number) (discard))))

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
      "Get the next symbol from input. Discarding any newlines come across 
      during regular expression expression."
        (discard-newline)
        (read input nil nil))

    (defun next? (sym)
      "Read the next symbol as `look' and test if `sym' is equal to `look'. 
      Then put the read symbol back into the input buffer."
        (let ((look (read input nil nil)))
            (mapcar #'(lambda (c) (unread-char c input))
                     (coerce (reverse 
                               (concatenate ; with extra delimiting whitespace
                                'string (symbol-name look) " "))
                             'list))
            (eq look sym))))


(defun assertion ()
  "<assertion> ::= <symbol> is <symbol>."
  (let ((obj (next)))
    (if (not (eq (next) 'is))
        (expected "is"))
    (let ((fact (next)))
        (if (not (char= (peek) #\.))
            (expected "."))
        (progn
            (discard)
            `(is= ,obj ,fact)))))

(defun question ()
  "<question> ::= is <symbol> <symbol>?"
  (next) ; skip 'is'
  (let ((obj (next))
        (fact (next)))
    (if (not (char= (peek) #\?))
       (expected "?"))
    (progn
        (discard)
        `(is? ,obj ,fact))))

(defun expr ()
  "<expr> ::= <assertion> | <question>"
  (cond ((next? 'is) (question))
        (t (assertion))))

(defun parse (source)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\Newline 'reserved)
    (set-macro-character #\. 'reserved)
    (set-macro-character #\; 'reserved)
    (set-macro-character #\? 'reserved)
    (open-input source)
    (return-first
      (loop
        while (peek)
        collect (expr))
      (close-input))))
