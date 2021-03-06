(defmacro return-first (expr &rest more)
  "Evaluates all given expressions but returns the first given expression."
  `(funcall (lambda (e) ,@more e) ,expr))

(let ((input nil) (lines 1) (lookahead nil) (reserved-chars nil))
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

    (defun peek? (c)
        (char= (peek) c))

    (defun discard-newline ()
        (if (and (peek) (peek? #\Newline))
            (progn (incf lines) (discard))))

    (defun expected (e &optional (f nil))
        (close-input)
        (if f
            (error "Expected `~A' on line ~D. Found `~A' instead." e lines f)
            (error "Expected `~A' on line ~D" e lines)))

    (defun reserved (stream char)
      "Catch any macro character reads as parse errors."
        (declare (ignore stream))
        (close-input)
        (error "Unexpected symbol `~S' on line ~D" char lines))

    (defun reserved? (c)
      (if (find c reserved-chars :test #'char=) t nil))

    (defun with-reserved (&rest chars)
      "Setting macro characters tells the read|peek|read-char suite to treat
      that char as a delimiter."
      (setf reserved-chars chars)
      (mapcar #'(lambda (c) (set-macro-character c 'reserved)) chars))

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
      (if (reserved? (peek))
          nil
          (progn
              (if (not lookahead)
                  (setq lookahead (read input nil nil)))
              (eq lookahead sym)))))

(defun p-assertion ()
  "<assertion> ::= <symbol> is <symbol> [and <symbol>]+."
  (let ((obj (next)))
    (if (not (next? 'is))
        (expected "is" (next)))
    (next)
    (let ((fact (next)) (form nil))
        (if (next? 'and)
            (setq form `(assertion ',obj ',fact ,@(loop while (next? 'and) 
                                                        do (next) ; skip 'and'
                                                        collect `',(next))))
            (setq form `(assertion ',obj ',fact)))
        (if (not (peek? #\.))
            (expected "." (peek)))
        (discard)
        form)))

(defun p-question-collect (obj)
  "[and <symbol>]+?
   Collects questions as sub lists into a super list."
  (let ((fact (next)))
    (if (next? 'and)
        (progn 
          (next) ; skip 'and'
          `((question ',obj ',fact) ,@(p-question-and obj)))
        (if (not (peek? #\?))
            (expected "?" (peek))
            (progn
                (discard)
                `((question ',obj ',fact)))))))

(defun p-question ()
  "<question> ::= is <symbol> <symbol> [and <symbol>]+?"
  (next) ; skip 'is'
  (let ((obj (next)))
    (let ((questions (p-question-collect obj)))
      (if (> (length questions) 1)
          `(and ,@questions)
          `(,@(car questions))))))

(defun p-expr ()
  "<expr> ::= <assertion> | <question>"
  (cond ((next? 'is) (p-question))
        (t (p-assertion))))

(defun parse (source)
  "Parse an input file into an AST."
  (let ((*readtable* (copy-readtable)))
    (with-reserved #\Newline #\. #\; #\?)
    (open-input source)
    (return-first
      (loop
        while (peek)
        collect (p-expr))
      (close-input))))

(defvar *objects* (make-hash-table))
(defvar *facts* (make-hash-table))

(defmacro key-exists? (hash key)
  `(not (eq (gethash ,key ,hash) nil)))

(defmacro get-object (name)
  `(gethash ,name *objects*))

(defun object? (name)
  (key-exists? *objects* name))

(defun make-object (name)
  (setf (get-object name) (cons name nil)))

(defun object-add-fact (name fact)
  (setf (get-object name) (nconc (get-object name) `(,fact))))

(defun assertion (obj fact &rest facts)
  (if (not (object? obj))
      (make-object obj))
  (object-add-fact obj fact)
  (if facts (mapcar #'(lambda (f) (object-add-fact obj f)) facts))
  t)

(defun question (object fact)
  (if (find fact (get-object object) :test #'eq)
      t
      nil))

(defmacro run (source)
  `(progn ,@(eval source)))
