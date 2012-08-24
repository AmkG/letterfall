; kernel.scm
; Implementation of the sweet-expressions project by readable mailinglist.
;
; Copyright (C) 2005-2012 by Egil Möller, David A. Wheeler,
;   and Alan Manuel K. Gloria.
;
; This software is released as open source software under the "MIT" license:
;
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE.

; This file includes code from SRFI-49, but significantly modified.

; -----------------------------------------------------------------------------
; Compatibility Layer
; -----------------------------------------------------------------------------
; The compatibility layer is composed of:
;
;   (readable-kernel-module-contents (exports ...) body ...)
;   - a macro that should package the given body as a module, or whatever your
;     scheme calls it (chicken eggs?), preferably with one of the following
;     names, in order of preference, depending on your Scheme's package naming
;     conventions/support
;       (readable kernel)
;       readable/kernel
;       readable-kernel
;       sweetimpl
;   - The first element after the module-contents name is a list of exported
;     functions.  This module shall never export a macro or syntax, not even
;     in the future.
;   - If your Scheme requires module contents to be defined inside a top-level
;     module declaration (unlike Guile where module contents are declared as
;     top-level entities after the module declaration) then the other
;     functions below should be defined inside the module context in order
;     to reduce user namespace pollution.
;
;   (my-peek-char port)
;   (my-read-char port)
;   - Performs I/O on a "port" object.
;   - The algorithm assumes that port objects have the following abilities:
;     * The port automatically keeps track of source location
;       information.  On R5RS there is no source location
;       information that can be attached to objects, so as a
;       fallback you can just ignore source location, which
;       will make debugging using sweet-expressions more
;       difficult.
;   - "port" or fake port objects are created by the make-read function
;     below.
;   
;   (make-read function)
;   - The given function accepts exactly 1 argument, a "fake port" that can
;     be passed to my-peek-char et al.
;   - make-read creates a new function that supports your Scheme's reader
;     interface.  Usually, this means making a new function that accepts
;     either 0 or 1 parameters, defaulting to (current-input-port).
;   - If your Scheme doesn't support unlimited lookahead, you should make
;     the fake port that supports 2-char lookahead at this point.
;   - If your Scheme doesn't keep track of source location information
;     automatically with the ports, you may again need to wrap it here.
;   - If your Scheme needs a particularly magical incantation to attach
;     source information to objects, then you might need to use a weak-key
;     table in the attach-sourceinfo function below and then use that
;     weak-key table to perform the magical incantation.
;
;   (invoke-read read port)
;   - Accepts a read function, which is a (most likely built-in) function
;     that requires a *real* port, not a fake one.
;   - Should unwrap the fake port to a real port, then invoke the given
;     read function on the actual real port.
;
;   (get-sourceinfo port)
;   - Given a fake port, constructs some object (which the algorithm treats
;     as opaque) to represent the source information at the point that the
;     port is currently in.
;
;   (attach-sourceinfo pos obj)
;   - Attaches the source information pos, as constructed by get-sourceinfo,
;     to the given obj.
;   - obj can be any valid Scheme object.  If your Scheme can only track
;     source location for a subset of Scheme object types, then this function
;     should handle it gracefully.
;   - Returns an object with the source information attached - this can be
;     the same object, or a different object that should look-and-feel the
;     same as the passed-in object.
;   - If source information cannot be attached anyway (your Scheme doesn't
;     support attaching source information to objects), just return the
;     given object.
;
;   (replace-read-with f)
;   - Replaces your Scheme's current reader.
;   - Replace 'read and 'get-datum at the minimum.  If your Scheme
;     needs any kind of involved magic to handle load and loading
;     modules correctly, do it here.
;
;   next-line
;   line-separator
;   paragraph-separator
;   - The Unicode characters with those names.
;   - If your Scheme does *not* support Unicode, define these to be #f.
;   - If your Scheme *does* support Unicode, to prevent other Schemes
;     from misreading this file, use the following defines:
;       (define next-line (integer->char #x0085))
;       (define line-separator (integer->char #x2028))
;       (define paragraph-separator (integer->char #x2029))
;
;   (parse-hash top-read char fake-port)
;   - a function that is invoked when an unrecognized, non-R5RS hash
;     character combination is encountered in the input port.
;   - this function is passed a "fake port", as wrapped by the
;     make-read function above.  You should probably use my-read-char
;     and my-peek-char in it, or at least unwrap the port (since
;     make-read does the wrapping, and you wrote make-read, we assume
;     you know how to unwrap the port).
;   - if your function needs to parse a datum, invoke
;     (top-read fake-port).  Do NOT use any other read function.  The
;     top-read function accepts exactly one parameter - the fake port
;     this function was passed in.
;     - top-read is either a version of curly-infix-read, or a version
;       of neoteric-read; this specal version accepts only a fake port.
;       It is never a version of sweet-read.  You don't normally want to
;       call sweet-read, because sweet-read presumes that it's starting
;       at the beginning of the line, with indentation processing still
;       active.  There's no reason either must be true when processing "#".
;   - At the start of this function, both the # and the character
;     after it have been read in.
;   - The function returns one of the following:
;       #f  - the hash-character combination is invalid/not supported.
;       ()  - the hash-character combination introduced a comment;
;             at the return of this function with this value, the
;             comment has been removed from the input port.
;       (a) - the datum read in is the value a
;
;   hash-pipe-comment-nests?
;   - a Boolean value that specifies whether #|...|# comments
;     should nest.


; On Guile 2.0, the define-module part needs to occur separately from
; the rest of the compatibility checks, unfortunately.  Sigh.
(cond-expand
  (guile
    ; define the module
    ; this ensures that the user's module does not get contaminated with
    ; our compatibility functions/macros
    (define-module (readable kernel))))
(cond-expand
; -----------------------------------------------------------------------------
; Guile Compatibility
; -----------------------------------------------------------------------------
  (guile

    ; properly get bindings
    (use-modules (guile))

    ; On Guile 1.x defmacro is the only thing supported out-of-the-box.
    ; This form still exists in Guile 2.x, fortunately.
    (defmacro readable-kernel-module-contents (exports . body)
      `(begin (export ,@exports)
              ,@body))

    ; Guile was the original development environment, so the algorithm
    ; practically acts as if it is in Guile.
    ; Needs to be lambdas because otherwise Guile 2.0 acts strangely,
    ; getting confused on the distinction between compile-time,
    ; load-time and run-time (apparently, peek-char is not bound
    ; during load-time).
    (define (my-peek-char p)     (peek-char p))
    (define (my-read-char p)     (read-char p))

    (define (make-read f)
      (lambda args
        (let ((port (if (null? args) (current-input-port) (car args))))
          (f port))))

    (define (invoke-read read port)
      (read port))

    ; create a list with the source information
    (define (get-sourceinfo port)
      (list (port-filename port)
            (port-line port)
            (port-column port)))
    ; destruct the list and attach, but only to cons cells, since
    ; only that is reliably supported across Guile versions.
    (define (attach-sourceinfo pos obj)
      (cond
        ((pair? obj)
          (set-source-property! obj 'filename (list-ref pos 0))
          (set-source-property! obj 'line     (list-ref pos 1))
          (set-source-property! obj 'column   (list-ref pos 2))
          obj)
        (#t
          obj)))

    ; To properly hack into 'load and in particular 'use-modules,
    ; we need to hack into 'primitive-load.  On 1.8 and 2.0 there
    ; is supposed to be a current-reader fluid that primitive-load
    ; hooks into, but it seems (unverified) that each use-modules
    ; creates a new fluid environment, so that this only sticks
    ; on a per-module basis.  But if the project is primarily in
    ; sweet-expressions, we would prefer to have that hook in
    ; *all* 'use-modules calls.  So our primitive-load uses the
    ; 'read global variable if current-reader isn't set.

    (define %sugar-current-load-port #f)
    ; replace primitive-load
    (define primitive-load-replaced #f)
    (define (setup-primitive-load)
      (cond
        (primitive-load-replaced
           (values))
        (#t
          (module-set! (resolve-module '(guile)) 'primitive-load
            (lambda (filename)
              (let ((hook (cond
                            ((not %load-hook)
                              #f)
                            ((not (procedure? %load-hook))
                              (error "value of %load-hook is neither procedure nor #f"))
                            (#t
                              %load-hook))))
                (cond
                  (hook
                    (hook filename)))
                (let* ((port      (open-input-file filename))
                       (save-port port))
                  (define (load-loop)
                    (let* ((the-read
                             (or
                                 ; current-reader doesn't exist on 1.6
                                 (if (string=? "1.6" (effective-version))
                                     #f
                                     (fluid-ref current-reader))
                                 read))
                           (form (the-read port)))
                      (cond
                        ((not (eof-object? form))
                          ; in Guile only
                          (primitive-eval form)
                          (load-loop)))))
                  (define (swap-ports)
                    (let ((tmp %sugar-current-load-port))
                      (set! %sugar-current-load-port save-port)
                      (set! save-port tmp)))
                  (dynamic-wind swap-ports load-loop swap-ports)
                  (close-input-port port)))))
          (set! primitive-load-replaced #t))))

    (define (replace-read-with f)
      (setup-primitive-load)
      (set! read f))

    ; define Unicode chars based on version.  On 1.x assume
    ; no Unicode (actually 1.9 has Unicode, but that's not a
    ; stable branch.)
    (define has-unicode
      (let* ((v (effective-version))
             (c (string-ref v 0)))
        (if (or (char=? c #\0) (char=? c #\1))
            #f
            #t)))
    (define next-line
      (if has-unicode
          (integer->char #x0085)
          #f))
    (define line-separator
      (if has-unicode
          (integer->char #x2028)
          #f))
    (define paragraph-separator
      (if has-unicode
          (integer->char #x2028)
          #f))

    ; Guile has #! !# comments; these comments do *not* nest.
    ; On Guile 1.6 and 1.8 the only comments are ; and #! !#
    ; On Guile 2.0, #; (SRFI-62) and #| #| |# |# (SRFI-30) comments exist.
    ; On Guile 2.0, #' #` #, #,@ have the R6RS meaning; on
    ; Guile 1.8 and 1.6 there is a #' syntax but I have yet
    ; to figure out what exactly it does.
    ; On Guile, #:x is a keyword.  Keywords have symbol
    ; syntax.
    (define (parse-hash top-read char fake-port)
      (let* ((ver (effective-version))
             (c   (string-ref ver 0))
             (>=2 (and (not (char=? c #\0)) (not (char=? c #\1)))))
        (cond
          ((char=? char #\!)
            ; non-nestable comment #! ... !#
            (non-nest-comment fake-port)
            '())
          ((char=? char #\:)
            ; On Guile 1.6, #: reads characters until it finds non-symbol
            ; characters.
            ; On Guile 1.8 and 2.0, #: reads in a datum, and if the
            ; datum is not a symbol, throws an error.
            ; Follow the 1.8/2.0 behavior as it is simpler to implement,
            ; and even on 1.6 it is unlikely to cause problems.
            ; NOTE: This behavior means that #:foo(bar) will cause
            ; problems on neoteric and higher tiers.
            (let ((s (top-read fake-port)))
              (if (symbol? s)
                  `( ,(symbol->keyword s) )
                  #f)))
          ; On Guile 2.0 #' #` #, #,@ have the R6RS meaning.
          ; guard against it here because of differences in
          ; Guile 1.6 and 1.8.
          ((and >=2 (char=? char #\'))
            `( (syntax ,(top-read fake-port)) ))
          ((and >=2 (char=? char #\`))
            `( (quasisyntax ,(top-read fake-port)) ))
          ((and >=2 (char=? char #\,))
            (let ((c2 (my-peek-char fake-port)))
              (cond
                ((char=? c2 #\@)
                  (my-read-char fake-port)
                  `( (unsyntax-splicing ,(top-read fake-port)) ))
                (#t
                  `( (unsyntax ,(top-read fake-port)) )))))
          ; #{ }# syntax
          ((char=? char #\{ )  ; Special symbol, through till ...}#
            `( ,(list->symbol (special-symbol fake-port))))
          (#t
            #f))))

    ; detect the !#
    (define (non-nest-comment fake-port)
      (let ((c (my-read-char fake-port)))
        (cond
          ((eof-object? c)
            (values))
          ((char=? c #\!)
            (let ((c2 (my-peek-char fake-port)))
              (if (char=? c2 #\#)
                  (begin
                    (my-read-char fake-port)
                    (values))
                  (non-nest-comment fake-port))))
          (#t
            (non-nest-comment fake-port)))))

  ; Return list of characters inside #{...}#, a guile extension.
  ; presume we've already read the sharp and initial open brace.
  ; On eof we just end.  We could error out instead.
  ; TODO: actually conform to Guile's syntax.  Note that 1.x
  ; and 2.0 have different syntax when spaces, backslashes, and
  ; control characters get involved.
  (define (special-symbol port)
    (cond
      ((eof-object? (my-peek-char port)) '())
      ((eqv? (my-peek-char port) #\})
        (my-read-char port) ; consume closing brace
        (cond
          ((eof-object? (my-peek-char port)) '(#\}))
          ((eqv? (my-peek-char port) #\#)
            (my-read-char port) ; Consume closing sharp.
            '())
          (#t (append '(#\}) (special-symbol port)))))
      (#t (append (list (my-read-char port)) (special-symbol port)))))

    (define hash-pipe-comment-nests? #t)

    )
; -----------------------------------------------------------------------------
; R5RS Compatibility
; -----------------------------------------------------------------------------
  (else
    ; assume R5RS with define-syntax

    ; On R6RS, and other Scheme's, module contents must
    ; be entirely inside a top-level module structure.
    ; Use module-contents to support that.  On Schemes
    ; where module declarations are separate top-level
    ; expressions, we expect module-contents to transform
    ; to a simple (begin ...), and possibly include
    ; whatever declares exported stuff on that Scheme.
    (define-syntax readable-kernel-module-contents
      (syntax-rules ()
        ((readable-kernel-module-contents exports body ...)
          (begin body ...))))

    ; We use my-* functions so that the
    ; "port" automatically keeps track of source position.
    ; On Schemes where that is not true (e.g. Racket, where
    ; source information is passed into a reader and the
    ; reader is supposed to update it by itself) we can wrap
    ; the port with the source information, and update that
    ; source information in the my-* functions.

    (define (my-peek-char port) (peek-char port))
    (define (my-read-char port) (read-char port))

    ; this wrapper function wraps a reader function
    ; that accepts a "fake" port above, and converts
    ; it to an R5RS-compatible function.  On Schemes
    ; which support source-information annotation,
    ; but use a different way of annotating
    ; source-information from Guile, this function
    ; should also probably perform that attachment
    ; on exit from the given inner function.
    (define (make-read f)
      (lambda args
        (let ((real-port (if (null? args) (current-input-port) (car args))))
          (f real-port))))

    ; invoke the given "actual" reader, most likely
    ; the builtin one, but make sure to unwrap any
    ; fake ports.
    (define (invoke-read read port)
      (read port))
    ; R5RS doesn't have any method of extracting
    ; or attaching source location information.
    (define (get-sourceinfo _) #f)
    (define (attach-sourceinfo _ x) x)

    ; Not strictly R5RS but we expect at least some Schemes
    ; to allow this somehow.
    (define (replace-read-with f)
      (set! read f))

    ; Assume that a random R5RS Scheme doesn't support Unicode
    ; out-of-the-box
    (define next-line #f)
    (define line-separator #f)
    (define paragraph-separator #f)

    ; R5RS has no hash extensions
    (define (parse-hash . _) #f)

    ; Hash-pipe comment is not in R5RS, but support
    ; it as an extension, and make them nest.
    (define hash-pipe-comment-nests? #t)

    ))

; -----------------------------------------------------------------------------
; Module declaration and useful utilities
; -----------------------------------------------------------------------------
(readable-kernel-module-contents
  ; exported functions
  (; tier read functions
   curly-infix-read neoteric-read sweet-read
   ; comparison functions
   compare-read-file ; compare-read-string
   ; replacing the reader
   replace-read restore-traditional-read)

  ; special tag to denote comment return from hash-processing
  (define comment-tag (cons '() '())) ; all cons cells are unique

  ; Define the whitespace characters, in relatively portable ways
  ; Presumes ASCII, Latin-1, Unicode or similar.
  (define tab (integer->char #x0009))             ; #\ht aka \t.
  (define linefeed (integer->char #x000A))        ; #\newline aka \n. FORCE it.
  (define carriage-return (integer->char #x000D)) ; \r.
  (define line-tab (integer->char #x000D))
  (define form-feed (integer->char #x000C))
  (define space '#\space)

  (define line-ending-chars-ascii (list linefeed carriage-return))
  (define line-ending-chars
    (append
      line-ending-chars-ascii
      (if next-line
          (list next-line)
          '())
      (if line-separator
          (list line-separator)
          '())))

  ; This definition of whitespace chars is per R6RS section 4.2.1.
  ; R6RS doesn't explicitly list the #\space character, be sure to include!
  (define whitespace-chars-ascii
     (list tab linefeed line-tab form-feed carriage-return #\space))
  (define whitespace-chars
    (append
      whitespace-chars-ascii
      (if next-line
          (list next-line)
          '())
      (if line-separator
          (list line-separator)
          '())
      (if paragraph-separator
          (list paragraph-separator)
          '())))
  ; If supported, add characters whose category is Zs, Zl, or Zp

  ; Returns a true value (not necessarily #t)
  (define (char-line-ending? char) (memq char line-ending-chars))

  ; Return #t if char is space or tab.
  (define (char-horiz-whitespace? char)
    (or (eqv? char #\space)
        (eqv? char tab)))

  ; Create own version, in case underlying implementation omits some.
  (define (my-char-whitespace? c)
    (or (char-whitespace? c) (memq c whitespace-chars)))

  ; Consume an end-of-line sequence. This is 2 unequal end-of-line
  ; characters, or a single end-of-line character, whichever is longer.
  (define (consume-end-of-line port)
    (let ((c (my-peek-char port)))
      (if (char-line-ending? c)
        (begin
          (my-read-char port)
          (let ((next (my-peek-char port)))
            (if (and (not (eq? c next))
                     (char-line-ending? next))
              (my-read-char port)))))))

  (define (consume-to-eol port)
    ; Consume every non-eol character in the current line.
    ; End on EOF or end-of-line char.
    ; Do NOT consume the end-of-line character(s).
    (let ((c (my-peek-char port)))
      (cond
        ((not (or (eof-object? c)
                  (char-line-ending? c)))
          (my-read-char port)
          (consume-to-eol port)))))

  (define (ismember? item lyst)
    ; Returns true if item is member of lyst, else false.
    (pair? (member item lyst)))

  ; Quick utility for debugging.  Display marker, show data, return data.
  (define (debug-show marker data)
    (display "DEBUG: ")
    (display marker)
    (display " = ")
    (write data)
    (display "\n")
    data)


  (define (my-read-delimited-list my-read stop-char port)
    ; Read the "inside" of a list until its matching stop-char, returning list.
    ; stop-char needs to be closing paren, closing bracket, or closing brace.
    ; This is like read-delimited-list of Common Lisp, but it
    ; calls the specified reader instead.
    ; This implements a useful extension: (. b) returns b. This is
    ; important as an escape for indented expressions, e.g., (. \\)
    (consume-whitespace port)
    (let*
      ((pos (get-sourceinfo port))
       (c   (my-peek-char port)))
      (cond
        ((eof-object? c) (read-error "EOF in middle of list") c)
        ((char=? c stop-char)
          (my-read-char port)
          (attach-sourceinfo pos '()))
        ((ismember? c '(#\) #\] #\}))  (read-error "Bad closing character") c)
        (#t
          (let ((datum (my-read port)))
            (cond
               ((eq? datum '.)
                 (let ((datum2 (my-read port)))
                   (consume-whitespace port)
                   (cond
                     ((not (eqv? (my-peek-char port) stop-char))
                      (read-error "Bad closing character after . datum"))
                     (#t
                       (my-read-char port)
                       datum2))))
               (#t
                 (attach-sourceinfo pos
                   (cons datum
                     (my-read-delimited-list my-read stop-char port))))))))))

; -----------------------------------------------------------------------------
; Read Preservation and Replacement
; -----------------------------------------------------------------------------

  (define default-scheme-read read)
  (define replace-read replace-read-with)
  (define (restore-traditional-read) (replace-read-with default-scheme-read))

; -----------------------------------------------------------------------------
; Scheme Reader re-implementation
; -----------------------------------------------------------------------------

; Unfortunately, since most Scheme readers will consume [, {, }, and ],
; we have to re-implement our own Scheme reader.  Ugh.
; If you fix your Scheme's "read" so that [, {, }, and ] are considered
; delimiters (and thus not consumed when reading symbols, numbers, etc.),
; you can just call default-scheme-read instead of using underlying-read below,
; with the limitation that vector constants #(...) will not support curly-infix
; or neoteric-function-expressions.
; We WILL call default-scheme-read on string reading (that DOES seem to work
; in common cases, and lets us use the implementation's string extensions).

  ; See R6RS section 4.2.1
  (define neoteric-delimiters
     (append (list #\( #\) #\[ #\] #\{ #\})
             (list #\" #\; #\#)   ; TODO: ADD???
             whitespace-chars))

  (define (consume-whitespace port)
    (let ((char (my-peek-char port)))
      (cond
        ((eqv? char #\;)
          (consume-to-eol port)
          (consume-whitespace port))
        ((my-char-whitespace? char)
          (my-read-char port)
          (consume-whitespace port)))))

  (define (read-until-delim port delims)
    ; Read characters until eof or a character in "delims" is seen.
    ; Do not consume the eof or delimiter.
    ; Returns the list of chars that were read.
    (let ((c (my-peek-char port)))
      (cond
         ((eof-object? c) '())
         ((ismember? c delims) '())
         (#t (cons (my-read-char port) (read-until-delim port delims))))))

  (define (read-error message)
    (display "Error: ")
    (display message)
    '())

  (define (read-number port starting-lyst)
    (string->number (list->string
      (append starting-lyst
        (read-until-delim port neoteric-delimiters)))))


  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (my-peek-char port)) (my-peek-char port))
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (my-read-char port))
              (rest (read-until-delim port neoteric-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (#t
              (let ((rest-string (list->string (cons c rest))))
                (cond
                  ((string-ci=? rest-string "space") #\space)
                  ((string-ci=? rest-string "newline") #\newline)
                  ((string-ci=? rest-string "ht") tab)  ; Scheme extension.
                  ((string-ci=? rest-string "tab") tab) ; Scheme extension.
                  (#t (read-error "Invalid character name"))))))))))


  ; NOTE: this function can return comment-tag.  Program defensively
  ; against this when calling it.
  (define (process-sharp top-read port)
    ; We've peeked a # character.  Returns what it represents.
    ; Note: Since we have to re-implement process-sharp anyway,
    ; the vector representation #(...) uses my-read-delimited-list, which in
    ; turn calls top-read.
    ; TODO: Create a readtable for this case.
    (my-read-char port) ; Remove #
    (cond
      ((eof-object? (my-peek-char port)) (my-peek-char port)) ; If eof, return eof.
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (my-read-char port)))
          (cond
            ((char-ci=? c #\t)  #t)
            ((char-ci=? c #\f)  #f)
            ((ismember? c '(#\i #\e #\b #\o #\d #\x
                            #\I #\E #\B #\O #\D #\X))
              (read-number port (list #\# (char-downcase c))))
            ((char=? c #\( )  ; Vector.
              (list->vector (my-read-delimited-list top-read #\) port)))
            ((char=? c #\\) (process-char port))
            ; Handle #; (item comment).  This
            ; only works at the item-level:
            ;  it can only remove c-expressions
            ;  or n-expressions, not whole
            ; t-expressions!
            ((char=? c #\;)
              (top-read port)
              comment-tag)
            ; handle nested comments
            ((char=? c #\|)
              (nest-comment port)
              comment-tag)
            (#t
              (let ((rv (parse-hash top-read c port)))
                (cond
                  ((not rv)
                    (read-error "Invalid #-prefixed string"))
                  ((null? rv)
                    comment-tag)
                  ((pair? rv)
                    (car rv))
                  (#t
                    (read-error "****ERROR IN COMPATIBILITY LAYER parse-hash: must return #f '() or `(,obj)"))))))))))


  ; detect #| or |#
  (define (nest-comment fake-port)
    (let ((c (my-read-char fake-port)))
      (cond
        ((eof-object? c)
          (values))
        ((char=? c #\|)
          (let ((c2 (my-peek-char fake-port)))
            (if (char=? c2 #\#)
                (begin
                  (my-read-char fake-port)
                  (values))
                (nest-comment fake-port))))
        ((and hash-pipe-comment-nests? (char=? c #\#))
          (let ((c2 (my-peek-char fake-port)))
            (if (char=? c2 #\|)
                (begin
                  (my-read-char fake-port)
                  (nest-comment fake-port))
                (values))
            (nest-comment fake-port)))
        (#t
          (nest-comment fake-port)))))

  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (my-read-char port) ; Remove .
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) '.) ; period eof; return period.
        ((ismember? c digits)
          (read-number port (list #\.)))  ; period digit - it's a number.
        (#t
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol (list->string (cons #\.
            (read-until-delim port neoteric-delimiters))))))))

  ; NOTE: this function can return comment-tag.  Program defensively
  ; against this when calling it.
  (define (underlying-read top-read port)
    ; Note: This reader is case-sensitive, which is consistent with R6RS
    ; and guile, but NOT with R5RS.  Most people won't notice, and I
    ; _like_ case-sensitivity.
    (consume-whitespace port)
    (let* ((pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\")
          ; old readers tend to read strings okay, call it.
          ; (guile 1.8 and gauche/gosh 1.8.11 are fine)
          (invoke-read default-scheme-read port))
        (#t
          ; attach the source information to the item read-in
          (attach-sourceinfo pos
            (cond
              ((ismember? c digits) ; Initial digit.
                (read-number port '()))
              ((char=? c #\#) (process-sharp top-read port))
              ((char=? c #\.) (process-period port))
              ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
                (my-read-char port)
                (if (ismember? (my-peek-char port) digits)
                  (read-number port (list c))
                  (string->symbol (list->string (cons c
                    (read-until-delim port neoteric-delimiters))))))

              ; We'll reimplement abbreviations, list open, and ;.
              ; These actually should be done by neoteric-read (and thus
              ; we won't see them), but redoing it here doesn't
              ; cost us anything,
              ; and it makes some kinds of testing simpler.  It also means that
              ; this function is a fully-usable Scheme reader, and thus perhaps
              ; useful for other purposes.
              ((char=? c #\')
                (my-read-char port)
                (list (attach-sourceinfo pos 'quote)
                  (top-read port)))
              ((char=? c #\`)
                (my-read-char port)
                (list (attach-sourceinfo pos 'quasiquote)
                  (top-read port)))
              ((char=? c #\`)
                (my-read-char port)
                  (cond
                    ((char=? #\@ (my-peek-char port))
                      (my-read-char port)
                      (list (attach-sourceinfo pos 'unquote-splicing)
                       (top-read port)))
                   (#t
                    (list (attach-sourceinfo pos 'unquote)
                      (top-read port)))))
              ; The open parent calls neoteric-read, but since this one
              ; shouldn't normally be used anyway (neoteric-read
              ; will get first crack at it), it doesn't matter:
              ((char=? c #\( ) ; )
                  (my-read-char port)
                  (my-read-delimited-list top-read #\) port))
              ((char=? c #\| )
                ; Scheme extension, |...| symbol (like Common Lisp)
                ; Disable this if you don't like it.
                (my-read-char port) ; Skip |
                (let ((newsymbol
                  (string->symbol (list->string
                    (read-until-delim port '(#\|))))))
                  (my-read-char port)
                  newsymbol))
              (#t ; Nothing else.  Must be a symbol start.
                (string->symbol (list->string
                  (read-until-delim port neoteric-delimiters))))))))))

; -----------------------------------------------------------------------------
; Curly Infix
; -----------------------------------------------------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating) first
  ; ones are "op".  Used to determine if a longer lyst is infix.
  ; Otherwise it returns false.
  ; If passed empty list, returns true (so recursion works correctly).
  (define (even-and-op-prefix op lyst)
    (cond
      ((null? lyst) #t)
      ((not (pair? lyst)) #f) ; Not a list.
      ((not (eq? op (car lyst))) #f) ; fail - operators not all equal?.
      ((null? (cdr lyst)) #f) ; fail - odd # of parameters in lyst.
      (#t (even-and-op-prefix op (cddr lyst))))) ; recurse.

  ; Return True if the lyst is in simple infix format (and should be converted
  ; at read time).  Else returns NIL.
  (define (simple-infix-listp lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (symbol? (cadr lyst))  ; 2nd parameter must be a symbol.
      (even-and-op-prefix (cadr lyst) (cdr lyst)))) ; even parameters equal??

  ; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
  (define (alternating-parameters lyst)
    (if (or (null? lyst) (null? (cdr lyst)))
      lyst
      (cons (car lyst) (alternating-parameters (cddr lyst)))))

  ; Transform a simple infix list - move the 2nd parameter into first position,
  ; followed by all the odd parameters.  Thus (3 + 4 + 5) => (+ 3 4 5).
  (define (transform-simple-infix lyst)
     (cons (cadr lyst) (alternating-parameters lyst)))

  (define (process-curly lyst)
    (if (simple-infix-listp lyst)
       (transform-simple-infix lyst) ; Simple infix expression.
       (cons 'nfx lyst))) ; Non-simple; prepend "nfx" to the list.

  ; NOTE: this function can return comment-tag.  Program defensively
  ; against this when calling it.
  (define (read-at-curly top-read port)
    (let* ((pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eqv? c #\{)
          (my-read-char port)
          ; read in as infix
          (attach-sourceinfo pos
            (process-curly
              (my-read-delimited-list top-read #\} port))))
        (#t
          (underlying-read top-read port)))))

  ; NOTE: this function can return comment-tag.  Program defensively
  ; against this when calling it.
  (define (curly-infix-read-func port)
    (read-at-curly curly-infix-read-nocomment-func port))
  (define (curly-infix-read-nocomment-func port)
    (let ((rv (curly-infix-read-func port)))
      (if (eq? rv comment-tag)
          ; comment, so retry
          (curly-infix-read-nocomment-func port)
          rv)))

; -----------------------------------------------------------------------------
; Neoteric Expressions
; -----------------------------------------------------------------------------

  (define (neoteric-process-tail port prefix)
      ; See if we've just finished reading a prefix, and if so, process.
      ; This recurses, to handle formats like f(x)(y).
      ; This implements prefixed (), [], and {}
      (let* ((pos (get-sourceinfo port))
             (c   (my-peek-char port)))
        (cond
          ((eof-object? c) prefix)
          ((char=? c #\( ) ; Implement f(x).
            (my-read-char port)
            (neoteric-process-tail port
              (attach-sourceinfo pos
                (cons prefix (my-read-delimited-list neoteric-read-nocomment-func #\) port)))))
          ((char=? c #\[ )  ; Implement f[x]
            (my-read-char port)
            (neoteric-process-tail port
                (attach-sourceinfo pos
                  (cons (attach-sourceinfo pos 'bracketaccess)
                    (cons prefix
                      (my-read-delimited-list neoteric-read-nocomment-func #\] port))))))
          ((char=? c #\{ )  ; Implement f{x}
            (neoteric-process-tail port
              (attach-sourceinfo pos
                (list prefix
                  ; NOTE: although read-at-curly could return comment-tag,
                  ; at this point we know the next item is { }, so
                  ; it cannot return a comment-tag in this context
                  (read-at-curly neoteric-read-nocomment-func port)))))
          (#t prefix))))

  ; NOTE: this function can return comment-tag.  Program defensively
  ; against this when calling it.
  (define (neoteric-read-func port)
    ; Read using "neoteric Lisp notation".
    ; This implements unprefixed (), [], and {}
    (consume-whitespace port)

    (neoteric-process-tail port
      (let* ((pos (get-sourceinfo port))
             (c   (my-peek-char port)))
        ; (write c)
        (cond
          ; We need to directly implement abbreviations ', etc., so that
          ; we retain control over the reading process.
          ((eof-object? c) c)
          (#t
            (attach-sourceinfo pos
              (cond
                ((char=? c #\')
                  (my-read-char port)
                  (list (attach-sourceinfo pos 'quote)
                    (neoteric-read-nocomment-func port)))
                ((char=? c #\`)
                  (my-read-char port)
                  (list (attach-sourceinfo pos 'quasiquote)
                    (neoteric-read-nocomment-func port)))
                ((char=? c #\,)
                  (my-read-char port)
                    (cond
                      ((char=? #\@ (my-peek-char port))
                        (my-read-char port)
                        (list (attach-sourceinfo pos 'unquote-splicing)
                         (neoteric-read-nocomment-func port)))
                     (#t
                      (list (attach-sourceinfo pos 'unquote)
                        (neoteric-read-nocomment-func port)))))
                ((char=? c #\( )
                   (my-read-char port)
                   (my-read-delimited-list neoteric-read-nocomment-func #\) port))
                ((char=? c #\[ )
                    (my-read-char port)
                    (my-read-delimited-list neoteric-read-nocomment-func #\] port))
                ((char=? c #\{ )
                  (my-read-char port)
                  (process-curly
                    (my-read-delimited-list neoteric-read-nocomment-func #\} port)))
                (#t (let ((result (underlying-read neoteric-read-nocomment-func port)))
                        result)))))))))

  (define (neoteric-read-nocomment-func port)
    (let ((rv (neoteric-read-func port)))
      (if (eq? rv comment-tag)
          (neoteric-read-nocomment-func port)
          rv)))

; -----------------------------------------------------------------------------
; Sweet Expressions
; -----------------------------------------------------------------------------

  ; NOTE split et al. should not begin in #, as # causes
  ; the top-level parser to guard against multiline comments.
  (define split (string->symbol "\\\\"))
  (define split-char #\\ ) ; First character of split symbol.
  (define non-whitespace-indent #\!) ; Non-whitespace-indent char.
  (define sublist (string->symbol "$"))
  (define sublist-char #\$) ; First character of sublist symbol.

  ; This is a special unique object that is used to
  ; represent the existence of the split symbol
  ; so that readblock-clean handles it properly:
  (define split-tag (cons '() '()))

  (define (readquote level port qt)
    (let ((char (my-peek-char port)))
      (if (char-whitespace? char)
          (list qt)
          (list qt (neoteric-read-nocomment-func port)))))

  ; NOTE: this function can return comment-tag.  Program defensively
  ; against this when calling it.
  (define (readitem level port)
    (let ((pos  (get-sourceinfo port))
          (char (my-peek-char port)))
      (cond
       ((eqv? char #\`)
        (my-read-char port)
        (attach-sourceinfo pos (readquote level port 'quasiquote)))
       ((eqv? char #\')
        (my-read-char port)
        (attach-sourceinfo pos (readquote level port 'quote)))
       ((eqv? char #\,)
        (my-read-char port)
        (cond
          ((eqv? (my-peek-char port) #\@)
            (my-read-char port)
            (attach-sourceinfo pos (readquote level port 'unquote-splicing)))
          (#t
            (attach-sourceinfo pos (readquote level port 'unquote)))))
       (#t
          (neoteric-read-func port)))))

  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length indentation1))
            (len2 (string-length indentation2)))
      (and (> len1 len2)
             (string=? indentation2 (substring indentation1 0 len2)))))

  (define (accumulate-hspace port)
    (if (or (char-horiz-whitespace?     (my-peek-char port))
            (eqv? non-whitespace-indent (my-peek-char port)))
        (cons (read-char port) (accumulate-hspace port))
        '()))

  (define (indentationlevel port)
    (let* ((indent (accumulate-hspace port)) (c (my-peek-char port)))
      (cond
        ((eqv? c #\;)
          (consume-to-eol port) ; COMPLETELY ignore comment-only lines.
          (consume-end-of-line port)
          (indentationlevel port))
        ; If ONLY whitespace on line, treat as "", because there's no way
        ; to (visually) tell the difference (preventing hard-to-find errors):
        ((eof-object? c) "")
        ((char-line-ending? c) "")
        (#t (list->string indent)))))

  ;; Reads all subblocks of a block
  ;; this essentially implements the "body" production
  ;; - return value:
  ;;   cons
  ;;     next-level ;
  ;;     (xs ...) ; the body
  (define (readblocks level port)
    (let* ((pos        (get-sourceinfo port))
           (read       (readblock-clean level port))
           (next-level (car read))
           (block      (cdr read)))
      (cond
        ; check EOF
        ((eqv? next-level -1)
          (cons "" '()))
        ((string=? next-level level)
          (let* ((reads (readblocks level port))
                 (next-next-level (car reads))
                 (next-blocks (cdr reads)))
            (if (eq? block '.)
                (if (pair? next-blocks)
                    (cons next-next-level (car next-blocks))
                    (cons next-next-level next-blocks))
                (cons next-next-level
                      (attach-sourceinfo pos (cons block next-blocks))))))
        (#t
          (cons next-level (attach-sourceinfo pos (list block)))))))

  ;; Read one block of input
  ;; this essentially implements the "head" production
  ;; - return value:
  ;;   cons
  ;;     next-level ; the indentation of the line that ends this block
  ;;     expr ;       the read-in expression
  (define (readblock level port)
    (readblock-internal level port #t))
  (define (readblock-internal level port first-item?)
    (let* ((pos  (get-sourceinfo port))
           (char (my-peek-char port)))
      (cond
       ((eof-object? char)
          (cons -1 char))
       ((eqv? char #\;)
          (consume-to-eol port)
          (readblock level port))
       ((char-line-ending? char)
          (consume-end-of-line port)
          (let ((next-level (indentationlevel port)))
            (if (indentation>? next-level level)
                (readblocks next-level port)
                (cons next-level (attach-sourceinfo pos '())))))
       ((char-horiz-whitespace? char)
          (my-read-char port)
          (readblock-internal level port first-item?))
       (#t
          (let ((first (readitem level port)))
            (cond
              ((and first-item?
                    (or (equal? first '(quote))
                        (equal? first '(quasiquote))
                        (equal? first '(unquote))
                        (equal? first '(unquote-splicing))))
                (consume-horizontal-whitespace port)
                (let* ((sub-read (readblock-clean level port))
                       (outlevel (car sub-read))
                       (sub-expr (cdr sub-read)))
                  (cons outlevel (attach-sourceinfo pos `(,@first ,sub-expr)))))
              ; remove multiline comment immediately if not at
              ; start of line
              ((and (not first-item?) (eq? first comment-tag))
                (readblock-internal level port first-item?))
              ((or
                 ; treat multiline comment at start-of-line as SPLIT
                 (and first-item? (eq? first comment-tag))
                 (and (eq? char split-char) (eq? first split)))
                ; consume horizontal, non indent whitespace
                (consume-horizontal-whitespace port)
                (if first-item?
                    ;; NB: need a couple of hacks to fix
                    ;; behavior when SPLIT-by-itself
                    (if (char-line-ending? (my-peek-char port))
                        ; check SPLIT-by-itself
                        ; SPLIT-by-itself: some hacks needed
                        (let* ((sub-read (readblock level port))
                               (outlevel (car sub-read))
                               (sub-expr (cdr sub-read)))
                          ; check SPLIT followed by same indent line
                          (if (and (null? sub-expr) (string=? outlevel level))
                              ; blank SPLIT:
                              ; \
                              ; \
                              ; x
                              ; ===> x, not () () x
                              (readblock level port)
                              ; non-blank SPLIT: insert our
                              ; split-tag.  Without SPLIT-tag
                              ; readblock-clean will mishandle:
                              ; \
                              ;   x y
                              ; ==> ((x y)), which is a single
                              ; item list.  Single-item lists
                              ; are extracted, resulting in
                              ; (x y)
                              (cons outlevel (cons split-tag (attach-sourceinfo pos sub-expr)))))
                        ; not SPLIT-by-itself: just ignore it
                        (readblock-internal level port first-item?))
                    ; SPLIT-inline: end this block
                    (cons level (attach-sourceinfo pos '()))))
              ; sublist
              ((and (eq? char sublist-char) (eq? first sublist))
                (cond
                  (first-item?
                    ; Create list of rest of items.
                    ; Was: (read-error "SUBLIST found at start of line")
                    (let* ((read (readblock-clean level port))
                           (next-level (car read))
                           (block (cdr read)))
                      (cons next-level (cons split-tag (attach-sourceinfo pos (list block))))))
                  (#t
                    (consume-horizontal-whitespace port)
                    (let* ((read (readblock-clean level port))
                           (next-level (car read))
                           (block (cdr read)))
                      (cons next-level (attach-sourceinfo pos (list block)))))))
              (#t
                (let* ((rest (readblock-internal level port #f))
                       (level (car rest))
                       (block (cdr rest)))
                  ;; this check converts:
                  ;;  . foo
                  ;; ->
                  ;;  (. foo)
                  ;; ->
                  ;;  foo
                  ;; HOWEVER, it might not be compatible
                  ;; 100% with the "." as indentation
                  ;; whitespace thing.
                  (cond
                    ((eqv? level -1)
                      ; EOF encountered - end at first
                      (cons "" (list first)))
                    ((eq? first '.)
                      (if (pair? block)
                          (cons level (car block))
                          rest))
                    (#t
                      (cons level (attach-sourceinfo pos (cons first block)))))))))))))

  ;; Consumes as much horizontal, non-indent whitespace as
  ;; possible.  Treat comments as horizontal whitespace too.
  (define (consume-horizontal-whitespace port)
    (let ((char (my-peek-char port)))
      (cond
        ((char-horiz-whitespace? char)
           (my-read-char port)
           (consume-horizontal-whitespace port))
        ((eqv? char #\;)
           (consume-to-eol port)))))

  ;; reads a block and handles (quote), (unquote),
  ;; (unquote-splicing) and (quasiquote).
  (define (readblock-clean level port)
    (let* ((read (readblock level port))
           (next-level (car read))
           (block (cdr read)))
      (cond
        ; remove split-tag
        ((and (pair? block) (eq? (car block) split-tag))
          (cons next-level (cdr block)))
        ; non-list and multi-item blocks.
        ((or (not (list? block)) (> (length block) 1))
          (cons next-level block))
        ; unwrap single-item blocks
        ((= (length block) 1)
          ; TODO: study if this is indeed necessary
          (if (eq? (car block) split-tag)
              ; "magically" remove split-tag
              (cons next-level '())
              (cons next-level (car block))))
        (#t
          (cons next-level '.)))))

  ; TODO: merge the latter part of readblock-clean and
  ; readblock-clean-rotated, so that changes need to
  ; be done in only one place.

  ;; like readblock-clean, but with an initial object
  ;; already given
  (define (readblock-clean-rotated level port pos obj)
    (let* ((read (readblock-internal level port #f))
           (next-level (car read))
           (sub-block (cdr read))
           (block (cons obj sub-block)))
      ; unlike readblock-clean, we know that block
      ; is indeed a list, and its first item is
      ; *not* split-tag.  The question is the length
      ; of that list.
      (cond
        ((null? sub-block)
          (cons next-level (attach-sourceinfo pos obj)))
        (#t (cons next-level (attach-sourceinfo pos block))))))

  ; TODO: merge handling of ;-comments and #|...|# comments
  (define (sugar-start-expr port)
    ; Read single complete I-expression.
    (let* ((indentation (list->string (accumulate-hspace port)))
           (pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c) ; EOF - return it, we're done.
        ((eqv? c #\; )    ; comment - consume and see what's after it.
          (let ((d (consume-to-eol port)))
            (cond
              ((eof-object? d) d) ; If EOF after comment, return it.
              (#t
                (my-read-char port) ; Newline after comment.  Consume NL
                (sugar-start-expr port))))) ; and try again
        ; hashes are potential comments too
        ((eqv? c #\#)
          (let ((obj (neoteric-read-func port)))
            (if (eq? obj comment-tag)
                ; heh, comment.  Consume spaces and start again.
                ; (Consuming horizontal spaces makes comments behave
                ; as SPLIT when an item is after a comment on the
                ; same line)
                (begin
                  (accumulate-hspace port)
                  (sugar-start-expr port))
                ; aaaaargh not a comment.  Use rotated version
                ; of readblock-clean.
                (let* ((sub-read (readblock-clean-rotated "" port pos obj))
                       (block (cdr sub-read)))
                  (cond
                    ((eq? block '.)
                      (attach-sourceinfo pos '()))
                    (#t
                      (attach-sourceinfo pos block)))))))
        ((char-line-ending? c)
          (consume-end-of-line port)
          (sugar-start-expr port)) ; Consume and again
        ((> (string-length indentation) 0) ; initial indentation disables
          ; ignore indented comments
          (let ((rv (neoteric-read-func port)))
            (if (eq? rv comment-tag)
                ; indented comment.  restart.
                (sugar-start-expr port)
                rv)))
        (#t
          (let* ((read (readblock-clean "" port))
                 (level (car read))
                 (block (cdr read)))
            (cond
             ((eq? block '.)
                (attach-sourceinfo pos '()))
             (#t
                (attach-sourceinfo pos block))))))))

; -----------------------------------------------------------------------------
; Comparison Functions
; -----------------------------------------------------------------------------

  (define compare-read-file '()) ; TODO

; -----------------------------------------------------------------------------
; Exported Interface
; -----------------------------------------------------------------------------

  (define curly-infix-read (make-read curly-infix-read-nocomment-func))
  (define neoteric-read (make-read neoteric-read-nocomment-func))
  (define sweet-read (make-read sugar-start-expr))

  )

; TODO: Fix bug if there's no end-of-line at the last line of file.
;       Seems to be in the sweet-expression processing.

; vim: set expandtab shiftwidth=2 :

