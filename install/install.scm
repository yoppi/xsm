#!/usr/bin/env gosh

;;; This file can be distributed under the same license of Gauche.
;;;   Copyright (c) 2004-2006 Kouhei Sutou <kou@cozmixng.org>

(use file.util)
(use gauche.config)
(use srfi-1)
(use srfi-37)

(define *ignore-directories* (map x->string '(.svn CVS RCS)))
(define *default-destdir* (gauche-site-library-directory))
(define *default-datadir* (rxmatch-before 
                           (#/\/?gauche\/?/ (gauche-site-library-directory))))

(define *shared-library-re* (string->regexp
                             #`"lib.*\.,(gauche-config \"--dylib-suffix\")$"))

(define (main args)
  (define (usage)
    (print #`"\t-b, --base=BASE\t\tBASE for install. (default \"\")")
    (print #`"\t-d, --destdir=DIR\tInstall library files in DIR.")
    (print #`"\t\t\t\t(default \",|*default-destdir*|\")")
    (print #`"\t-d, --datadir=DIR\tInstall data files in DIR.")
    (print #`"\t\t\t\t(default \",|*default-datadir*|\")")
    (print "\t-t, --test\t\tOnly show how to install. Don't install.")
    (print "\t-h, --help\t\tDisplay this help."))
  (define (bad-option message)
    (print message)
    (usage)
    (exit -1))
  (define options
    (list (option '(#\b "base") #t #t
                  (lambda (option name arg base dest-dir data-dir test? . others)
                    (unless arg
                      (bad-option #`"BASE is required for option ,|name|"))
                    (values arg dest-dir data-dir test?)))
          (option '(#\d "destdir") #t #t
                  (lambda (option name arg base dest-dir data-dir test? . others)
                    (unless arg
                      (bad-option #`"DIR is required for option ,|name|"))
                    (values base arg data-dir test?)))
          (option '(#\d "datadir") #t #t
                  (lambda (option name arg base dest-dir data-dir test? . others)
                    (unless arg
                      (bad-option #`"DIR is required for option ,|name|"))
                    (values base dest-dir arg test?)))
          (option '(#\t "test") #f #f
                  (lambda (option name arg base dest-dir data-dir test? . others)
                    (values base dest-dir data-dir #t)))
          (option '(#\h "help") #f #f
                  (lambda (option name arg . others)
                    (usage)
                    (exit 0)))))
  (receive (base dest-dir data-dir test?)
      (args-fold (cdr args)
                 options
                 (lambda (option name arg . seeds) ; unrecognized
                   (bad-option #`"Unrecognized option: ,|name|"))
                 (lambda (operand base dest-dir data-dir test?) ; operand
                   (values base dest-dir data-dir test?))
                 ""
                 *default-destdir*
                 *default-datadir*
                 #f)
    (install-directory "lib" (string-append base dest-dir) test?)
    (install-directory "data" (string-append base data-dir) test?))
  0)

(define (install-directory from to test?)
  (if (file-is-directory? from)
    (directory-fold from
                    (lambda (file knil)
                      (let ((target (sys-dirname
                                     (string-scan file from 'after))))
                        (install-file file
                                      (string-append to target)
                                      test?)))
                    #t
                    :lister
                    (lambda (dir knil)
                      (let ((target (string-scan dir from 'after)))
                        (if (member (sys-basename target)
                                    *ignore-directories*
                                    string=?)
                          '()
                          (begin
                            (make-installed-directory
                             (build-path to (rxmatch-after (#/^\/*/ target)))
                             test?)
                            (directory-list dir
                                            :children? #t
                                            :add-path? #t))))))))

(define (shared-library? filename)
  (and (rxmatch *shared-library-re* filename)
       #t))

(define (install-file file dir test?)
  (let ((target (build-path dir (sys-basename file))))
    (print #`"installing ,|file| => ,|target|")
    (unless test?
      (copy-file file
                 target
                 :if-exists :supersede
                 :safe #t)
      (sys-chmod target #o644))))

(define (make-installed-directory dir test?)
  (print #`"making installed directory ,|dir|")
  (unless test?
    (make-directory* dir)
    (sys-chmod dir #o755)))
