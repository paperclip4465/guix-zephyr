;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2018 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2023 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guest utils)
  #:use-module (ice-9 match)
  #:export (push!
	    arguments-from-environment-variable
	    file-sans-extension
	    fold2
	    string-distance
	    string-closest
	    with-directory-excursion))

(define-syntax-rule (push! elt lst)
  (set! lst (cons elt lst)))

(define (file-sans-extension file)
  "Return the substring of FILE without its extension, if any."
  (let ((dot (string-rindex file #\.)))
    (if dot
	(substring file 0 dot)
	file)))

(define fold2
  (case-lambda
    ((proc seed1 seed2 lst)
     "Like `fold', but with a single list and two seeds."
     (let loop ((result1 seed1)
		(result2 seed2)
		(lst     lst))
       (if (null? lst)
	   (values result1 result2)
	   (call-with-values
	       (lambda () (proc (car lst) result1 result2))
	     (lambda (result1 result2)
	       (loop result1 result2 (cdr lst)))))))
    ((proc seed1 seed2 lst1 lst2)
     "Like `fold', but with two lists and two seeds."
     (let loop ((result1 seed1)
		(result2 seed2)
		(lst1    lst1)
		(lst2    lst2))
       (if (or (null? lst1) (null? lst2))
	   (values result1 result2)
	   (call-with-values
	       (lambda () (proc (car lst1) (car lst2) result1 result2))
	     (lambda (result1 result2)
	       (loop result1 result2 (cdr lst1) (cdr lst2)))))))))

(define (string-distance s1 s2)
  "Compute the Levenshtein distance between two strings."
  ;; Naive implemenation
  (define loop
    (lambda (as bt)
      (match as
	(() (length bt))
	((a s ...)
	 (match bt
	   (() (length as))
	   ((b t ...)
	    (if (char=? a b)
		(loop s t)
		(1+ (min
		     (loop as t)
		     (loop s bt)
		     (loop s t))))))))))

  (let ((c1 (string->list s1))
	(c2 (string->list s2)))
    (loop c1 c2)))

(define* (string-closest trial tests #:key (threshold 3))
  "Return the string from TESTS that is the closest from the TRIAL,
according to 'string-distance'.  If the TESTS are too far from TRIAL,
according to THRESHOLD, then #f is returned."
  (identity                              ;discard second return value
    (fold2 (lambda (test closest minimal)
	     (let ((dist (string-distance trial test)))
	       (if (and  (< dist minimal) (< dist threshold))
		   (values test dist)
		   (values closest minimal))))
	   #f +inf.0
	   tests)))

(define-syntax-rule (with-directory-excursion dir body ...)
  "Run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
   (dynamic-wind
     (lambda ()
       (chdir dir))
     (lambda ()
       body ...)
     (lambda ()
       (chdir init)))))

(define (arguments-from-environment-variable variable)
  "Retrieve value of environment variable denoted by string VARIABLE in the
form of a list of strings (`char-set:graphic' tokens) suitable for consumption
by `args-fold', if VARIABLE is defined, otherwise return an empty list."
  (let ((env (getenv variable)))
    (if env
	(string-tokenize env char-set:graphic)
	'())))
