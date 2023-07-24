(define-module (guix scripts import west)
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix scripts hash)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-26) ; cut
  #:use-module (ice-9 match)
  #:use-module (yaml)
  #:export (guix-import-west))

(define-record-type* <west-manifest>
  west-manifest make-west-manifest
  west-manifest?
  this-manifest
  (defaults west-manifest-defaults)
  (remotes west-manifest-remotes)
  (projects west-manifest-projects))

(define (read-west.yml path)
  (match (car (read-yaml-file path))
    ((_ . props)
     (west-manifest
      (defaults (assoc-ref props "defaults"))
      (remotes (vector->list
		(assoc-ref props "remotes")))
      (projects (vector->list
		 (assoc-ref props "projects")))))
    (_ (error "invalid west.yml"))))

(define (download/hash url revision)
  (let ((dir (mkdtemp "/tmp/west-download-XXXXXX"))
	(current-dir (getcwd))
	(hash #f))
    (invoke "git" "clone" url dir)
    (chdir dir)
    (invoke "git" "checkout" revision)
    (set! hash
	  (substring
	   (with-output-to-string (lambda () (guix-hash "-rx" ".")))
	   0 52))
    (chdir current-dir)
    (invoke "rm" "-rf" dir)
    hash))


(define* (canonicalize-projects manifest zephyr-version #:optional check-hashes)
  "Return a list of packages for each project in manifest.
When check-hashes is #t, download and calculate source hash.
This can take a while.

Unfortunately the west.yml does not include descriptions or license information
so you will have to fill this in manually."

  (define defaults (west-manifest-defaults manifest))

  (define remotes
    (map (lambda (x)
	   (cons (assoc-ref x "name")
		 (assoc-ref x "url-base")))
	 (west-manifest-remotes manifest)))

  (define (canonicalize-name name)
    (string-append "zephyr-"
		   (string-join (string-split name #\_) "-")))

  (define (make-package proj)
    (let* ((name (assoc-ref proj "name"))
	   (commit (assoc-ref proj "revision"))
	   (remote (or (assoc-ref proj "remote")
		       (assoc-ref defaults "remote")))
	   (url (string-append (or (assoc-ref proj "url")
				   (and remote
					(assoc-ref remotes remote))
				   (error (format #f "No url/remote for ~a found" name)))
			       "/" name)))
      `(define-public ,(string->symbol (canonicalize-name name))
	 (let ((commit ,commit))
	   (package
	    (name ,(canonicalize-name name))
	    (version (git-version "0.0" ,zephyr-version commit))
	    (home-page ,url)
	    (source
	     (origin (method git-fetch)
		     (uri (git-reference
			   (url ,url)
			   (commit commit)))
		     (file-name (git-file-name name (substring commit 0 10)))
		     (sha256
		      (base32 ,(if check-hashes
				   (download/hash url commit)
				   "missinghashmissinghashmissinghashmissinghashmissingh")))))
	    (build-system zephyr-build-system)
	    (arguments '(#:workspace-path ,(string-append "/" (assoc-ref proj "path"))))
	    (synopsis "")
	    (description "")
	    (license #f))))))

  (map make-package (west-manifest-projects manifest)))

(define* (guix-import-west path zephyr-version #:optional check-hashes)
  "Return a list of guix packages imported from a west.yml manifest.
If check-hashes is true, the sources will be downloaded and hashes calculated."
  (canonicalize-projects (read-west.yml path) zephyr-version check-hashes))
