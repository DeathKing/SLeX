;;; download-papers.scm
;;;
;;;   This script is used to download the paper cited in the README.md, some
;;; wget binary or alternatives assumed to be already installed.

(load-option 'synchronous-subprocess)

(load-relative "../slex.scm")

(define wget-or-alter "wget")
(define entry-re "\*\*\[(?<label>[^\]]*)\]\*\*\[\[PDF\]\((?<url>[^\)]*)\)\]")

(let ((re (RE/compile-from-string entry-re))
      (file (open-port "README.md")))
  (forall entry in (RE/scan-port re file)
    (run-shell-commond
      (format #f
              "~A ~A ~A.pdf~%"
              wget-or-alter
              (get-named-match-result entry 'url)
              (get-named-match-result entry 'label)))))

