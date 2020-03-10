;;; pubmed-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pubmed" "pubmed.el" (0 0 0 0))
;;; Generated autoloads from pubmed.el

(autoload 'pubmed-mode "pubmed" "\
Major mode for PubMed.

\(fn)" t nil)

(autoload 'pubmed-show-mode "pubmed" "\
Mode for displaying PubMed entries.

\(fn)" t nil)

(autoload 'pubmed-search "pubmed" "\
Search PubMed with QUERY.

\(fn QUERY)" t nil)

(autoload 'pubmed-complete "pubmed" "\
Perform completion using PubMed suggestions preceding point.

\(fn &optional PREDICATE)" t nil)

(autoload 'pubmed-author-complete "pubmed" "\
Perform completion using PubMed suggestions preceding point.

\(fn &optional PREDICATE)" t nil)

(autoload 'pubmed-journal-complete "pubmed" "\
Perform completion using PubMed suggestions preceding point.

\(fn &optional PREDICATE)" t nil)

(autoload 'pubmed-completion-at-point "pubmed" "\
Function used for `completion-at-point-functions'.

\(fn)" t nil)

(autoload 'pubmed-author-completion-at-point "pubmed" "\
Function used for `completion-at-point-functions'.

\(fn)" t nil)

(autoload 'pubmed-journal-completion-at-point "pubmed" "\
Function used for `completion-at-point-functions'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pubmed" '("pubmed-")))

;;;***

;;;### (autoloads nil "pubmed-advanced-search" "pubmed-advanced-search.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pubmed-advanced-search.el

(autoload 'pubmed-advanced-search-history-mode "pubmed-advanced-search" "\
Major mode for PubMed History.

\(fn)" t nil)

(autoload 'pubmed-advanced-search "pubmed-advanced-search" "\
Show PubMed Advanced Search Builder.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pubmed-advanced-search" '("pubmed-advanced-search-")))

;;;***

;;;### (autoloads nil "pubmed-bibtex" "pubmed-bibtex.el" (0 0 0 0))
;;; Generated autoloads from pubmed-bibtex.el

(autoload 'pubmed-bibtex-show "pubmed-bibtex" "\
In PubMed, show the BibTeX references of the marked entries or current entry. If optional argument ENTRIES is a list of UIDs, show the BibTeX references of the entries.

\(fn &optional ENTRIES)" t nil)

(autoload 'pubmed-bibtex-write "pubmed-bibtex" "\
In PubMed, write the BibTeX references of the marked entries or current entry to file FILE.
If optional argument ENTRIES is a list of UIDs, write the BibTeX
references of the entries. If FILE is not empty, the references
are appended to the end of the file.

\(fn &optional FILE ENTRIES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pubmed-bibtex" '("pubmed-bibtex-")))

;;;***

;;;### (autoloads nil "pubmed-openaccessbutton" "pubmed-openaccessbutton.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pubmed-openaccessbutton.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pubmed-openaccessbutton" '("pubmed-")))

;;;***

;;;### (autoloads nil "pubmed-pmc" "pubmed-pmc.el" (0 0 0 0))
;;; Generated autoloads from pubmed-pmc.el

(autoload 'pubmed-get-pmc "pubmed-pmc" "\
In PubMed, try to fetch the fulltext PDF of the marked entries, the current entry or the optional argument ENTRIES.

\(fn &optional ENTRIES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pubmed-pmc" '("pubmed-")))

;;;***

;;;### (autoloads nil "pubmed-scihub" "pubmed-scihub.el" (0 0 0 0))
;;; Generated autoloads from pubmed-scihub.el

(autoload 'pubmed-get-scihub "pubmed-scihub" "\
In PubMed, try to fetch the fulltext PDF of the marked entries, the current entry or the optional argument ENTRIES.

\(fn &optional ENTRIES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pubmed-scihub" '("pubmed-")))

;;;***

;;;### (autoloads nil "pubmed-unpaywall" "pubmed-unpaywall.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pubmed-unpaywall.el

(autoload 'pubmed-get-unpaywall "pubmed-unpaywall" "\
In PubMed, try to fetch the fulltext PDF of the marked entries, the current entry or the optional argument ENTRIES.

\(fn &optional ENTRIES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pubmed-unpaywall" '("pubmed-")))

;;;***

;;;### (autoloads nil nil ("pubmed-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pubmed-autoloads.el ends here
