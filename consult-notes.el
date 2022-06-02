;;; consult-notes.el --- Manage notes with consult -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.17"))
;; Keywords: convenience
;; Homepage: https://github.com/mclear-tools/consult-notes

;; Copyright (C) 2022 Colin McLear

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Manage your notes with consult.

;;; Code
;;;; Requirements
(require 'consult)    ;; core dependency
(require 'marginalia) ;; for faces
(require 'embark)     ;; for actions
(require 'dired-x)

;;;; Variables
(defgroup consult-notes nil
  "Search notes with consult."
  :group 'convenience)

(defcustom consult-notes-category 'consult-note
  "Category symbol for the notes in this package."
  :group 'consult-notes)

(defcustom consult-notes-history nil
  "History variable for `consult-notes'."
  :group 'consult-notes)

(defcustom consult-notes-sources-data
  '(("Org" ?o org-agenda-files))
  "Sources for `consult-notes' file search.
There are three elements in the list. The first is a title string. The second is a narrowing key, and the third is a path (string)."
  :group 'consult-notes
  :type '(list string key string))

(defcustom consult-notes-all-notes ""
  "Dir for grep of all notes."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-ripgrep-args  "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --ignore-case --no-heading --line-number --hidden --glob=!.git/ -L --sortr=accessed ."
  "Arguments for `ripgrep' and `consult-notes-search-all'."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-grep-args "grep --null --line-buffered --color=never --ignore-case   --exclude-dir=.git --line-number -I -R -S ."
  "Arguments for `grep' and `consult-notes-search-all'."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-default-format '(org-mode)
  "Default format for consult-notes open function."
  :group 'consult-notes
  :type 'sexp)

;;;; General Notes Functions

(defun consult-notes-make-source (name char dir)
  "Return a notes source list suitable for `consult--multi'.
NAME is the source name, CHAR is the narrowing character,
and DIR is the directory to find notes. "
  (let ((idir (propertize (file-name-as-directory dir) 'invisible t)))
    `(:name     ,name
      :narrow   ,char
      :category ,consult-notes-category
      :face     consult-file
      :annotate ,(apply-partially 'consult-annotate-note name)
      :items    ,(lambda () (mapcar (lambda (f) (concat idir f))
				               ;; filter files that glob *.*
				               (directory-files dir nil "[^.].*[.].+")))
      :action   ,(lambda (f) (find-file f) consult-notes-default-format))))

(defun consult-annotate-note (name cand)
  "Annotate file CAND with its source name, size, and modification time."
  (let* ((attrs (file-attributes cand))
	     (fsize (file-size-human-readable (file-attribute-size attrs)))
	     (ftime (format-time-string "%b %d %H:%M" (file-attribute-modification-time attrs))))
    (put-text-property 0 (length name) 'face 'marginalia-type name)
    (put-text-property 0 (length fsize) 'face 'marginalia-size fsize)
    (put-text-property 0 (length ftime) 'face 'marginalia-date ftime)
    (format "%15s  %7s  %10s" name fsize ftime)))

;;;###autoload
(defun consult-notes ()
  "Find a file in a notes directory with consult-multi."
  (interactive)
  (consult--multi (mapcar #'(lambda (s) (apply 'consult-notes-make-source s))
			              consult-notes-sources-data)
		          :prompt "Notes: "
		          :history 'consult-notes-history))

;;;###autoload
(defun consult-notes-search-all ()
  "Search all notes using ripgrep.
If ripgrep is not installed fall back to consult-grep."
  (interactive)
  (let ((consult-ripgrep-args consult-notes-ripgrep-args)
        (consult-grep-args consult-notes-grep-args))
    (if (executable-find "rg")
        (consult-ripgrep consult-notes-all-notes)
      (consult-grep consult-notes-all-notes))))


;;;; Embark support
(defun consult-notes-open-dired (cand)
  "Open notes directory dired with point on file CAND."
  (interactive "fNote: ")
  ;; dired-jump is in dired-x.el but is moved to dired in Emacs 28
  (dired-jump nil cand))

(defun consult-notes-marked (cand)
  "Open a notes file CAND in Marked 2.
Marked 2 is a mac app that renders markdown."
  (interactive "fNote: ")
  (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" (expand-file-name cand))))

(defun consult-notes-grep (cand)
  "Run grep in directory of notes file CAND."
  (interactive "fNote: ")
  (consult-grep (file-name-directory cand)))

(embark-define-keymap consult-notes-map
                      "Keymap for Embark notes actions."
                      :parent embark-file-map
                      ("d" consult-notes-dired)
                      ("g" consult-notes-grep)
                      ;; ("h" consult-notes-org-headline)
                      ("m" consult-notes-marked))

(add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))
;; make embark-export use dired for notes
(setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Provide Consult Notes
(provide 'consult-notes)
;;; consult-notes.el ends here
