;;; consult-notes.el --- Manage notes with consult -*- lexical-binding: t; coding: utf-8-emacs -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 0.7
;; Package-Requires: ((emacs "27.1") (consult "0.17") (s "1.12.0") (dash "2.19"))
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

;;; Code:
;;;; Requirements
(require 'consult)    ;; core dependency

;;;; Variables
;; obsolete vars
(define-obsolete-variable-alias 'consult-notes-sources
  'consult-notes-file-dir-sources "0.6")
(define-obsolete-variable-alias 'consult-notes--all-sources
  'consult-notes-all-sources "0.6")

(defgroup consult-notes nil
  "Search notes with consult."
  :group 'convenience)

(defcustom consult-notes-category 'consult-note
  "Category symbol for the notes in this package."
  :group 'consult-notes
  :type 'symbol)

(defcustom consult-notes-all-sources nil
  "Sources for `consult-notes'."
  :group 'consult-notes
  :type '(repeat symbol))

(defcustom consult-notes-file-dir-sources nil
  "Directories of files for searching with `consult-notes'.
Each source entry is a list.
There are three required elements: a title string, a narrowing key
(character), and a directory path (string) containing note files.
Optional keyword arguments may follow, such as :hidden t to hide
the source from the default list (still accessible via narrowing)."
  :group 'consult-notes
  :type '(repeat (choice
                  (list string character string)
                  (list string character string
                        (const :hidden) boolean))))

(defcustom consult-notes-file-dir-annotate-function #'consult-notes--file-dir-annotate
  "Function to call for annotations of file note directories in `consult-notes'.

The default function displays dir, file size, and modified time.
Please see the function `consult-notes--file-dir-annotate' for
details."
  :group 'consult-notes
  :type 'function)

(defcustom consult-notes-use-rg t
  "Whether to use ripgrep or just grep for text searches."
  :group 'consult-notes
  :type 'boolean)

(defcustom consult-notes-default-format '(org-mode)
  "Default format for `consult-notes' open function."
  :group 'consult-notes
  :type 'sexp)

(defcustom consult-notes-max-relative-age (* 60 60 24 14)
  "Maximum relative age in seconds displayed by the file annotator.

Set to `most-positive-fixnum' to always use a relative age, or 0 to never show
a relative age."
  :type 'integer)

(defcustom consult-notes-file-match "[^.].*[.].+[^~]$"'
  "Default format for `consult-notes' open function."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-file-action 'consult--file-action
  "Default action for `consult-notes' open function when no match is found."
  :group 'consult-notes
  :type 'function)

;; Placate the byte-compiler
(defvar org-roam-directory)
(defvar consult-notes-history nil
  "History variable for `consult-notes'.")

;;;; Faces
;; Define faces used in consult-notes

(defface consult-notes-name '((t (:inherit (warning) :weight light)))
  "Face for name data in `consult-notes'."
  :group 'faces)

(defface consult-notes-size '((t (:inherit (warning) :weight light)))
  "Face for size data in `consult-notes'."
  :group 'faces)

(defface consult-notes-time '((t (:inherit (warning) :weight light)))
  "Face for time data in `consult-notes'."
  :group 'faces)

(defface consult-notes-dir '((t (:inherit (warning) :weight light)))
  "Face for directory data in `consult-notes'."
  :group 'faces)

(defface consult-notes-backlinks '((t (:inherit (warning) :weight light)))
  "Face for backlinks data in `consult-notes'."
  :group 'faces)

(defface consult-notes-sep '((t (:inherit (consult-separator))))
  "Face for separator in `consult-notes'."
  :group 'faces)

;;;; Time/Date Functions
;; These are derived from Daniel Mendler's Marginalia package.
;; See https://github.com/minad/marginalia

(defconst consult-notes--time-relative
  `((100 "sec" 1)
    (,(* 60 100) "min" 60.0)
    (,(* 3600 30) "hour" 3600.0)
    (,(* 3600 24 400) "day" ,(* 3600.0 24.0))
    (nil "year" ,(* 365.25 24 3600)))
  "Formatting used by the function `consult-notes--time-relative'.")

;; Taken from `seconds-to-string'.
(defun consult-notes--time-relative (time)
  "Format TIME as a relative age."
  (setq time (max 0 (float-time (time-since time))))
  (let ((sts consult-notes--time-relative) here)
    (while (and (car (setq here (pop sts))) (<= (car here) time)))
    (setq time (round time (caddr here)))
    (format "%s %s%s ago" time (cadr here) (if (= time 1) "" "s"))))

(defun consult-notes--time-absolute (time)
  "Format TIME as an absolute age."
  (let ((system-time-locale "C"))
    (format-time-string
     (if (> (decoded-time-year (decode-time (current-time)))
            (decoded-time-year (decode-time time)))
         " %Y %b %d"
       "%b %d %H:%M")
     time)))

(defun consult-notes--time (time)
  "Format file age TIME, suitably for use in annotations."
  (if (< (float-time (time-since time)) consult-notes-max-relative-age)
      (consult-notes--time-relative time)
    (consult-notes--time-absolute time)))

;;;; Consult-Notes File-Directory Function
;;;###autoload
(defun consult-notes--file-dir-source (name char dir &rest args)
  "Generate the notes source for each directory of files in `consult-notes-dir-sources'.

 Return a notes source list suitable for `consult--multi'.
NAME is the source name, CHAR is the narrowing character,
and DIR is the directory to find notes."
  (let ((hidden (plist-get args :hidden)))
    `(:name     ,(propertize name 'face 'consult-notes-sep)
      :narrow   ,char
      :category ,consult-notes-category
      :face     consult-file
      :annotate ,(apply-partially consult-notes-file-dir-annotate-function name dir)
      :hidden   ,hidden
      :items    ,(lambda ()
                  (let* ((files (directory-files dir nil consult-notes-file-match)))
                    files))
      :state    ,(lambda ()
                  (let ((open (consult--temporary-files))
                        (state (consult--file-state)))
                    (lambda (action cand)
                      (unless cand
                        (funcall open))
                      (funcall state action (and cand (concat (file-name-as-directory dir) cand)))))))))

;;;; Consult-Notes File Dir Annotation Function
;;;###autoload
(defun consult-notes--file-dir-annotate (name dir cand)
  "Annotate file CAND with its directory DIR, size, and modification time."
  (let* ((file  (concat (file-name-as-directory dir) cand))
         (dirs  (abbreviate-file-name dir))
         (attrs (file-attributes file))
         (fsize (file-size-human-readable (file-attribute-size attrs)))
	     (ftime (consult-notes--time (file-attribute-modification-time attrs))))
    (put-text-property 0 (length name)  'face 'consult-notes-name name)
    (put-text-property 0 (length dirs)  'face 'consult-notes-name dirs)
    (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
    (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
    (format "%7s %8s  %12s  %8s" name fsize ftime dirs)))

;;;; Consult-Notes Make File-Dir Sources
;;;###autoload
(defun consult-notes--make-file-dir-sources ()
  "Add generated `consult--multi' sources to list of all sources."
  (let ((sources (mapcar (lambda (s) (apply #'consult-notes--file-dir-source s))
		                 consult-notes-file-dir-sources)))
    (dolist (i sources)
      (add-to-list 'consult-notes-all-sources i))))

;;;; Minor Modes

;;;;; Consult-Notes Denote
;; Define a minor-mode for consult-notes & denote
;;;###autoload
(define-minor-mode consult-notes-denote-mode
  "Toggle `consult-notes-denote-mode' to integrate consult with denote."
  :init-value nil
  :group 'consult-notes
  :global t
  (require 'consult-notes-denote)
  ;; Add or remove denote notes from sources
  (cond (consult-notes-denote-mode
         ;; Add denote notes source to consult--multi integration
         (add-to-list 'consult-notes-all-sources 'consult-notes-denote--source 'append)
         (setq consult-notes-file-action #'consult-notes-denote--new-note))
        (t
         ;; Remove denote notes from sources
         (delete 'consult-notes-denote--source consult-notes-all-sources)
         ;; Revert default new action
         (custom-reevaluate-setting 'consult-notes-file-action))))

;;;;; Consult-Notes Org-Roam
;; Define a minor-mode for consult-notes & org-roam
;;;###autoload
(define-minor-mode consult-notes-org-roam-mode
  "Toggle `consult-notes-org-roam-mode' to integrate org-roam with consult-notes.

When enabled, this mode adds org-roam nodes and refs as sources in
`consult-notes', allowing you to search and select org-roam content
alongside other note sources.

The display format respects your `org-roam-node-display-template'
customization and any custom node accessor methods you've defined with
`cl-defmethod'. Annotations are controlled by
`consult-notes-org-roam-annotate-function'.

Optional argument ARG indicates whether the mode should be enabled or disabled."
  :init-value nil
  :group 'consult-notes
  :global t
  (require 'consult-notes-org-roam)
  ;; Add or remove sources when enabled or disabled
  ;; Note: We no longer override org-roam-node-display-template as of v0.8
  ;; Users should customize org-roam-node-display-template directly
  (cond (consult-notes-org-roam-mode
         ;; Add org-roam consult--multi integration
         (add-to-list 'consult-notes-all-sources 'consult-notes-org-roam--nodes 'append)
         (add-to-list 'consult-notes-all-sources 'consult-notes-org-roam--refs 'append))
        (t
         ;; Remove org-roam sources
         (delete 'consult-notes-org-roam--nodes consult-notes-all-sources)
         (delete 'consult-notes-org-roam--refs  consult-notes-all-sources))))

;;;;; Consult-Notes Org-Headings
;; Define a minor-mode for consult-notes & org headings in specified files
;;;###autoload
(define-minor-mode consult-notes-org-headings-mode
  "Toggle `consult-notes-org-headings-mode'."
  :init-value nil
  :lighter nil
  :group 'consult-notes
  :global t
  (require 'consult-notes-org-headings)
  ;; Add or remove org-headings from sources
  (cond (consult-notes-org-headings-mode
         ;; Add org-headings source to consult--multi integration
         (add-to-list 'consult-notes-all-sources 'consult-notes-org-headings--source 'append))
        (t
         ;; Remove org-headings from sources
         (delete 'consult-notes-org-headings--source consult-notes-all-sources)
         ;; Revert default new action
         (custom-reevaluate-setting 'consult-notes-file-action))))

;;;; Consult-Notes Search (Grep/Ripgrep)

;;;###autoload
(defun consult-notes-search-in-all-notes ()
  "Search in all notes using `grep' or `ripgrep'.
Which search function is used depends on the value of `consult-notes-use-rg'."
  (interactive)
  (let ((sources (delete-dups
                  (flatten-list
                   (append
                    ;; dir sources
                    (mapcar #'cddr consult-notes-file-dir-sources)
                    ;; org roam
                    (when (bound-and-true-p consult-notes-org-roam-mode)
                      (list (expand-file-name org-roam-directory)))
                    ;; denote
                    (when (bound-and-true-p consult-notes-denote-mode)
                      (list (expand-file-name denote-directory)))
                    ;; org agenda files
                    (when (bound-and-true-p consult-notes-org-headings-mode)
                      (mapcar #'expand-file-name consult-notes-org-headings-files)))))))
    (if consult-notes-use-rg
        (consult-ripgrep sources)
      (consult-grep sources))))

;;;; Consult-Notes Consult--Multi

;;;###autoload
(defun consult-notes (&optional sources)
  "Find a file in a notes directory with consult-multi, or from SOURCES."
  (interactive)
  (consult-notes--make-file-dir-sources)
  (consult--multi (or sources consult-notes-all-sources)
                  :require-match
                  (confirm-nonexistent-file-or-buffer)
                  :prompt "Notes: "
                  :history 'consult-notes-history
                  :add-history (seq-some #'thing-at-point '(region symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Provide Consult Notes
(provide 'consult-notes)

;; Local Variables:
;; coding: utf-8-emacs
;; End:
;;; consult-notes.el ends here
