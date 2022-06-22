;;; consult-notes.el --- Manage notes with consult -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (consult "0.17") (marginalia) (embark))
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
(defgroup consult-notes nil
  "Search notes with consult."
  :group 'convenience)

(defcustom consult-notes-category 'consult-note
  "Category symbol for the notes in this package."
  :group 'consult-notes
  :type 'symbol)

(defcustom consult-notes-history nil
  "History variable for `consult-notes'."
  :group 'consult-notes
  :type 'symbol)

(defcustom consult-notes-sources
  '(("Notes" ?n "~/Notes"))
  "Sources for `consult-notes' file search.

There are three elements in the list. The first is a title
string. The second is a narrowing key, and the third is a
directory path (string) containing note files."
  :group 'consult-notes
  :type '(list string key string))

(defcustom consult-notes-annotate-note-function #'consult-notes-annotate-note
  "Function to call for annotations in `consult-notes'.

The default function displays dir, file size, and modified time.
Please see the function `consult-notes-annotate-note' for
details."
  :group 'consult-notes
  :type 'function)

(defcustom consult-notes-use-rg t
  "Whether to use ripgrep or just grep for text searches."
  :group 'consult-notes
  :type 'boolean)

(defcustom consult-notes-ripgrep-args  "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --ignore-case --no-heading --line-number --hidden --glob=!.git/ -L --sortr=accessed ."
  "Arguments for `ripgrep' and `consult-notes-search-all'."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-grep-args "grep --null --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I -R ."
  "Arguments for `grep' and `consult-notes-search-all'."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-default-format '(org-mode)
  "Default format for `consult-notes' open function."
  :group 'consult-notes
  :type 'sexp)

(defcustom consult-notes-max-relative-age (* 60 60 24 14)
  "Maximum relative age in seconds displayed by the file annotator.

Set to `most-positive-fixnum' to always use a relative age, or 0 to never show
a relative age."
  :type 'integer)

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
  "Face for directory data in `consult-notes'."
  :group 'faces)

(defface consult-notes-sep '((t (:inherit (bold))))
  "Face for directory data in `consult-notes'."
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

;;;; General Notes Functions

(defun consult-notes--make-source (name char dir)
  "Return a notes source list suitable for `consult--multi'.
NAME is the source name, CHAR is the narrowing character,
and DIR is the directory to find notes."
  (let ((idir (propertize (file-name-as-directory dir) 'invisible t)))
    `(:name     ,(propertize name 'face 'consult-notes-sep)
      :narrow   ,char
      :category ,consult-notes-category
      :face     consult-file
      :annotate ,(apply-partially consult-notes-annotate-note-function name)
      :items    ,(lambda () (mapcar (lambda (f) (concat idir f))
				               ;; filter files that glob *.*
				               (directory-files dir nil "[^.].*[.].+")))
      :action   ,(lambda (f) (find-file f) consult-notes-default-format))))

(defun consult-notes-annotate-note (name cand)
  "Annotate file CAND with its source NAME, size, and modification time."
  (let* ((attrs (file-attributes cand))
	     (fsize (file-size-human-readable (file-attribute-size attrs)))
	     (ftime (consult-notes--time (file-attribute-modification-time attrs))))
    (put-text-property 0 (length name)  'face 'consult-notes-name name)
    (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
    (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
    (format "%s  %5s  %5s" name fsize ftime)))

(defvar consult-notes--all-sources nil
  "List of all sources for use with consult-notes.
This is an internal variable. The user will typically only interact with `consult-notes-sources'.")

(defun consult-notes--make-all-sources ()
  "Add generated `consult--multi' sources to list of all sources."
  (let ((sources (mapcar #'(lambda (s) (apply 'consult-notes--make-source s))
		                 consult-notes-sources)))
    (dolist (i sources)
      (add-to-list 'consult-notes--all-sources i))))

;;;###autoload
(defun consult-notes (&optional sources)
  "Find a file in a notes directory with consult-multi, or from SOURCES."
  (interactive)
  (consult-notes--make-all-sources)
  (let ((selected (consult--multi (or sources consult-notes--all-sources)
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt "Notes: "
                                  :history 'consult-notes-history)))
    ;; For non-matching candidates, fall back to buffer-file creation.
    (unless (plist-get (cdr selected) :match)
      (consult--file-action (car selected)))))

;;;###autoload
(defun consult-notes-search-all ()
  "Search all notes using ripgrep, if that is set.
If ripgrep is not installed fall back to `consult-grep'."
  (interactive)
  (let ((consult-ripgrep-args consult-notes-ripgrep-args)
        (consult-grep-args consult-notes-grep-args))
    (if consult-notes-use-rg
        (consult-ripgrep consult-notes-all-notes)
      (consult-grep consult-notes-all-notes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Provide Consult Notes
(provide 'consult-notes)
;;; consult-notes.el ends here
