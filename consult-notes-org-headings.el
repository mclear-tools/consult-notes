;;; consult-notes-org-headings.el --- find org heading notes using consult -*- lexical-binding: t; coding: utf-8-emacs -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 0.7
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

;; Manage notes by search org file headings as part of `consult-notes'.

;;; Code:
(require 'consult-notes)
(require 'org)

;;;; Variables
(defvar consult-notes-org-headings--history nil)

(defcustom consult-notes-org-headings-files org-agenda-files
  "Source for `consult-notes-org-headings'.

Default value is the value of variable `org-agenda-files'."
  :group 'consult-notes
  :type '(repeat file))

(defcustom consult-org-headings-narrow-key ?h
  "Key for narrowing using `consult-notes' function."
  :group 'consult-notes
  :type 'key)

;;;; Functions
;; See https://emacs.stackexchange.com/a/28689/11934
(defun consult-notes--string-matches (search strings)
  "Match (partial) string SEARCH to member of list STRINGS."
  (while (and strings (not (string-match search (car strings))))
    (setq strings (cdr strings)))
  strings)

;; This is adapted from `(org-agenda-files)'.
(defun consult-notes-org-headings-files ()
  "Get the list of org-headings files."
  (let ((files
	     (cond
	      ((stringp consult-notes-org-headings-files) (org-read-agenda-file-list))
	      ((listp consult-notes-org-headings-files) consult-notes-org-headings-files)
	      (t (error "Invalid value of `consult-notes-org-headings-files'")))))
    (setq files (apply 'append
		               (mapcar (lambda (f)
				                 (if (file-directory-p f)
				                     (directory-files
				                      f t org-agenda-file-regexp)
				                   (list (expand-file-name f org-directory))))
			                   files)))
    (when org-agenda-skip-unavailable-files
      (setq files (delq nil
			            (mapcar (lambda (file)
				                  (and (file-readable-p file) file))
				                files))))
    files))

(defun consult-notes--org-headings (match scope &rest skip)
  "Return a list of Org heading candidates.

MATCH, SCOPE and SKIP are as in `org-map-entries'."
  (let (buffer)
    (apply
     #'org-map-entries
     (lambda ()
       (unless (eq buffer (buffer-name))
         (setq buffer (buffer-name)
               org-outline-path-cache nil))
       (pcase-let ((`(_ ,level ,todo ,prio ,_hl ,tags) (org-heading-components))
                   (cand (org-format-outline-path
                          (org-get-outline-path 'with-self 'use-cache))))
         (when tags
           (setq tags (concat " " (propertize tags 'face `(:height 0.8 :inherit org-tag)))))

         (setq cand (concat (propertize cand 'face 'consult-file)
                            tags (consult--tofu-encode (point))
                            (propertize " " 'display `(space :align-to center))

                            (format "%18s" (propertize (concat "@" buffer) 'face 'consult-notes-sep))))
         (add-text-properties 0 1
                              `(org-marker ,(point-marker)
                                           consult-org--heading (,level ,todo . ,prio))
                              cand)
         cand))
     match scope skip)))

(defun consult-notes-org-headings--mrkr (cand &optional find-file)
  "Return the marker for CAND.
FIND-FILE is the file open function, defaulting to `find-file'."
  (when cand
    (let* ((mrkr (and cand (get-text-property 0 'org-marker cand))))
      mrkr)))

(defun consult-notes-org-headings--state ()
  "Org headings state function."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall jump action (consult-notes-org-headings--mrkr
                            cand
                            (and (not (eq action 'return)) open))))))

;;;; Annotations
(defun consult-notes-org-headings-annotations (cand)
  "Annotate file CAND with its file attributes, size, and modification time."
  (let* ((name (buffer-name
                (marker-buffer
                 (get-text-property 0 'org-marker cand))))
         (path (car
                (consult-notes--string-matches name consult-notes-org-headings-files)))
         (attrs (file-attributes path))
         (ftime (consult-notes--time (file-attribute-modification-time attrs)))
         (fsize (file-size-human-readable (or (file-attribute-size attrs) 0 ))))
    (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
    (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
    (format "%8s  %8s" fsize ftime)))

;;;; Source
(defconst consult-notes-org-headings--source
  (list :name (propertize "Org Headings" 'face 'consult-notes-sep)
        :narrow consult-org-headings-narrow-key
        :category 'org-heading
        :require-match t
        :items (lambda ()
                 (consult-notes--org-headings t (consult-notes-org-headings-files)))
        :state #'consult-notes-org-headings--state
        :annotate #'consult-notes-org-headings-annotations
        :history 'consult-notes-org-headings--history
        :lookup (lambda (selected &rest _) (get-text-property 0 'org-marker selected)))
  "Source for the `consult-notes' function.")

(provide 'consult-notes-org-headings)

;; Local Variables:
;; coding: utf-8-emacs
;; End:
;;; consult-notes-org-headings.el ends here
