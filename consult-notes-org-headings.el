;;; consult-notes-org-headings.el --- find org heading notes using consult -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 0.5
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
(defcustom consult-notes-org-headings-files org-agenda-files
  "Source for `consult-notes-org-headings'."
  :group 'consult-notes
  :type '(repeat file))

;;;; Functions
;; See https://emacs.stackexchange.com/a/28689/11934
(defun consult-notes--string-matches (search strings)
  "Match (partial) string SEARCH to member of list STRINGS."
  (while (and strings (not (string-match search (car strings))))
    (setq strings (cdr strings)))
  strings)

;; This is adapted from `(org-agenda-files)'.
(defun consult-notes-org-headings-files (&optional unrestricted)
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
  (apply
   #'org-map-entries
   (lambda ()
     ;; Reset the cache when the buffer changes, since `org-get-outline-path' uses the cache
     (setq org-outline-path-cache nil)
     (pcase-let ((`(_ ,level ,todo ,prio ,_hl ,tags) (org-heading-components))
                 (cand (org-format-outline-path
                        (org-get-outline-path 'with-self 'use-cache))))
       (when tags
         (setq tags (concat " " (propertize tags 'face `(:height 0.8 :inherit org-tag)))))
       (setq cand (concat (propertize cand 'face 'consult-file)
                          tags (consult--tofu-encode (point))))
       (add-text-properties 0 1
                            `(consult--candidate ,(point-marker)
                                                 consult-org--heading (,level ,todo . ,prio))
                            cand)
       cand))
   match scope skip))

(defun consult-notes--org-headings-state()
  "Preview org headline in relevant file."
  (let ((state (consult--jump-state)))
    (lambda (action cand)
      (when cand
        (let* ((pos (get-text-property 0 'consult--candidate cand))
               (buf (marker-buffer pos))
               (name (substring-no-properties (buffer-name buf)))
               (path (car
                      (consult-notes--string-matches name consult-notes-org-headings-files)))
               (file (consult--file-action path)))
          (funcall state action file))))))

;;;; Annotations
(defun consult-notes-org-headings-annotations (cand)
  "Annotate file CAND with its file attributes, size, and modification time."
  (let* ((name (buffer-name
                (marker-buffer
                 (get-text-property 0 'consult--candidate cand))))
         (path (car
                (consult-notes--string-matches name consult-notes-org-headings-files)))
         (attrs (file-attributes path))
         (ftime (consult-notes--time (file-attribute-modification-time attrs)))
         (fsize (file-size-human-readable (file-attribute-size attrs))))
    (put-text-property 0 (length name)  'face 'consult-notes-size name)
    (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
    (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
    (format "%18s %8s  %8s" name fsize ftime)))

;;;; Source
(defconst consult-notes-org-headings--source
  (list :name (propertize "Org Headings" 'face 'consult-notes-sep)
        :narrow ?a
        :sort nil
        :category 'consult-notes
        :items (funcall #'consult-notes--org-headings t (consult-notes-org-headings-files) nil)
        :state #'consult-notes--org-headings-state
        :annotate #'consult-notes-org-headings-annotations)
  "Source for consult-notes multi.")



(provide 'consult-notes-org-headings)
;;; consult-notes-org-headings.el ends here
