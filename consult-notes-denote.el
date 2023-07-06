;;; consult-notes-denote.el --- find or create denote notes using consult -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 0.7
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

;; Manage your denote notes with consult. Please note that user must have
;; installed `denote' for `consult-notes-denote' to function.

;;; Code:

(require 'consult-notes)
(unless (require 'denote nil 'noerror)
  (message "Denote not found! Please check if it is installed."))

;;;; Variables

(defcustom consult-notes-denote-display-id t
  "Whether ID is displayed in annotations for `consult-notes-denote'."
  :group 'consult-notes
  :type 'boolean)

(defcustom consult-notes-denote-dir t
  "Whether directory name of file is displayed in the annotations for `consult-notes-denote'."
  :group 'consult-notes
  :type 'boolean)

(defcustom consult-notes-denote-files-function (function denote-directory-files)
  "Fuction for listing denote files. If only text files are wanted use `denote-directory-text-only-files' instead."
  :group 'consult-notes
  :type 'function)

;;;; Source
(defconst consult-notes-denote--source
  (list :name     (propertize "Denote notes" 'face 'consult-notes-sep)
        :narrow   ?d
        :category consult-notes-category
        :annotate #'consult-notes-denote--annotate
        :items    (lambda ()
                    (let* ((max-width 0)
                           (cands (mapcar (lambda (f)
                                            (let* ((id (denote-retrieve-filename-identifier f))
                                                   (title-1 (or (denote-retrieve-title-value f (denote-filetype-heuristics f)) (denote-retrieve-filename-title f)))
                                                   (title (if consult-notes-denote-display-id
                                                              (concat id " " title-1)
                                                            title-1))
                                                   (dir (file-relative-name (file-name-directory f) denote-directory))
                                                   (keywords (denote-extract-keywords-from-path f)))
                                              (let ((current-width (string-width title)))
                                                (when (> current-width max-width)
                                                  (setq max-width (+ 24 current-width))))
                                              (propertize title 'denote-path f 'denote-keywords keywords)))
                                          (funcall consult-notes-denote-files-function))))
                      (mapcar (lambda (c)
                                (let* ((keywords (get-text-property 0 'denote-keywords c))
                                       (path (get-text-property 0 'denote-path c))
                                       (dirs (directory-file-name (file-relative-name (file-name-directory path) denote-directory))))
                                  (concat c
                                          ;; align keywords
                                          (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                                          (format "%18s"
                                                  (if keywords
                                                      (concat (propertize "#" 'face 'consult-notes-name)
                                                              (propertize (mapconcat 'identity keywords " ") 'face 'consult-notes-name))
                                                    ""))
                                          (when consult-notes-denote-dir (format "%18s" (propertize (concat "/" dirs) 'face 'consult-notes-name))))))
                              cands)))
        ;; Custom preview
        :state  #'consult-notes-denote--state
        ;; Create new note on match fail
        :new     #'consult-notes-denote--new-note))

(defun consult-notes-denote--file (cand)
  (format "%s" (get-text-property 0 'denote-path cand)))

(defun consult-notes-denote--state ()
  "File preview for denote files."
  (let ((open (consult--temporary-files))
        (state (consult--file-state)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall state action (and cand
                                 (consult-notes-denote--file cand))))))

(defun consult-notes-denote--blinks (cand)
  (format "%s" (get-text-property 0 'denote-blinks)))

(defun consult-notes-denote--extension-file-type (f)
  "Return denote file-type of F."
  (pcase (file-name-extension f)
    ("org" "org")
    ("md" "markdown-toml")
    ("txt" "text")))

(defun consult-notes-denote--new-note (cand)
  "Create new note with Denote with title CAND.

Input \"foo\", then create \"id-foo\", file type is determined by
`denote-file-type', choose manually when `denote-prompts' includes
'file-type, or simply include the extension; \"foo.txt\", creates
\"id-foo.txt\."
  (let* ((f (expand-file-name cand denote-directory))
         (f-dir (file-name-directory f))
         (f-name-base (file-name-base f))
         (file-type (consult-notes-denote--extension-file-type f))
         keywords date subdirectory template)
    (dolist (prompt denote-prompts)
      (pcase prompt
        ('keywords (setq keywords (denote-keywords-prompt)))
        ('file-type (setq file-type (denote-file-type-prompt)))
        ('subdirectory (setq subdirectory (denote-subdirectory-prompt)))
        ('date (setq date (denote-date-prompt)))
        ('template (setq template (denote-template-prompt)))))
    (denote (string-trim f-name-base) keywords file-type subdirectory date template)))

;;;; Annotation
(defun consult-notes-denote--annotate (cand)
  "Annotate CAND in `consult-notes-denote'."
  (let* ((path (get-text-property 0 'denote-path cand))
         (attrs (file-attributes path))
         (ftime (consult-notes--time (file-attribute-modification-time attrs)))
         (fsize (file-size-human-readable (file-attribute-size attrs))))
    (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
    (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
    (format "%8s  %8s" fsize ftime)))

(provide 'consult-notes-denote)
;;; consult-notes-denote.el ends here
