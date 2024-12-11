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

(defcustom consult-notes-denote-files-function
  (lambda () (denote-directory-files nil t nil))
  "Function for listing Denote files. All files, only Denote files (Org, Markdown or TXT) or a regular expression."
  :group 'consult-notes
  :type '(choice
          (const :tag "All files" 
                 (lambda () (denote-directory-files nil t nil)))
          (const :tag "Denote files"
                 (lambda () (denote-directory-files nil t t)))
          (function :tag "Custom regex function"
                    :value (lambda () 
                             (let ((regex (read-string "Enter regex: ")))
                               (denote-directory-files regex t nil))))))

(defcustom consult-notes-denote-annotate-function #'consult-notes-denote--annotate
  "Function to call for annotations of file note directories in `consult-notes'.

The default function displays dir, file size, and modified time.
Please see the function `consult-notes-denote--annotate' for
details."
  :group 'consult-notes
  :type 'function)

(defcustom consult-notes-denote-display-keywords-function #'consult-notes-denote--display-keywords
  "Function to display the keywords of the file in the annotations for `consult-notes-denote'."
  :group 'consult-notes
  :type 'function)

(defcustom consult-notes-denote-display-keywords-indicator "#"
  "Prefix to indicate Denote keywords of the file in the annotations for `consult-notes-denote-display-keywords-function'."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-denote-display-keywords-width 20
  "Minimum width reserved for keywords in the annotations for `consult-notes-denote-display-keywords-function'."
  :group 'consult-notes
  :type 'integer)

(defcustom consult-notes-denote-display-dir-function #'consult-notes-denote--display-dir
  "Function used to display the directory name of the file in the annotations for `consult-notes-denote'.

This function is only called when `consult-notes-denote-dir' is not nil."
  :group 'consult-notes
  :type 'function)

(defcustom consult-notes-denote-title-margin 24
  "Margin between the title and the keywords in the annotations for `consult-notes-denote'."
  :group 'consult-notes
  :type 'integer)

;;;; Source
(defconst consult-notes-denote--source
  (list :name     (propertize "Denote notes" 'face 'consult-notes-sep)
        :narrow   ?d
        :category consult-notes-category
        :annotate consult-notes-denote-annotate-function
        :items    (lambda ()
                    (let* ((max-width 0)
			   (max-title-width (- (window-width (minibuffer-window)) consult-notes-denote-display-keywords-width))
                           (cands (mapcar (lambda (f)
                                            (let* ((id (denote-retrieve-filename-identifier f))
                                                   (title-1 (or (denote-retrieve-title-value f (denote-filetype-heuristics f))
								(denote-retrieve-filename-title f)))
                                                   (title (if consult-notes-denote-display-id
                                                              (concat id " " title-1)
                                                            title-1))
                                                   (dir (file-relative-name (file-name-directory f) denote-directory))
                                                   (keywords (denote-extract-keywords-from-path f)))
                                              (let ((current-width (string-width title)))
                                                (when (> current-width max-width)
                                                  (setq max-width (min (+ consult-notes-denote-title-margin current-width)
								       max-title-width))))
                                              (propertize title 'denote-path f 'denote-keywords keywords)))
                                          (funcall consult-notes-denote-files-function))))
                      (mapcar (lambda (c)
                                (let* ((keywords (get-text-property 0 'denote-keywords c))
                                       (path (get-text-property 0 'denote-path c))
                                       (dirs (directory-file-name (file-relative-name (file-name-directory path) denote-directory))))
                                  (concat c
                                          ;; align keywords
                                          (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
					  (propertize (funcall consult-notes-denote-display-keywords-function keywords) 'face 'consult-notes-name)
					  (when consult-notes-denote-dir
					    (propertize (funcall consult-notes-denote-display-dir-function dirs) 'face 'consult-notes-name)))))
                              cands)))
        ;; Custom preview
        :state  #'consult-notes-denote--state
        ;; Create new note on match fail
        :new     #'consult-notes-denote--new-note))

(defun consult-notes-denote--display-keywords (keywords)
  (format "%18s" (if keywords (concat
			       consult-notes-denote-display-keywords-indicator
			       (mapconcat 'identity keywords " ")) "")))

(defun consult-notes-denote--display-dir (dirs)
  (format "%18s" (concat "/" dirs)))

(defun consult-notes-denote--file (cand)
  (format "%s" (get-text-property 0 'denote-path cand)))

;; Helper function
(defun consult-notes-denote--excluded-p (file)
  "Return non-nil if FILE should be excluded from preview."
  (seq-some (lambda (pattern)
              (string-match-p pattern file))
            consult-preview-excluded-files))

;; Preview & make sure to respect excluded files
(defun consult-notes-denote--state ()
  "File preview for denote files."
  (let ((open (consult--temporary-files))
        (state (consult--file-state)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall state action (and cand
                                 (not (consult-notes-denote--excluded-p cand))
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
