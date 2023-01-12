;;; consult-notes-org-roam.el --- Manage org-roam notes with consult -*- lexical-binding: t -*-

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

;; Manage your org-roam notes with consult. Please note that user must have
;; installed org-roam for `consult-notes-org-roam' to function.

;;; Code:

(require 'consult-notes)
(require 's)
(require 'dash)
(unless (require 'org-roam nil 'noerror)
  (message "Org-roam not found! Please ensure that it is installed."))

;;;; Declare Roam Vars & Functions
;; Defined for byte-compilation. These will be set when org-roam is loaded
(defvar org-roam-node-display-template)
(defvar org-roam-directory)

(declare-function org-roam-backlink-source-node "org-roam")
(declare-function org-roam-node-visit "org-roam")
(declare-function org-roam-node-create "org-roam")
(declare-function org-id-get-create "org-roam")
(declare-function org-roam-node-from-id "org-roam")
(declare-function org-roam-backlinks-get "org-roam")
(declare-function org-roam-backlink-target-node "org-roam")
(declare-function org-roam-ref-read--completions "org-roam")
(declare-function org-roam-node-open "org-roam")
(declare-function org-roam-node-title "org-roam")
(declare-function org-roam-node-read--completions "org-roam")
(declare-function org-roam-node-from-title-or-alias "org-roam")
(declare-function org-roam-node-id "org-roam")
(declare-function org-roam-db-query "org-roam")
(declare-function org-roam-node-file-mtime "org-roam")
(declare-function org-roam-node-file "org-roam")

;;;; Variables
(defcustom consult-notes-org-roam-template
  (concat "${title:84} "
          (propertize "${dir:12} "  'face 'consult-key)
          (propertize "${sizes:6} " 'face 'consult-key)
          (propertize "${fmtime} "  'face 'consult-key)
          (propertize "${tags:10} " 'face 'org-tag)
          "${blinks:3} ")
  "Default display template for org-roam notes."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-org-roam-show-file-size nil
  "Show file size in annotations for org-roam notes in `consult-notes'.")

(defcustom consult-notes-org-roam-blinks nil
  "Shoe number of backlinks for org-roam note in `consult-notes'.")

(defcustom consult-notes-org-roam-annotate-function #'consult-notes-org-roam-annotate
  "Function for annotations for org-roam nodes/refs in `consult-notes'.

The default function displays back links, dir, file size, and
modified time. Please see the function
`consult-notes-org-roam-annotate' for details."
  :group 'consult-notes
  :type 'function)

(defcustom consult-notes-org-roam-node-name "Zettel Roam Nodes"
  "Name for org-roam node search section in `consult--multi'."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-org-roam-ref-name "Reference Roam Nodes"
  "Name for org-roam refs search section in `consult--multi'."
  :group 'consult-notes
  :type 'string)

(defcustom consult-notes-org-roam-node-narrow-key '?z
  "Name for narrowing key for org-roam node notes in `consult--multi'."
  :group 'consult-notes
  :type 'key)

(defcustom consult-notes-org-roam-ref-narrow-key '?r
  "Name for narrowing key for org-roam ref notes in `consult--multi'."
  :group 'consult-notes
  :type 'key)

(defvar consult-notes-org-roam--old-display-template nil
  "For internal use only.
Set the old display template value when
`consult-notes-org-roam-mode' is loaded.")

;;;; Functions
;; Display functions
(cl-defmethod org-roam-node-sizes ((node org-roam-node))
  "Display NODE size."
  (file-size-human-readable (file-attribute-size (file-attributes (org-roam-node-file node)))))

(cl-defmethod org-roam-node-dir ((node org-roam-node))
  "Display NODE parent directory."
  (file-name-nondirectory (directory-file-name (file-name-directory (org-roam-node-file node)))))

(cl-defmethod org-roam-node-fmtime ((node org-roam-node))
  "Display NODE modified time."
  (consult-notes--time (org-roam-node-file-mtime node)))

(cl-defmethod org-roam-node-blinks ((node org-roam-node))
  "Display NODE backlinks count."
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                        :from links
                        :where (= dest $s1)
                        :and (= type "id")]
                       (org-roam-node-id node)))))
    (if (> count 0)
        (propertize (format "%3s" count) 'face 'default)
      (propertize (format "%3s" "nil") 'face 'shadow))))


(defun consult-notes-org-roam-annotate (cand)
  "Annotate CAND with useful info."
  (let* ((node
          (org-roam-node-from-title-or-alias cand))
         (file
          (org-roam-node-file node))
         (dir
          (file-name-nondirectory (directory-file-name (file-name-directory file))))
         (size
          (file-size-human-readable (file-attribute-size (file-attributes file))))
         (time
          (consult-notes--time (org-roam-node-file-mtime node)))
         (links (caar (org-roam-db-query
                       [:select (funcall count source)
                        :from links
                        :where (= dest $s1)
                        :and (= type "id")]
                       (org-roam-node-id node)))))

    (put-text-property 0 (length dir)   'face 'consult-notes-dir dir)
    (when consult-notes-org-roam-show-file-size
      (put-text-property 0 (length size)  'face 'consult-notes-size size))
    (put-text-property 0 (length time)  'face 'consult-notes-time time)
    (concat (propertize " " 'display `(space :align-to center))
            (when consult-notes-org-roam-blinks
              (if (> links 0)
                  (propertize (format "%3s" links) 'face 'consult-notes-backlinks)
                (propertize (format "%3s" "nil") 'face 'shadow)))
            " "
            (s-truncate 8 (format "%s" dir) "…")
            " "
            (when consult-notes-org-roam-show-file-size (format "%5s" size))
            " "
            (format "%5s" time))))

(defun consult-notes-org-roam-node-preview ()
  "Create preview function for nodes."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview)))
    (lambda (action cand)
      (let ((node (org-roam-node-from-title-or-alias cand)))
        (unless cand
          (funcall open))
        (if (org-roam-node-p node)
            (funcall preview action
                     (and cand
                          (eq action 'preview)
                          (set-window-start
                           (selected-window)
                           (org-roam-node-point node))
                          (funcall open (org-roam-node-file node)))))))))


;;;; Org-Roam & Consult--Multi
;; Define sources for consult--multi
(defvar consult-notes-org-roam--nodes
  `(:name ,(propertize consult-notes-org-roam-node-name 'face 'consult-notes-sep)
    :narrow ,consult-notes-org-roam-node-narrow-key
    :require-match t
    :category 'org-roam-node
    :annotate ,consult-notes-org-roam-annotate-function
    :items ,(lambda () (let* ((node (mapcar #'cdr (org-roam-node-read--completions)))
                         (title (mapcar #'org-roam-node-title node)))
                    (progn title)))
    :state ,#'consult-notes-org-roam-node-preview
    :action ,(lambda (cand) (let* ((node (org-roam-node-from-title-or-alias cand)))
                         (org-roam-node-open node))))
  "Setup for `org-roam' and `consult--multi'.")

(defvar consult-notes-org-roam--refs
  `(:name ,(propertize consult-notes-org-roam-ref-name 'face 'consult-notes-sep)
    :narrow ,consult-notes-org-roam-ref-narrow-key
    :require-match t
    :category 'org-roam-ref
    :annotate ,consult-notes-org-roam-annotate-function
    :items ,(lambda () (let* ((node (mapcar #'cdr (org-roam-ref-read--completions)))
                         (title (mapcar #'org-roam-node-title node)))
                    (progn title)))
    :state ,#'consult-notes-org-roam-node-preview
    :action (lambda (cand) (let* ((node (org-roam-node-from-title-or-alias cand)))
                        (org-roam-node-open node))))
  "Setup for `org-roam-refs' and `consult--multi'.")

;; Alias org-roam-node-find
(defalias 'consult-notes-org-roam-find-node 'org-roam-node-find
  "Find and open an Org-roam node by its title or alias.

INITIAL-INPUT is the initial input for the prompt. FILTER-FN is a
function to filter out nodes: it takes an `org-roam-node', and
when nil is returned the node will be filtered out. If
OTHER-WINDOW, visit the NODE in another window. The TEMPLATES, if
provided, override the list of capture templates (see
`org-roam-capture-'.)")

;; Find Org-Roam nodes by relation
;; https://ag91.github.io/blog/2021/03/12/find-org-roam-notes-via-their-relations/
;;;###autoload
(defun consult-notes-org-roam-find-node-relation (arg &optional node choices)
  "Navigate org-roam notes by link relation.

With universal ARG tries to navigate the tags of the current
note. Optionally takes a selected NODE and filepaths CHOICES."
  (interactive "P")
  (let* ((choices
          (or choices
              (when arg
                (-map #'org-roam-backlink-target-node (org-roam-backlinks-get (org-roam-node-from-id (or (ignore-errors (org-roam-node-id node))
                                                                                                         (org-id-get-create))))))))
         (all-notes (org-roam-node-read--completions))
         (completions
          (or (--filter (-contains-p choices (cdr it)) all-notes) all-notes))
         (next-node
          ;; taken from org-roam-node-read
          (let* ((nodes completions)
                 (node (completing-read
                        "Node: "
                        (lambda (string pred action)
                          (if (eq action 'metadata)
                              '(metadata
                                (annotation-function . (lambda (title)
                                                         (funcall org-roam-node-annotation-function
                                                                  (get-text-property 0 'node title))))
                                (category . org-roam-node))
                            (complete-with-action action nodes string pred))))))
            (or (cdr (assoc node nodes))
                (org-roam-node-create :title node)))))
    (if (equal node next-node)
        (org-roam-node-visit node)
      (consult-notes-org-roam-find-node-relation nil next-node (cons next-node (-map #'org-roam-backlink-source-node (org-roam-backlinks-get next-node)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Provide Consult Notes Org Roam
(provide 'consult-notes-org-roam)
;;; consult-notes-org-roam.el ends here
