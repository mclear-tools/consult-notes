;;; consult-notes-org-roam.el --- Manage org-roam notes with consult -*- lexical-binding: t -*-

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

;; Manage your org-roam notes with consult.

;;; Code:

(require 'consult-notes)
(require 'org-roam)

;;;; Variables

(defcustom consult-notes-org-roam-template
  (concat "${blinks:3} "
          "${title:84} "
          (propertize "${dir:12} "  'face 'marginalia-size)
          (propertize "${sizes:6} " 'face 'marginalia-size)
          (propertize "${fmtime} "  'face 'marginalia-date)
          (propertize "${tags:10} " 'face 'org-tag))
  "Default display template for org-roam notes."
  :group 'consult-notes
  :type 'string)


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
  (marginalia--time (org-roam-node-file-mtime node)))

(cl-defmethod org-roam-node-blinks ((node org-roam-node))
  "Display NODE backlinks count."
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                        :from links
                        :where (= dest $s1)
                        :and (= type "id")]
                       (org-roam-node-id node))))
         )
    (if (> count 0)
        (format "%d" count)
      "nil")))

;; Alias org-roam-node-find
(defalias 'consult-notes-org-roam-find-node 'org-roam-node-find
  "Find and open an Org-roam node by its title or alias.
        INITIAL-INPUT is the initial input for the prompt.
        FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
        and when nil is returned the node will be filtered out.
        If OTHER-WINDOW, visit the NODE in another window.
        The TEMPLATES, if provided, override the list of capture templates (see
        `org-roam-capture-'.)")

;; Find Org-Roam nodes by relation
;; https://ag91.github.io/blog/2021/03/12/find-org-roam-notes-via-their-relations/
;;;###autoload
(defun consult-notes-org-roam-find-node-relation (arg &optional node choices)
  "Navigate org-roam notes by link relation.
With universal ARG tries to navigate the tags of the current note. Optionally takes a selected NOTE and filepaths CHOICES."
  (interactive "P")
  (let* ((depth (if (numberp arg) arg 1))
         (choices
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

;;;; Consult-Notes-Org-Roam-Mode
;; Define a minor-mode for consult-notes & org-roam
;;;###autoload
(define-minor-mode consult-notes-org-roam-mode
  "Toggle `consult-notes-org-roam-mode' to integrate consult with org-roam.
By enabling `consult-notes-org-roam-mode' the functions `org-roam-node-read' and
`org-roam-ref-read' are overriden by consults-org-roam's equivalents. Optional
argument ARG indicates whether the mode should be enabled or disabled."
  :lighter nil
  (require 'org-roam)
  ;; Add or remove advice when enabled respectively disabled
  (if consult-notes-org-roam-mode
      (progn
        ;; Save previous value of display-template
        (setq org-roam-old-display-template org-roam-node-display-template)
        ;; Set new value
        (setq org-roam-node-display-template consult-notes-org-roam-template))
    (progn
      ;; Reset display template value
      (setq org-roam-node-display-template org-roam-old-display-template))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Provide Consult Notes Org Roam
(provide 'consult-notes-org-roam)
;;; consult-notes-org-roam.el ends here
