;; Completing-read interface using consult. Override `org-roam-node-read' so
;; that every completing function resorts to consult
(defun consult-notes--org-roam-node-read (&optional initial-input filter-fn sort-fn
                                                    require-match prompt)
  "Read and return an `org-roam-node' with the help of consult.
INITIAL-INPUT is the initial minibuffer prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-read-sort-by-file-mtime'
for an example sort function.
If REQUIRE-MATCH, the minibuffer prompt will require a match.
PROMPT is a string to show at the beginning of the mini-buffer,
defaulting to \'Node: \'."
  (let* ((nodes (org-roam-node-read--completions filter-fn sort-fn)) ;;
         (prompt (or prompt "Node: "))
         ;; Sets state-func only when there are nodes to avoid errors
         ;; with empty roam-dirs
         (state-func (when nodes
                       (consult-notes--org-roam-node-preview)))
         (node
          (consult--read
           nodes
           :prompt prompt
           :initial initial-input
           :sort sort-fn
           :require-match require-match
           :category 'org-roam-node
           ;;:history 'org-roam-node-history
           :state state-func
           ;; Uses the DEFAULT argument of alist-get to return input in case the input is not found as key.
           :lookup (lambda (selected candidates input narrow) (alist-get selected candidates input nil #'equal)))))
    (if (org-roam-node-p node) (progn node)
      (progn (org-roam-node-create :title node)))))

;; Completing-read interface for references using consult. Override
;; `org-roam-ref-read' so that each an every completing function
;; regarding refs resorts to consult
(defun consult-notes--org-roam-ref-read (&optional initial-input filter-fn)
  "Read a ref and return its `org-roam-node' with the help of consult.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (let* ((refs (org-roam-ref-read--completions))
         (refs (cl-remove-if-not (lambda (n)
                                   (if filter-fn (funcall filter-fn (cdr n)) t)) refs))
         (ref (consult--read
               refs
               :prompt "Refs: "
               :initial initial-input
               :predicate filter-fn
               :require-match t
               :category 'org-roam-ref
               :history 'org-roam-ref-history
               :state (consult-notes--org-roam-node-preview)
               :lookup #'consult--lookup-cdr)))
    (progn ref)))

(defun consult-notes--org-roam-node-preview ()
  "Create preview function for nodes."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview)))
    (lambda (action cand)
      (when (eq action 'exit)
        (funcall open))
      (if (org-roam-node-p cand)
          (funcall preview action
                   (and cand
                        (eq action 'preview)
                        (set-window-start
                         (selected-window)
                         (org-roam-node-point cand))
                        (funcall open (org-roam-node-file cand))))))))















;; Override org-roam with consult
;; See https://github.com/jgru/consult-org-roam
(defun consult-notes--org-roam-select-file (&optional prompt list)
  "Wrapper around `consult--read' to select an org-roam file.
Offers candidates withing `org-roam-directory', or from LIST when
supplied. Can take a PROMPT argument."
  (let* ((files (if list list
                  (org-roam-list-files)))
         (prompt (if prompt prompt
                   "Select File: ")))
    (consult--read
     files
     :prompt prompt
     :sort t
     :require-match t
     :state (consult--file-preview))))

(defun consult-notes--org-roam-node-preview ()
  "Create preview function for nodes."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview)))
    (lambda (action cand)
      (when (eq action 'exit)
        (funcall open))
      (if (org-roam-node-p cand)
          (funcall preview action
                   (and cand
                        (eq action 'preview)
                        (set-window-start
                         (selected-window)
                         (org-roam-node-point cand))
                        (funcall open (org-roam-node-file cand))))))))

;; Completing-read interface using consult. Override `org-roam-node-read' so
;; that every completing function resorts to consult
(defun consult-notes--org-roam-node-read (&optional initial-input filter-fn sort-fn
                                                    require-match prompt)
  "Read and return an `org-roam-node' with the help of consult.
INITIAL-INPUT is the initial minibuffer prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-read-sort-by-file-mtime'
for an example sort function.
If REQUIRE-MATCH, the minibuffer prompt will require a match.
PROMPT is a string to show at the beginning of the mini-buffer,
defaulting to \'Node: \'."
  (let* ((nodes (org-roam-node-read--completions filter-fn sort-fn)) ;;
         (prompt (or prompt "Node: "))
         ;; Sets state-func only when there are nodes to avoid errors
         ;; with empty roam-dirs
         (state-func (when nodes
                       (consult-notes--org-roam-node-preview)))
         (node
          (consult--read
           nodes
           :prompt prompt
           :initial initial-input
           :sort sort-fn
           :require-match require-match
           :category 'org-roam-node
           ;;:history 'org-roam-node-history
           :state state-func
           ;; Uses the DEFAULT argument of alist-get to return input in case the input is not found as key.
           :lookup (lambda (selected candidates input narrow) (alist-get selected candidates input nil #'equal)))))
    (if (org-roam-node-p node) (progn node)
      (progn (org-roam-node-create :title node)))))

;; Completing-read interface for references using consult. Override
;; `org-roam-ref-read' so that each an every completing function
;; regarding refs resorts to consult
(defun consult-notes--org-roam-ref-read (&optional initial-input filter-fn)
  "Read a ref and return its `org-roam-node' with the help of consult.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (let* ((refs (org-roam-ref-read--completions))
         (refs (cl-remove-if-not (lambda (n)
                                   (if filter-fn (funcall filter-fn (cdr n)) t)) refs))
         (ref (consult--read
               refs
               :prompt "Refs: "
               :initial initial-input
               :predicate filter-fn
               :require-match t
               :category 'org-roam-ref
               :history 'org-roam-ref-history
               :state (consult-notes--org-roam-node-preview)
               :lookup #'consult--lookup-cdr)))
    (progn ref)))


(defun consult-notes-org-roam-backlinks ()
  "Select from list of all notes that link to the current note."
  (interactive)
  (let* ((node (org-roam-node-at-point))
         (ids (mapcar (lambda (el) (car el))(org-roam-db-query
                                        [:select [source]
                                         :from links
                                         :where (= dest $s1)
                                         :and (= type "id")]
                                        (if node
                                            (org-roam-node-id (org-roam-node-at-point))
                                          (user-error "Buffer does not contain org-roam-nodes"))))))
    (if ids
        (consult-notes--org-roam-node-read "" (lambda (n)
                                                (if (org-roam-node-p n)
                                                    (if (member (org-roam-node-id n) ids)
                                                        t
                                                      nil))))c ww      (user-error "No backlinks found"))))


(defun consult-notes-org-roam-forward-links ()
  "Select a forward link contained in the current buffer."
  (interactive)
  (let ((id-links '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "id")
          (push
           (org-element-property :path link) id-links))))
    (if id-links
        (consult-notes--org-roam-node-read "" (lambda (n)
                                                (if (org-roam-node-p n)
                                                    (if (member (org-roam-node-id n) id-links)
                                                        t
                                                      nil))))
      (user-error "No forward links found"))))

(defun consult-notes--org-roam-node-preview ()
  "Create preview function for nodes."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview)))
    (lambda (action cand)
      (when (eq action 'exit)
        (funcall open))
      (if (org-roam-node-p cand)
          (funcall preview action
                   (and cand
                        (eq action 'preview)
                        (set-window-start
                         (selected-window)
                         (org-roam-node-point cand))
                        (funcall open (org-roam-node-file cand))))))))

(define-minor-mode consult-notes-org-roam-mode
  "Toggle `consult-notes-org-roam-mode' to integrate consult with org-roam.
By enabling `consult-notes-org-roam-mode' the functions `org-roam-node-read' and
`org-roam-ref-read' are overriden by consults-org-roam's equivalents. Optional
argument ARG indicates whether the mode should be enabled or disabled."
  :lighter " cor"
  ;; Add or remove advice when enabled respectively disabled
  (if consult-notes-org-roam-mode
      (progn
        (advice-add #'org-roam-node-read :override #'consult-notes--org-roam-node-read)
        (advice-add #'org-roam-ref-read :override #'consult-notes--org-roam-ref-read)
        (setq org-roam-node-display-template consult-notes-org-roam-template))
    (progn
      (advice-remove #'org-roam-node-read #'consult-notes--org-roam-node-read)
      (advice-remove #'org-roam-ref-read #'consult-notes--org-roam-ref-read))))

---



(format "%d" (caar (org-roam-db-query
                    [:select (funcall count source)
                     :from links
                     :where (= dest $s1)
                     :and (= type "id")]
                    (org-roam-node-id (org-roam-node-from-id "20210718T000842.009199")))))

(defvar consult-notes-org-roam-refs
  `(:name "Test Ref"
    :narrow ?t
    :require-match t
    :category 'org-roam-ref
    ;; :annotate ,(lambda (cand)
    ;;              (let* ((file (org-roam-node-file (org-roam-node-from-title-or-alias cand)))
    ;;                     (attrs (file-attributes file))
    ;;                     (dir (file-name-nondirectory (directory-file-name (file-name-directory file))))
    ;;                     (size
    ;;                      (file-size-human-readable (file-attribute-size (file-attributes file))))
    ;;                     (time (consult-notes--time (file-attribute-modification-time attrs)))
    ;;                     )
    ;;                (concat (format "%7s %7s" dir size) "  " (format "%10s" time))
    ;;                ))
    :annotate ,#'consult-notes-org-roam-annotate
    :items ,(lambda () (let* ((node (mapcar #'cdr (org-roam-ref-read--completions)))
                         (title (mapcar #'org-roam-node-title node))
                         )
                    (progn title)))
    ;; :action ,(lambda () (org-roam-node-open (org-roam-node-from-ref)))
    ))
(org-roam-node-file (org-roam-node-from-title-or-alias "(0000): Intellectual Isolation"))

(file-size-human-readable (file-attribute-size (file-attributes (org-roam-node-file (org-roam-node-from-ref "@kitcher2011")))))

(consult-notes--time (file-attributes (org-roam-node-file (org-roam-node-from-ref "@kitcher2011"))))

(let* ((keys
        (mapcar #'car (org-roam-ref-read--completions)))
       (refs
        (mapcar (lambda (r)
                  (org-roam-node-from-ref (concat "@" r)))
                keys))
       (title
        (mapcar (lambda (t)
                  (org-roam-node-title t))
                refs)))
  (progn title))

;; :annotate ,(lambda ()
;;              (let* ((node (mapcar #'cdr (org-roam-ref-read--completions)))
;;                     (file (mapcar #'org-roam-node-file node))
;;                     (attrs (file-attributes file))
;;                     (fsize (file-size-human-readable (file-attribute-size attrs)))
;;                     ;; (ftime (consult-notes--time (file-attribute-modification-time attrs)))
;;                     )
;; (put-text-property 0 (length name)  'face 'consult-key name)
;; (put-text-property 0 (length fsize) 'face 'consult-key fsize)
;; (put-text-property 0 (length ftime) 'face 'consult-key ftime)
;; (format "%7s"  fsize)))
;; :annotate ,(apply-partially 'consult-annotate-note cand)
;; :annotate ,(lambda (cand) (format "%S" (org-roam-node-title (cdr cand))))
;; :annotate: org-roam-ref-annotation-function

;; (defvar consult-notes-org-roam-refs
;;   `(:name "Test REFS"
;;     :narrow ?t
;;     :require-match
;;     (confirm-nonexistent-file-or-buffer)
;;     :category 'org-roam-node
;;     :items  ,#'consult-notes-org-roam-find-node)
;;   ;; :action ,
;;   )


(completing-read "Test: " ( (mapcar #'car (org-roam-ref-read--completions))))

(org-roam-node-from-ref (concat "@" (cdr (org-roam-ref-read--completions)))

                        (lambda ("kitcher2011") (org-roam-node-open (org-roam-node-from-ref (concat "@" (s-trim (truncate-string-to-width "kitcher2011                               this " 15)))))

                          (org-roam-node-open (consult-org-roam-ref-read))



                          (let* ((ids
                                  (org-roam-db-query [:select * :from citations
                                                      :where (= cite-key $s1)]
                                                     (car reference)))
                                 (anodes
                                  (mapcar (lambda (id)
                                            (org-roam-node-from-id (car id)))
                                          ids))

                                 (lambda (f)
                                   (let* ((ref (mapcar #'car (org-roam-ref-read--completions)))
                                          (template
                                           (org-roam-node--process-display-format org-roam-node-display-template))
                                          (bnodes
                                           (mapcar (lambda (node)
                                                     (org-roam-node-read--to-candidate node template)) anodes))
                                          (node (completing-read
                                                 "Node: "
                                                 (lambda (string pred action)
                                                   (if (eq action 'metadata)
                                                       `(metadata
                                                         ;; get title using annotation function
                                                         (annotation-function
                                                          . ,(lambda (title)
                                                               (funcall org-roam-node-annotation-function
                                                                        (get-text-property 0 'node title))))
                                                         (category . org-roam-node))
                                                     (complete-with-action action bnodes string pred)))))
                                          (fnode
                                           (cdr (assoc node bnodes))))
                                     (if ids
                                         ;; Open node in other window
                                         (org-roam-node-open fnode)
                                       (message "No notes cite this reference.")))

                                   (defvar consult-notes--sources-dir-data
                                     (mapcar #'(lambda (s) (apply 'consult-notes-make-source s))
		                                     consult-notes-sources-data))

                                   (defvar consult-notes--sources-dirs nil)

                                   (dolist (i consult-notes--sources-dir-data)
                                     (push i consult-notes-sources))

                                   (push consult-notes-org-roam-test consult-notes-sources)
                                   (setq consult-notes-sources (remove consult-notes-org-roam-test consult-notes-sources))

                                   (setq consult-notes-sources '(
                                                                 consult--source-recent-file
                                                                 ;; consult-notes-org-roam-test
                                                                 ))

                                   (defvar consult-notes-org-roam-test
                                     `(:name "Test : "
                                       :narrow ?t
                                       :category org-roam-node
                                       ))




                                   :items ,(lambda (&optional initial-input filter-fn)
                                             (let* ((refs (org-roam-ref-read--completions))
                                                    (refs (cl-remove-if-not (lambda (n)
                                                                              (if filter-fn (funcall filter-fn (cdr n)) t)) refs))
                                                    (ref (consult--read
                                                          refs
                                                          :prompt "Refs: "
                                                          :initial initial-input
                                                          :predicate filter-fn
                                                          :require-match t
                                                          :category 'org-roam-ref
                                                          :history 'org-roam-ref-history
                                                          ;; :state (consult-org-roam--node-preview)
                                                          :lookup #'consult--lookup-cdr)))
                                               (progn ref)))
                                   ))


                            ---------------


                            (setq refs (mapcar (lambda (ref) (org-roam-node-file (org-roam-ref-read--completions)))
                                               (org-roam-ref-read)))

                            (org-roam-db-query [:select * :from citations :where (= cite-key "wuerth2014")])

                            (citar--completion-table "@kitcher2011")




                            (setq ordb (org-roam-db-query [:select * :from citations
                                                           :where (= cite-key "wuerth2014")]
                                                          (car (cons "wuerth2014" (citar--get-entry "wuerth2014")))))


                            (setq tnode (mapcar (lambda (id)
                                                  (org-roam-node-from-id (car id)))
                                                ordb))

                            (setq template (org-roam-node--process-display-format org-roam-node-display-template))
                            (setq nlist (mapcar (lambda (node)
                                                  (org-roam-node-read--to-candidate node template)) tnode))

                            (setq one (completing-read "Notes: " nlist))

                            (defun consult-org-roam-ref-read (&optional initial-input filter-fn)
                              "Read a ref and return its `org-roam-node' with the help of consult.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
filtered out."
                              (let* ((refs (org-roam-ref-read--completions))
                                     (refs (cl-remove-if-not (lambda (n)
                                                               (if filter-fn (funcall filter-fn (cdr n)) t)) refs))
                                     (ref (consult--read
                                           refs
                                           :prompt "Refs: "
                                           :initial initial-input
                                           :predicate filter-fn
                                           :require-match t
                                           :category 'org-roam-ref
                                           :history 'org-roam-ref-history
                                           ;; :state (consult-org-roam--node-preview)
                                           :lookup #'consult--lookup-cdr)))
                                (progn ref)))

                            (consult-org-roam-ref-read)

                            (defun concite-notes-org-roam ()
                              "Notes."
                              (consult-org-roam-ref-read))

                            (defvar concite-source-notes
                              `(:name "Notes"
                                :narrow ?n
                                :category org-roam-node
                                :items ,(lambda () (consult-org-roam-ref-read))))

                            (add-to-list 'consult-notes-sources-data 'concite-source-notes 'append)
                            (delete 'concite-source-notes consult-notes-sources-data)
