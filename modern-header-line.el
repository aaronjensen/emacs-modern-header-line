;;; modern-header-line.el --- Minimal modeline -*- lexical-binding: t -*-

;; Stripped-down version of rougier/modern-header-line containing only
;; the functionality actually used in this configuration.

;;; Code:

(defgroup modern-header-line nil
  "Nano Modeline"
  :group 'convenience)

(defface modern-header-line-active
  `((t (:foreground ,(face-foreground 'default)
        :background ,(face-background 'header-line nil t)
        :box (:line-width 1 :color ,(face-background 'default)))))
  "Face for active modeline.")

(defface modern-header-line-inactive
  `((t (:inherit (,(when (facep 'nano-faded) 'nano-faded)
                  modern-header-line-active))))
  "Face for inactive modeline.")

(defface modern-header-line--empty-face
  `((t (:foreground ,(face-foreground 'default))))
  "Empty face for resetting header-line.")

(defface modern-header-line-name-active
  '((t :weight semibold
       :inherit (variable-pitch modern-header-line-active)))
  "Active name face.")

(defface modern-header-line-name-inactive
  '((t :weight semibold
       :inherit (variable-pitch nano-faded modern-header-line-inactive)))
  "Inactive name face.")

(defface modern-header-line-project-active
  '((t :height 0.8
       :inherit (variable-pitch modern-header-line-active)))
  "Active project face.")

(defface modern-header-line-project-inactive
  '((t :height 0.8
       :inherit (variable-pitch nano-faded modern-header-line-inactive)))
  "Inactive project face.")

(defface modern-header-line-secondary
  '((t :height 0.94
       :inherit (nano-faded)))
  "Secondary face.")

(defcustom modern-header-line-faces
  '((header-active      . (modern-header-line-active))
    (header-inactive    . (modern-header-line-inactive))
    (name-active        . (modern-header-line-name-active))
    (name-inactive      . (modern-header-line-name-inactive))
    (project-active     . (modern-header-line-project-active))
    (project-inactive   . (modern-header-line-project-inactive))
    (secondary-active   . (modern-header-line-secondary))
    (secondary-inactive . (modern-header-line-secondary)))
  "Nano modeline faces alist."
  :type '(alist :key-type symbol :value-type (repeat face))
  :group 'modern-header-line)

(defvar modern-header-line-base-face nil
  "Base face used during modeline rendering.")

(defvar modern-header-line--selected-window nil
  "Selected window before mode-line was activated.")

(defcustom modern-header-line-abbreviations nil
  "Alist of abbreviations for long project names.
Each element is (WORD . ABBREVIATION)."
  :type '(alist :key-type string :value-type string)
  :group 'modern-header-line)

(defvar modern-header-line--project-relative-name-cache
  (make-hash-table :test 'equal))

(defun modern-header-line--update-selected-window ()
  "Update selected window (before mode-line is active)."
  (setq modern-header-line--selected-window (selected-window)))

(defun modern-header-line--base-face (face-prefix)
  "Return the face for FACE-PREFIX according to current active state."
  (let* ((window (get-buffer-window (current-buffer)))
         (active (eq window modern-header-line--selected-window))
         (state (intern (concat (symbol-name face-prefix)
                                (if active "-active" "-inactive"))))
         (face (cadr (assoc state modern-header-line-faces))))
    face))

(defun modern-header-line-face (&optional face-prefix)
  "Return the face for FACE-PREFIX according to current active state.
Make it inherit the base face."
  (let* ((window (get-buffer-window (current-buffer)))
         (active (eq window modern-header-line--selected-window))
         (state (intern (concat (symbol-name face-prefix)
                                (if active "-active" "-inactive"))))
         (face (cdr (assoc state modern-header-line-faces)))
         (face (if modern-header-line-base-face
                   (push modern-header-line-base-face face)
                 face))
         (face (reverse face)))
    `(:inherit ,face)))

(defun modern-header-line--make (left right face-prefix)
  "Build a dynamic mode/header line made of LEFT and RIGHT part,
using the given FACE-PREFIX as the default."
  `(:eval
    (let* ((modern-header-line-base-face (modern-header-line--base-face ',face-prefix))
           (left (mapconcat
                  (lambda (element)
                    (if (stringp element)
                        (propertize element 'face modern-header-line-base-face)
                      (apply (car element) (cdr element))))
                  ',left))
           (right (mapconcat
                   (lambda (element)
                     (if (stringp element)
                         (propertize element 'face modern-header-line-base-face)
                       (apply (car element) (cdr element))))
                   ',right))
           (fringe (if fringes-outside-margins 0.0 -1.0)))
      (concat (propertize " "
                          'display `(space :align-to (+ left
                                                        (,fringe . left-fringe)
                                                        (0.0 . left-margin))))
              left
              (propertize " "
                          'face `(:inherit ,modern-header-line-base-face)
                          'display `(space :align-to (- right
                                                        (,fringe . right-fringe)
                                                        (0.0 . right-margin)
                                                        ,(length right))))
              right))))

(defun modern-header-line-header (left &optional right)
  "Install a header line made of LEFT and RIGHT parts."
  (setq-local header-line-format (modern-header-line--make left right 'header))
  (face-remap-set-base 'header-line 'modern-header-line--empty-face)
  (add-hook 'post-command-hook #'modern-header-line--update-selected-window))

(defun modern-header-line-cursor-position (&optional format)
  "Cursor position using given FORMAT."
  (let ((format (or format "%l:%c ")))
    (propertize (format-mode-line format)
                'face (modern-header-line-face 'secondary))))

(defun modern-header-line--shorten-directory-path (path threshold)
  "Shorten a directory PATH to single letters if past THRESHOLD.
Never shorten the last part of the path."
  (let* ((path (abbreviate-file-name path))
         (parts (split-string path "/"))
         (shortened-parts))
    (while parts
      (let ((part (car parts)))
        (if (or (not (cdr parts))
                (< (length (string-join (append parts shortened-parts) "/"))
                   threshold))
            (push part shortened-parts)
          (push (if (zerop (length part))
                    part
                  (substring part 0 1))
                shortened-parts)))
      (setq parts (cdr parts)))
    (string-join (reverse shortened-parts) "/")))

(defun modern-header-line-project-root ()
  "Return the current project root or nil."
  (when-let* ((project (project-current)))
    (project-root project)))

(defun modern-header-line-project-name ()
  "Return the current project name or nil."
  (when-let* ((project-root (modern-header-line-project-root)))
    (file-name-nondirectory (directory-file-name project-root))))

(defun modern-header-line-project-relative-name (file-name max-width)
  "Return FILE-NAME relative to project, shortened to MAX-WIDTH."
  (let ((key (cons file-name max-width)))
    (or (gethash key modern-header-line--project-relative-name-cache)
        (puthash key
                 (modern-header-line--shorten-directory-path
                  (if-let* ((project-root (modern-header-line-project-root)))
                      (file-relative-name file-name project-root)
                    file-name)
                  max-width)
                 modern-header-line--project-relative-name-cache))))

(defun modern-header-line-buffer-file-name ()
  "Return the buffer file name, relative to project and shortened."
  (when buffer-file-name
    (modern-header-line-project-relative-name
     (substring-no-properties buffer-file-name)
     (- (window-width) 20))))

(defun modern-header-line-buffer-name (&optional name)
  "Return formatted buffer NAME with modified indicator."
  (concat
   (propertize
    (or name
        (modern-header-line-buffer-file-name)
        (format-mode-line "%b"))
    'face (modern-header-line-face 'name))
   (propertize
    (if (and buffer-file-name
             (buffer-modified-p))
        (concat (propertize " " 'display '(space :width (3)))
                (propertize "⬤" 'display '((raise 0.15) (height 0.4)))
                (propertize " " 'display '(space :width (5))))
      (propertize " " 'display '(space :width (16))))
    'face (modern-header-line-face 'header))))

(defun modern-header-line--abbreviate (text)
  "Abbreviate TEXT using `modern-header-line-abbreviations'."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "\\b\\w+\\b" nil t)
      (let* ((word (match-string 0))
             (abbr (assoc-default word modern-header-line-abbreviations)))
        (when abbr
          (replace-match abbr))))
    (buffer-string)))

(defun modern-header-line-project (&rest _args)
  "Return formatted current project name."
  (propertize
   (let ((name (modern-header-line-project-name))
         (max-length 32))
     (if name
         (progn
           (when (> (length name) max-length)
             (setq name (modern-header-line--abbreviate name)))
           (when (> (length name) max-length)
             (setq name (concat
                         (substring name 0 (- max-length 1))
                         "…")))
           (concat "[" name "]"))
       ""))
   'face (modern-header-line-face 'project)))

(defun modern-header-line-window-dedicated ()
  "Return pin indicator if window is dedicated."
  (when (window-dedicated-p)
    (propertize "🖈 " 'face (modern-header-line-face 'secondary) 'display '(height 0.85))))

(defun modern-header-line-prog-mode ()
  "Nano line for prog mode."
  (modern-header-line-header
   '((modern-header-line-window-dedicated)
     (modern-header-line-buffer-name)
     (modern-header-line-project))
   '((modern-header-line-cursor-position))))

(defun modern-header-line-text-mode ()
  "Nano line for text mode."
  (modern-header-line-header
   '((modern-header-line-window-dedicated)
     (modern-header-line-buffer-name)
     (modern-header-line-project))
   '((modern-header-line-cursor-position))))

(defun modern-header-line ()
  "Set modeline according to major mode."
  (cond ((derived-mode-p 'prog-mode)
         (modern-header-line-prog-mode))
        ((derived-mode-p 'vterm-mode))
        (t
         (modern-header-line-text-mode))))

(defun modern-header-line-org-src-mode ()
  "Applies the styling to the header-line-format that org-src-mode sets"
  (modern-header-line-header
   `(,header-line-format)
   '((modern-header-line-cursor-position))))

;;;###autoload
(defun modern-header-line-mode ()
  "Enable modern-header-line."
  (setq-default mode-line-format nil)
  (add-hook 'after-change-major-mode-hook #'modern-header-line)
  (add-hook 'org-src-mode-hook #'modern-header-line-org-src-mode)
  (modern-header-line))

(provide 'modern-header-line)
;;; modern-header-line.el ends here
