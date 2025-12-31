;;; javelin.el --- Implementation of harpoon: bookmarks on steroids -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Damian Barabonkov
;; Based on harpoon.el, Copyright (C) 2022 Otávio Schwanck

;; Author: Damian Barabonkov
;; Keywords: tools languages
;; Homepage: https://github.com/DamianB-BitFlipper/javelin.el
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a plugin based on harpoon from vim (by ThePrimeagen).
;;
;; It is a buffer quick-bookmark manager on steroids. Each "bookmark" is called a javelin
;; position, and you can easily assign and delete buffers to javelin positions. Javelin
;; positions are namespaced by project and branch if within a git repository. Otherwise,
;; just by project if not within git. Globally namespaced otherwise.

(require 'subr-x)
(require 'cl-lib)
(require 'project)

;; Since these are optional, simply declare the external functions
;; that may or may not be available
(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function magit-get-current-branch "magit")

;;; Code:

;;; --- Customizable variables ---

(defgroup javelin nil
  "Organize bookmarks by project and branch."
  :group 'tools)

(defcustom javelin-cache-dir (concat user-emacs-directory ".local/javelin/")
  "Where the cache will be saved."
  :type 'string)

(defcustom javelin-separate-by-branch t
  "Javelin separated by branch."
  :type 'boolean)

(defcustom javelin-default-positions-namespace "javelin"
  "Namespace for javelin positions when not in a project.
Used as a fallback identifier for the cache file when javelin cannot
detect a project root. The namespace helps isolate javelin positions
for different contexts."
  :type 'string)
;;; --- Customizable variables ---

(defvar javelin-project-provider (if (featurep 'projectile) 'projectile 'project)
  "Project provider to use for getting project root.
Can be `projectile' or `project'.")

(defvar javelin-git-branch-provider (if (featurep 'magit) 'magit 'git)
  "Git branch provider to use for getting current branch.
Can be `magit' or `git'.")

(defun javelin--get-project-root ()
  "Get the project root.
Returns the project root path as a string, or nil if there is no project."
  (cond
   ((eq javelin-project-provider 'projectile) (projectile-project-root))
   ((eq javelin-project-provider 'project)
    (when-let ((proj (project-current)))
      (expand-file-name (project-root proj))))))

(defun javelin--get-project-name ()
  "Get the javelin project name.
Returns the project name as a string, or nil if there is no project."
  (when (javelin--get-project-root)
    (cond
     ((eq javelin-project-provider 'projectile) (projectile-project-name))
     ;; The `project' package has no built-in way to get the project name.
     ;; Extract it by splitting the project root path by "/" and taking
     ;; the second-to-last element (the directory name before the trailing slash).
     ;; e.g., "/home/user/projects/myproject/" -> "myproject"
     ((eq javelin-project-provider 'project)
      (let* ((project-path (javelin--get-project-root))
             (path-parts (split-string project-path "/"))
             (name-index (- (length path-parts) 2)))
        (nth name-index path-parts))))))

(defun javelin--get-branch-name ()
  "Get the branch name for javelin.
Uses `javelin-git-branch-provider' to determine the method.
Returns nil if not in a git repository."
  (cond
   ((eq javelin-git-branch-provider 'magit)
    (magit-get-current-branch))
   ((eq javelin-git-branch-provider 'git)
    (condition-case nil
        (string-trim
         (shell-command-to-string
          (concat "cd " (javelin--get-project-root) "; git rev-parse --abbrev-ref HEAD")))
      (error nil)))))

(defun javelin--cache-key ()
  "Key to save current file on cache.
Returns `javelin-default-positions-namespace' if there is no project."
  ;; Use `url-hexify-string' to percent-encode the cache key, making it
  ;; filename-friendly by escaping "/" and other forbidden characters.
  (let ((project-name (or (javelin--get-project-name) javelin-default-positions-namespace)))
    (url-hexify-string
     (if-let ((branch (and javelin-separate-by-branch (javelin--get-branch-name))))
         (concat project-name "#" branch)
       project-name))))

(defun javelin--cache-file-name ()
  "File name for javelin on current project."
  (concat javelin-cache-dir (javelin--cache-key) ".json"))

(defun javelin--ensure-cache-file ()
  "Create javelin cache dir and file if they don't exist."
  (unless (file-directory-p javelin-cache-dir)
    (make-directory javelin-cache-dir t))
  (unless (file-exists-p (javelin--cache-file-name))
    (write-region (json-serialize '()) nil (javelin--cache-file-name))))

(defun javelin--sort-positions (data)
  "Sort DATA by `javelin_position' in increasing order."
  (seq-sort (lambda (a b)
              (< (alist-get 'javelin_position a)
                 (alist-get 'javelin_position b)))
            data))

(defun javelin--read-javelin-positions ()
  "Read and parse the javelin positions cache JSON file.
Returns a list of alists with `javelin_position' and `filepath' keys."
  (let ((data (if (file-exists-p (javelin--cache-file-name))
                  (condition-case nil
		      (with-temp-buffer
			(insert-file-contents (javelin--cache-file-name))
			(goto-char (point-min))
			(json-parse-buffer :object-type 'alist
                                           :array-type 'list))
                    ;; JSON file got corrupted, delete it and return empty list
                    (error
                     (delete-file (javelin--cache-file-name))
                     '()))
                '())))
    (javelin--sort-positions data)))

(defun javelin--write-javelin-positions (data)
  "Write DATA to the javelin JSON file.
DATA should be a list of alists with `javelin_position' and `filepath' keys.
Entries are sorted by `javelin_position' in increasing order before writing.
Creates the cache file if it does not exist.
If DATA is empty, deletes the cache file instead."
  (if (null data)
      (when (file-exists-p (javelin--cache-file-name))
        (delete-file (javelin--cache-file-name)))
    (javelin--ensure-cache-file)
    (write-region (json-serialize (vconcat (javelin--sort-positions data))) nil (javelin--cache-file-name))))

(defun javelin--get-filepath-by-position (javelin-position)
  "Get the filepath for a given JAVELIN-POSITION.
Returns nil if not found."
  (let ((data (javelin--read-javelin-positions)))
    (alist-get 'filepath
               (seq-find (lambda (item)
                           (= (alist-get 'javelin_position item) javelin-position))
                         data))))

(defun javelin--set-filepath-by-position (javelin-position filepath)
  "Set FILEPATH for a given JAVELIN-POSITION.
Updates existing entry or adds a new one."
  (let* ((data (javelin--read-javelin-positions))
         (existing (seq-find (lambda (item)
                               (= (alist-get 'javelin_position item) javelin-position))
                             data)))
    (if existing
        ;; Update existing entry
        (setf (alist-get 'filepath existing) filepath)
      ;; Add new entry
      (push `((javelin_position . ,javelin-position) (filepath . ,filepath)) data))
    (javelin--write-javelin-positions data)))

(defun javelin--remove-filepath-by-position (javelin-position)
  "Remove entry with JAVELIN-POSITION from the javelin list."
  (let ((data (javelin--read-javelin-positions)))
    (javelin--write-javelin-positions
     (seq-remove (lambda (item)
                   (= (alist-get 'javelin_position item) javelin-position))
                 data))))

(defun javelin--next-available-position ()
  "Get the next available javelin position.
Scans positions 1-9 and returns the first gap found.
Returns nil if all positions 1-9 are taken."
  (let ((data (javelin--read-javelin-positions)))
    (cl-loop for pos from 1 to 9
             unless (seq-find (lambda (item)
                                (= (alist-get 'javelin_position item) pos))
                              data)
             return pos)))

(defun javelin--buffer-filepath-relative-to-root ()
  "Get buffer file name relative to project root.
Returns the relative path if in a project, otherwise the absolute path.
For non-file buffers, returns the buffer name."
  (if-let ((filepath (buffer-file-name)))
      (let ((project-root (javelin--get-project-root)))
        (if project-root
            (file-relative-name filepath project-root)
          filepath))
    (buffer-name)))

;;; --- Go-to functions ---

;;;###autoload
(defun javelin-go-to (javelin-number)
  "Go to specific file or buffer on javelin by JAVELIN-NUMBER."
  (let* ((name (javelin--get-filepath-by-position javelin-number))
         (project-root (javelin--get-project-root))
         (full-file-name (when name
                           (if project-root
                               (concat project-root name)
                             name))))
    (cond
     ((null name)
      (message "No file javelined to position %d" javelin-number))
     ((file-exists-p full-file-name)
      (find-file full-file-name))
     ((get-buffer name)
      (switch-to-buffer name))
     (t
      (message "%s not found." name)))))

;;;###autoload
(defun javelin-go-to-1 ()
  "Go to file 1 on javelin."
  (interactive)
  (javelin-go-to 1))

;;;###autoload
(defun javelin-go-to-2 ()
  "Go to file 2 on javelin."
  (interactive)
  (javelin-go-to 2))

;;;###autoload
(defun javelin-go-to-3 ()
  "Go to file 3 on javelin."
  (interactive)
  (javelin-go-to 3))

;;;###autoload
(defun javelin-go-to-4 ()
  "Go to file 4 on javelin."
  (interactive)
  (javelin-go-to 4))

;;;###autoload
(defun javelin-go-to-5 ()
  "Go to file 5 on javelin."
  (interactive)
  (javelin-go-to 5))

;;;###autoload
(defun javelin-go-to-6 ()
  "Go to file 6 on javelin."
  (interactive)
  (javelin-go-to 6))

;;;###autoload
(defun javelin-go-to-7 ()
  "Go to file 7 on javelin."
  (interactive)
  (javelin-go-to 7))

;;;###autoload
(defun javelin-go-to-8 ()
  "Go to file 8 on javelin."
  (interactive)
  (javelin-go-to 8))

;;;###autoload
(defun javelin-go-to-9 ()
  "Go to file 9 on javelin."
  (interactive)
  (javelin-go-to 9))

;;; --- Go-to functions ---

;;; --- Delete functions ---

;;;###autoload
(defun javelin-delete (javelin-number)
  "Delete an item on javelin. JAVELIN-NUMBER: Position to delete."
  (javelin--remove-filepath-by-position javelin-number)
  (message "Deleted javelin position %d" javelin-number))

;;;###autoload
(defun javelin-delete-1 ()
  "Delete item javelin on position 1."
  (interactive)
  (javelin-delete 1))

;;;###autoload
(defun javelin-delete-2 ()
  "Delete item javelin on position 2."
  (interactive)
  (javelin-delete 2))

;;;###autoload
(defun javelin-delete-3 ()
  "Delete item javelin on position 3."
  (interactive)
  (javelin-delete 3))

;;;###autoload
(defun javelin-delete-4 ()
  "Delete item javelin on position 4."
  (interactive)
  (javelin-delete 4))

;;;###autoload
(defun javelin-delete-5 ()
  "Delete item javelin on position 5."
  (interactive)
  (javelin-delete 5))

;;;###autoload
(defun javelin-delete-6 ()
  "Delete item javelin on position 6."
  (interactive)
  (javelin-delete 6))

;;;###autoload
(defun javelin-delete-7 ()
  "Delete item javelin on position 7."
  (interactive)
  (javelin-delete 7))

;;;###autoload
(defun javelin-delete-8 ()
  "Delete item javelin on position 8."
  (interactive)
  (javelin-delete 8))

;;;###autoload
(defun javelin-delete-9 ()
  "Delete item javelin on position 9."
  (interactive)
  (javelin-delete 9))

;;; --- Delete functions ---

;;; --- Assign to functions ---

;;;###autoload
(defun javelin-assign-to (javelin-number)
  "Assign the current buffer to a specific position in javelin.
JAVELIN-NUMBER: The position (1-9) to assign the current file to."
  (let ((file-to-add (javelin--buffer-filepath-relative-to-root)))
    (javelin--set-filepath-by-position javelin-number file-to-add)
    (message "Assigned %s to javelin position %d" file-to-add javelin-number)))

;;;###autoload
(defun javelin-assign-to-1 ()
  "Assign current buffer to position 1 on javelin."
  (interactive)
  (javelin-assign-to 1))

;;;###autoload
(defun javelin-assign-to-2 ()
  "Assign current buffer to position 2 on javelin."
  (interactive)
  (javelin-assign-to 2))

;;;###autoload
(defun javelin-assign-to-3 ()
  "Assign current buffer to position 3 on javelin."
  (interactive)
  (javelin-assign-to 3))

;;;###autoload
(defun javelin-assign-to-4 ()
  "Assign current buffer to position 4 on javelin."
  (interactive)
  (javelin-assign-to 4))

;;;###autoload
(defun javelin-assign-to-5 ()
  "Assign current buffer to position 5 on javelin."
  (interactive)
  (javelin-assign-to 5))

;;;###autoload
(defun javelin-assign-to-6 ()
  "Assign current buffer to position 6 on javelin."
  (interactive)
  (javelin-assign-to 6))

;;;###autoload
(defun javelin-assign-to-7 ()
  "Assign current buffer to position 7 on javelin."
  (interactive)
  (javelin-assign-to 7))

;;;###autoload
(defun javelin-assign-to-8 ()
  "Assign current buffer to position 8 on javelin."
  (interactive)
  (javelin-assign-to 8))

;;;###autoload
(defun javelin-assign-to-9 ()
  "Assign current buffer to position 9 on javelin."
  (interactive)
  (javelin-assign-to 9))

;;; --- Assign to functions ---

;;;###autoload
(defun javelin-go-or-assign-to (javelin-number &optional force)
  "Go to javelin position if occupied, otherwise assign current buffer to it.
JAVELIN-NUMBER: The position (1-9) to go to or assign.
With FORCE (or prefix arg \\[universal-argument]), always assign even if position is occupied."
  (if (and (not force) (javelin--get-filepath-by-position javelin-number))
      (javelin-go-to javelin-number)
    (javelin-assign-to javelin-number)))

;;; --- Go or assign to functions ---

;;;###autoload
(defun javelin-go-or-assign-to-1 ()
  "Go to position 1 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 1 current-prefix-arg))

;;;###autoload
(defun javelin-go-or-assign-to-2 ()
  "Go to position 2 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 2 current-prefix-arg))

;;;###autoload
(defun javelin-go-or-assign-to-3 ()
  "Go to position 3 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 3 current-prefix-arg))

;;;###autoload
(defun javelin-go-or-assign-to-4 ()
  "Go to position 4 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 4 current-prefix-arg))

;;;###autoload
(defun javelin-go-or-assign-to-5 ()
  "Go to position 5 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 5 current-prefix-arg))

;;;###autoload
(defun javelin-go-or-assign-to-6 ()
  "Go to position 6 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 6 current-prefix-arg))

;;;###autoload
(defun javelin-go-or-assign-to-7 ()
  "Go to position 7 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 7 current-prefix-arg))

;;;###autoload
(defun javelin-go-or-assign-to-8 ()
  "Go to position 8 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 8 current-prefix-arg))

;;;###autoload
(defun javelin-go-or-assign-to-9 ()
  "Go to position 9 if occupied, otherwise assign current buffer to it.
With prefix arg \\[universal-argument], always assign even if position is occupied."
  (interactive)
  (javelin-go-or-assign-to 9 current-prefix-arg))

;;; --- Go or assign to functions ---

;;;###autoload
(defun javelin-go-to-next ()
  "Go to the next file in javelin."
  (interactive)
  (let* ((data (javelin--read-javelin-positions))
         (files (mapcar (lambda (item) (alist-get 'filepath item)) data))
         (current-file (javelin--buffer-filepath-relative-to-root))
         (current-index (or (cl-position current-file files :test 'string=) -1))
         (next-index (mod (+ current-index 1) (length files)))
         (next-item (nth next-index data)))
    (when next-item
      (javelin-go-to (alist-get 'javelin_position next-item)))))

;;;###autoload
(defun javelin-go-to-prev ()
  "Go to the previous file in javelin."
  (interactive)
  (let* ((data (javelin--read-javelin-positions))
         (files (mapcar (lambda (item) (alist-get 'filepath item)) data))
         (current-file (javelin--buffer-filepath-relative-to-root))
         (current-index (or (cl-position current-file files :test 'string=) -1))
         (prev-index (mod (+ current-index (length files) -1) (length files)))
         (prev-item (nth prev-index data)))
    (when prev-item
      (javelin-go-to (alist-get 'javelin_position prev-item)))))

;;;###autoload
(defun javelin-add-file ()
  "Add current file to javelin."
  (interactive)
  (let* ((file-to-add (javelin--buffer-filepath-relative-to-root))
         (data (javelin--read-javelin-positions))
         (existing (seq-find (lambda (item)
                               (string= (alist-get 'filepath item) file-to-add))
                             data)))
    (if existing
        (message "This file is already on javelin.")
      (let ((next-num (javelin--next-available-position)))
        (javelin--set-filepath-by-position next-num file-to-add)
        (message "File added to javelin at position %d." next-num)))))

;;;###autoload
(defun javelin-toggle-quick-menu ()
  "Open quick menu to select a javelined file."
  (interactive)
  (let* ((data (javelin--read-javelin-positions))
         (candidates (mapcar (lambda (item)
                               (cons (format "%d: %s"
                                             (alist-get 'javelin_position item)
                                             (alist-get 'filepath item))
                                     item))
                             data))
         (candidate-strings (mapcar #'car candidates))
         ;; Completion table that preserves order (disables sorting)
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata (display-sort-function . identity)
                             (cycle-sort-function . identity))
                         (complete-with-action action candidate-strings string pred)))))
    (when-let ((selection (completing-read "Javelin to file: " collection)))
      (javelin-go-to (alist-get 'javelin_position (cdr (assoc selection candidates)))))))

;;;###autoload
(defun javelin-clear ()
  "Clear all javelin positions."
  (interactive)
  (when (yes-or-no-p "Do you really want to clear javelin all javelin positions?")
    (javelin--write-javelin-positions '())
    (message "Javelin positions cleaned.")))

;;;###autoload
(defun javelin-clear-all ()
  "Delete all javelin position namespaces."
  (interactive)
  (when (yes-or-no-p "Do you really want to clear all javelin positions across all javelin projects? ")
    (let ((files (directory-files javelin-cache-dir t "\\.json$")))
      (dolist (file files)
        (delete-file file))
      (message "Deleted all javelin positions across %d project(s)." (length files)))))

;;; --- Minor mode ---

(defvar javelin-minor-mode-map
  (let ((map (make-sparse-keymap))
        (delete-map (make-sparse-keymap)))
    ;; Go or assign to position
    (define-key map (kbd "M-1") #'javelin-go-or-assign-to-1)
    (define-key map (kbd "M-2") #'javelin-go-or-assign-to-2)
    (define-key map (kbd "M-3") #'javelin-go-or-assign-to-3)
    (define-key map (kbd "M-4") #'javelin-go-or-assign-to-4)
    (define-key map (kbd "M-5") #'javelin-go-or-assign-to-5)
    (define-key map (kbd "M-6") #'javelin-go-or-assign-to-6)
    (define-key map (kbd "M-7") #'javelin-go-or-assign-to-7)
    (define-key map (kbd "M-8") #'javelin-go-or-assign-to-8)
    (define-key map (kbd "M-9") #'javelin-go-or-assign-to-9)
    ;; Delete position (M-0 prefix)
    (define-key delete-map (kbd "0") #'javelin-clear)
    (define-key delete-map (kbd "1") #'javelin-delete-1)
    (define-key delete-map (kbd "2") #'javelin-delete-2)
    (define-key delete-map (kbd "3") #'javelin-delete-3)
    (define-key delete-map (kbd "4") #'javelin-delete-4)
    (define-key delete-map (kbd "5") #'javelin-delete-5)
    (define-key delete-map (kbd "6") #'javelin-delete-6)
    (define-key delete-map (kbd "7") #'javelin-delete-7)
    (define-key delete-map (kbd "8") #'javelin-delete-8)
    (define-key delete-map (kbd "9") #'javelin-delete-9)
    (define-key map (kbd "M-0") delete-map)
    ;; Quick menu
    (define-key map (kbd "M--") #'javelin-toggle-quick-menu)
    map)
  "Keymap for `javelin-minor-mode'.")

;;;###autoload
(define-minor-mode javelin-minor-mode
  "Minor mode for quick file navigation with javelin.
Provides keybindings for jumping to javelined files:
  M-1 to M-9: Go to position (or assign if empty, \\[universal-argument] to force assign)
  M-0 0: Clear all positions
  M-0 1-9: Delete position
  M--: Toggle quick menu"
  :lighter " Harp"
  :keymap javelin-minor-mode-map
  :global nil)

;;;###autoload
(define-globalized-minor-mode global-javelin-minor-mode
  javelin-minor-mode
  (lambda () (javelin-minor-mode 1))
  :group 'javelin)

(provide 'javelin)
;;; javelin.el ends here
