;;; harpoon.el --- Bookmarks on steroids    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Otávio Schwanck, 2025 Damian Barabonkov

;; Author: Damian Barabonkov
;; Keywords: tools languages
;; Homepage: https://github.com/DamianB-BitFlipper/harpoon.el
;; Based on: https://github.com/otavioschwanck/harpoon.el
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (f "0.20.0"))

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
;; It is a buffer quick-bookmark manager on steroids. Each "bookmark" is called a harpoon
;; position, and you can easily assign and delete buffers to harpoon positions. Harpoon
;; positions are namespaced by project and branch if within a git repository. Otherwise,
;; just by project if not within git. Globally namespaced otherwise.

(require 'f)
(require 'subr-x)

;;; --- Customizable variables ---
(defgroup harpoon nil
  "Organize bookmarks by project and branch."
  :group 'tools)

(defcustom harpoon-cache-dir (concat user-emacs-directory ".local/harpoon/")
  "Where the cache will be saved."
  :type 'string)

(defcustom harpoon-separate-by-branch t
  "Harpoon separated by branch."
  :type 'boolean)

(defcustom harpoon-default-positions-namespace "harpoon"
  "Namespace for harpoon positions when not in a project.
Used as a fallback identifier for the cache file when harpoon cannot
detect a project root. The namespace helps isolate harpoon positions
for different contexts."
  :type 'string)
;;; --- Customizable variables ---

(defvar harpoon-project-provider (if (featurep 'projectile) 'projectile 'project)
  "Project provider to use for getting project root.
Can be `projectile' or `project'.")

(defvar harpoon-git-branch-provider (if (featurep 'magit) 'magit 'git)
  "Git branch provider to use for getting current branch.
Can be `magit' or `git'.")

(defun harpoon--get-project-root ()
  "Get the project root.
Returns the project root path as a string, or nil if there is no project."
  (cond
   ((eq harpoon-project-provider 'projectile) (projectile-project-root))
   ((eq harpoon-project-provider 'project)
    (when-let ((proj (project-current)))
      (expand-file-name (project-root proj))))))

(defun harpoon--get-project-name ()
  "Get the harpoon project name.
Returns the project name as a string, or nil if there is no project."
  (when (harpoon--get-project-root)
    (cond
     ((eq harpoon-project-provider 'projectile) (projectile-project-name))
     ;; The `project' package has no built-in way to get the project name.
     ;; Extract it by splitting the project root path by "/" and taking
     ;; the second-to-last element (the directory name before the trailing slash).
     ;; e.g., "/home/user/projects/myproject/" -> "myproject"
     ((eq harpoon-project-provider 'project)
      (let* ((project-path (harpoon--get-project-root))
             (path-parts (split-string project-path "/"))
             (name-index (- (length path-parts) 2)))
        (nth name-index path-parts))))))

(defun harpoon--get-branch-name ()
  "Get the branch name for harpoon.
Uses `harpoon-git-branch-provider' to determine the method.
Returns nil if not in a git repository."
  (cond
   ((eq harpoon-git-branch-provider 'magit)
    (magit-get-current-branch))
   ((eq harpoon-git-branch-provider 'git)
    (condition-case nil
        (string-trim
         (shell-command-to-string
          (concat "cd " (harpoon--get-project-root) "; git rev-parse --abbrev-ref HEAD")))
      (error nil)))))

(defun harpoon--cache-key ()
  "Key to save current file on cache.
Returns `harpoon-default-positions-namespace' if there is no project."
  ;; Use `url-hexify-string' to percent-encode the cache key, making it
  ;; filename-friendly by escaping "/" and other forbidden characters.
  (let ((project-name (or (harpoon--get-project-name) harpoon-default-positions-namespace)))
    (url-hexify-string
     (if-let ((branch (and harpoon-separate-by-branch (harpoon--get-branch-name))))
         (concat project-name "#" branch)
       project-name))))

(defun harpoon--cache-file-name ()
  "File name for harpoon on current project."
  (concat harpoon-cache-dir (harpoon--cache-key) ".json"))

(defun harpoon--ensure-cache-file ()
  "Create harpoon cache dir and file if they don't exist."
  (unless (f-directory? harpoon-cache-dir)
    (make-directory harpoon-cache-dir t))
  (unless (file-exists-p (harpoon--cache-file-name))
    (f-write-text (json-serialize '()) 'utf-8 (harpoon--cache-file-name))))

(defun harpoon--sort-positions (data)
  "Sort DATA by `harpoon_position' in increasing order."
  (seq-sort (lambda (a b)
              (< (alist-get 'harpoon_position a)
                 (alist-get 'harpoon_position b)))
            data))

(defun harpoon--read-harpoon-positions ()
  "Read and parse the harpoon positions cache JSON file.
Returns a list of alists with `harpoon_position' and `filepath' keys."
  (let ((data (if (file-exists-p (harpoon--cache-file-name))
                  (condition-case nil
                      (json-parse-string (f-read (harpoon--cache-file-name) 'utf-8)
                                         :object-type 'alist
                                         :array-type 'list)
                    ;; JSON file got corrupted, delete it and return empty list
                    (error
                     (delete-file (harpoon--cache-file-name))
                     '()))
                '())))
    (harpoon--sort-positions data)))

(defun harpoon--write-harpoon-positions (data)
  "Write DATA to the harpoon JSON file.
DATA should be a list of alists with `harpoon_position' and `filepath' keys.
Entries are sorted by `harpoon_position' in increasing order before writing.
Creates the cache file if it does not exist.
If DATA is empty, deletes the cache file instead."
  (if (null data)
      (when (file-exists-p (harpoon--cache-file-name))
        (delete-file (harpoon--cache-file-name)))
    (harpoon--ensure-cache-file)
    (f-write-text (json-serialize (vconcat (harpoon--sort-positions data))) 'utf-8 (harpoon--cache-file-name))))

(defun harpoon--get-filepath-by-position (harpoon-position)
  "Get the filepath for a given HARPOON-POSITION.
Returns nil if not found."
  (let ((data (harpoon--read-harpoon-positions)))
    (alist-get 'filepath
               (seq-find (lambda (item)
                           (= (alist-get 'harpoon_position item) harpoon-position))
                         data))))

(defun harpoon--set-filepath-by-position (harpoon-position filepath)
  "Set FILEPATH for a given HARPOON-POSITION.
Updates existing entry or adds a new one."
  (let* ((data (harpoon--read-harpoon-positions))
         (existing (seq-find (lambda (item)
                               (= (alist-get 'harpoon_position item) harpoon-position))
                             data)))
    (if existing
        ;; Update existing entry
        (setf (alist-get 'filepath existing) filepath)
      ;; Add new entry
      (push `((harpoon_position . ,harpoon-position) (filepath . ,filepath)) data))
    (harpoon--write-harpoon-positions data)))

(defun harpoon--remove-filepath-by-position (harpoon-position)
  "Remove entry with HARPOON-POSITION from the harpoon list."
  (let ((data (harpoon--read-harpoon-positions)))
    (harpoon--write-harpoon-positions
     (seq-remove (lambda (item)
                   (= (alist-get 'harpoon_position item) harpoon-position))
                 data))))

(defun harpoon--next-available-position ()
  "Get the next available harpoon position.
Scans positions 1-9 and returns the first gap found.
Returns nil if all positions 1-9 are taken."
  (let ((data (harpoon--read-harpoon-positions)))
    (cl-loop for pos from 1 to 9
             unless (seq-find (lambda (item)
                                (= (alist-get 'harpoon_position item) pos))
                              data)
             return pos)))

(defun harpoon--buffer-filepath-relative-to-root ()
  "Get buffer file name relative to project root.
Returns the relative path if in a project, otherwise the absolute path."
  (let ((project-root (harpoon--get-project-root)))
    (if project-root
        (file-relative-name (buffer-file-name) project-root)
      (buffer-file-name))))

;;; --- Go-to functions ---

;;;###autoload
(defun harpoon-go-to (harpoon-number)
  "Go to specific file on harpoon by HARPOON-NUMBER."
  (let* ((file-name (harpoon--get-filepath-by-position harpoon-number))
         (project-root (harpoon--get-project-root))
         (full-file-name (when file-name
                           (if project-root
                               (concat project-root file-name)
                             file-name))))
    (cond
     ((null file-name)
      (message "No file harpooned to position %d" harpoon-number))
     ((file-exists-p full-file-name)
      (find-file full-file-name))
     (t
      (message "%s not found." full-file-name)))))

;;;###autoload
(defun harpoon-go-to-1 ()
  "Go to file 1 on harpoon."
  (interactive)
  (harpoon-go-to 1))

;;;###autoload
(defun harpoon-go-to-2 ()
  "Go to file 2 on harpoon."
  (interactive)
  (harpoon-go-to 2))

;;;###autoload
(defun harpoon-go-to-3 ()
  "Go to file 3 on harpoon."
  (interactive)
  (harpoon-go-to 3))

;;;###autoload
(defun harpoon-go-to-4 ()
  "Go to file 4 on harpoon."
  (interactive)
  (harpoon-go-to 4))

;;;###autoload
(defun harpoon-go-to-5 ()
  "Go to file 5 on harpoon."
  (interactive)
  (harpoon-go-to 5))

;;;###autoload
(defun harpoon-go-to-6 ()
  "Go to file 6 on harpoon."
  (interactive)
  (harpoon-go-to 6))

;;;###autoload
(defun harpoon-go-to-7 ()
  "Go to file 7 on harpoon."
  (interactive)
  (harpoon-go-to 7))

;;;###autoload
(defun harpoon-go-to-8 ()
  "Go to file 8 on harpoon."
  (interactive)
  (harpoon-go-to 8))

;;;###autoload
(defun harpoon-go-to-9 ()
  "Go to file 9 on harpoon."
  (interactive)
  (harpoon-go-to 9))

;;; --- Go-to functions ---

;;; --- Delete functions ---

;;;###autoload
(defun harpoon-delete (harpoon-number)
  "Delete an item on harpoon. HARPOON-NUMBER: Position to delete."
  (harpoon--remove-filepath-by-position harpoon-number)
  (message "Deleted harpoon position %d" harpoon-number))

;;;###autoload
(defun harpoon-delete-1 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon-delete 1))

;;;###autoload
(defun harpoon-delete-2 ()
  "Delete item harpoon on position 2."
  (interactive)
  (harpoon-delete 2))

;;;###autoload
(defun harpoon-delete-3 ()
  "Delete item harpoon on position 3."
  (interactive)
  (harpoon-delete 3))

;;;###autoload
(defun harpoon-delete-4 ()
  "Delete item harpoon on position 4."
  (interactive)
  (harpoon-delete 4))

;;;###autoload
(defun harpoon-delete-5 ()
  "Delete item harpoon on position 5."
  (interactive)
  (harpoon-delete 5))

;;;###autoload
(defun harpoon-delete-6 ()
  "Delete item harpoon on position 6."
  (interactive)
  (harpoon-delete 6))

;;;###autoload
(defun harpoon-delete-7 ()
  "Delete item harpoon on position 7."
  (interactive)
  (harpoon-delete 7))

;;;###autoload
(defun harpoon-delete-8 ()
  "Delete item harpoon on position 8."
  (interactive)
  (harpoon-delete 8))

;;;###autoload
(defun harpoon-delete-9 ()
  "Delete item harpoon on position 9."
  (interactive)
  (harpoon-delete 9))

;;; --- Delete functions ---

;;; --- Assign to functions ---

;;;###autoload
(defun harpoon-assign-to (harpoon-number)
  "Assign the current buffer to a specific position in harpoon.
HARPOON-NUMBER: The position (1-9) to assign the current file to."
  (let ((file-to-add (harpoon--buffer-filepath-relative-to-root)))
    (harpoon--set-filepath-by-position harpoon-number file-to-add)
    (message "Assigned %s to harpoon position %d" file-to-add harpoon-number)))

;;;###autoload
(defun harpoon-assign-to-1 ()
  "Assign current buffer to position 1 on harpoon."
  (interactive)
  (harpoon-assign-to 1))

;;;###autoload
(defun harpoon-assign-to-2 ()
  "Assign current buffer to position 2 on harpoon."
  (interactive)
  (harpoon-assign-to 2))

;;;###autoload
(defun harpoon-assign-to-3 ()
  "Assign current buffer to position 3 on harpoon."
  (interactive)
  (harpoon-assign-to 3))

;;;###autoload
(defun harpoon-assign-to-4 ()
  "Assign current buffer to position 4 on harpoon."
  (interactive)
  (harpoon-assign-to 4))

;;;###autoload
(defun harpoon-assign-to-5 ()
  "Assign current buffer to position 5 on harpoon."
  (interactive)
  (harpoon-assign-to 5))

;;;###autoload
(defun harpoon-assign-to-6 ()
  "Assign current buffer to position 6 on harpoon."
  (interactive)
  (harpoon-assign-to 6))

;;;###autoload
(defun harpoon-assign-to-7 ()
  "Assign current buffer to position 7 on harpoon."
  (interactive)
  (harpoon-assign-to 7))

;;;###autoload
(defun harpoon-assign-to-8 ()
  "Assign current buffer to position 8 on harpoon."
  (interactive)
  (harpoon-assign-to 8))

;;;###autoload
(defun harpoon-assign-to-9 ()
  "Assign current buffer to position 9 on harpoon."
  (interactive)
  (harpoon-assign-to 9))

;;; --- Assign to functions ---

;;;###autoload
(defun harpoon-go-or-assign-to (harpoon-number)
  "Go to harpoon position if occupied, otherwise assign current buffer to it.
HARPOON-NUMBER: The position (1-9) to go to or assign."
  (if (harpoon--get-filepath-by-position harpoon-number)
      (harpoon-go-to harpoon-number)
    (harpoon-assign-to harpoon-number)))

;;; --- Go or assign to functions ---

;;;###autoload
(defun harpoon-go-or-assign-to-1 ()
  "Go to position 1 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 1))

;;;###autoload
(defun harpoon-go-or-assign-to-2 ()
  "Go to position 2 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 2))

;;;###autoload
(defun harpoon-go-or-assign-to-3 ()
  "Go to position 3 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 3))

;;;###autoload
(defun harpoon-go-or-assign-to-4 ()
  "Go to position 4 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 4))

;;;###autoload
(defun harpoon-go-or-assign-to-5 ()
  "Go to position 5 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 5))

;;;###autoload
(defun harpoon-go-or-assign-to-6 ()
  "Go to position 6 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 6))

;;;###autoload
(defun harpoon-go-or-assign-to-7 ()
  "Go to position 7 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 7))

;;;###autoload
(defun harpoon-go-or-assign-to-8 ()
  "Go to position 8 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 8))

;;;###autoload
(defun harpoon-go-or-assign-to-9 ()
  "Go to position 9 if occupied, otherwise assign current buffer to it."
  (interactive)
  (harpoon-go-or-assign-to 9))

;;; --- Go or assign to functions ---

;;;###autoload
(defun harpoon-go-to-next ()
  "Go to the next file in harpoon."
  (interactive)
  (let* ((data (harpoon--read-harpoon-positions))
         (files (mapcar (lambda (item) (alist-get 'filepath item)) data))
         (current-file (harpoon--buffer-filepath-relative-to-root))
         (current-index (or (cl-position current-file files :test 'string=) -1))
         (next-index (mod (+ current-index 1) (length files)))
         (next-item (nth next-index data)))
    (when next-item
      (harpoon-go-to (alist-get 'harpoon_position next-item)))))

;;;###autoload
(defun harpoon-go-to-prev ()
  "Go to the previous file in harpoon."
  (interactive)
  (let* ((data (harpoon--read-harpoon-positions))
         (files (mapcar (lambda (item) (alist-get 'filepath item)) data))
         (current-file (harpoon--buffer-filepath-relative-to-root))
         (current-index (or (cl-position current-file files :test 'string=) -1))
         (prev-index (mod (+ current-index (length files) -1) (length files)))
         (prev-item (nth prev-index data)))
    (when prev-item
      (harpoon-go-to (alist-get 'harpoon_position prev-item)))))

;;;###autoload
(defun harpoon-add-file ()
  "Add current file to harpoon."
  (interactive)
  (let* ((file-to-add (harpoon--buffer-filepath-relative-to-root))
         (data (harpoon--read-harpoon-positions))
         (existing (seq-find (lambda (item)
                               (string= (alist-get 'filepath item) file-to-add))
                             data)))
    (if existing
        (message "This file is already on harpoon.")
      (let ((next-num (harpoon--next-available-position)))
        (harpoon--set-filepath-by-position next-num file-to-add)
        (message "File added to harpoon at position %d." next-num)))))

;;;###autoload
(defun harpoon-toggle-quick-menu ()
  "Open quick menu to select a harpooned file."
  (interactive)
  (let* ((data (harpoon--read-harpoon-positions))
         (candidates (mapcar (lambda (item)
                               (cons (format "%d: %s"
                                             (alist-get 'harpoon_position item)
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
    (when-let ((selection (completing-read "Harpoon to file: " collection)))
      (harpoon-go-to (alist-get 'harpoon_position (cdr (assoc selection candidates)))))))

;;;###autoload
(defun harpoon-clear ()
  "Clear all harpoon positions."
  (interactive)
  (when (yes-or-no-p "Do you really want to clear harpoon all harpoon positions? ")
    (harpoon--write-harpoon-positions '())
    (message "Harpoon positions cleaned.")))

;;;###autoload
(defun harpoon-clear-all ()
  "Delete all harpoon position namespaces."
  (interactive)
  (when (yes-or-no-p "Do you really want to clear all harpoon positions across all harpoon projects? ")
    (let ((files (directory-files harpoon-cache-dir t "\\.json$")))
      (dolist (file files)
        (delete-file file))
      (message "Deleted all harpoon positions across %d project(s)." (length files)))))

(provide 'harpoon)
;;; harpoon.el ends here
