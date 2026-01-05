;;; javelin.el --- Implementation of harpoon: bookmarks on steroids -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Damian Barabonkov
;; Based on harpoon.el, Copyright (C) 2022 Otávio Schwanck

;; Author: Damian Barabonkov
;; Keywords: tools languages
;; Homepage: https://github.com/DamianB-BitFlipper/javelin.el
;; Version: 0.2.3
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
(require 'bookmark)
(require 'vc-git)


;;; Code:

;;; --- Customizable variables ---

(defgroup javelin nil
  "Organize bookmarks by project and branch."
  :group 'tools)

(defcustom javelin-separate-by-branch t
  "Javelin separated by branch."
  :type 'boolean)

(defcustom javelin-default-positions-namespace "javelin"
  "Namespace for javelin positions when not in a project.
Used as a fallback identifier when javelin cannot detect a project root.
The namespace helps isolate javelin positions for different contexts."
  :type 'string)

(defcustom javelin-disable-confirmation nil
  "If non-nil, skip confirmation prompts for destructive actions."
  :type 'boolean)
;;; --- Customizable variables ---

(defconst javelin--bookmark-prefix "javelin:"
  "Prefix for javelin bookmark names.")

(defvar javelin--bookmarks-loaded nil
  "Non-nil when javelin has loaded bookmark data.")

(defun javelin--ensure-bookmarks-loaded ()
  "Load the default bookmark file once if it exists."
  (unless javelin--bookmarks-loaded
    (setq javelin--bookmarks-loaded t)
    (when (and (boundp 'bookmark-default-file)
               (file-exists-p bookmark-default-file))
      (bookmark-load bookmark-default-file t))))

(defun javelin--bookmark-save-silent ()
  "Save bookmarks without emitting messages."
  (let ((inhibit-message t))
    (bookmark-save)))

(defun javelin--get-project-root ()
  "Get the project root.
Returns the project root path as a string, or nil if there is no project."
  (when-let ((proj (project-current)))
    (expand-file-name (project-root proj))))

(defun javelin--get-project-name ()
  "Get the javelin project name.
Returns the project name as a string, or nil if there is no project."
  (when (javelin--get-project-root)
    ;; The `project' package has no built-in way to get the project name.
    ;; Extract it by splitting the project root path by "/" and taking
    ;; the second-to-last element (the directory name before the trailing slash).
    ;; e.g., "/home/user/projects/myproject/" -> "myproject"
    (let* ((project-path (javelin--get-project-root))
           (path-parts (split-string project-path "/"))
           (name-index (- (length path-parts) 2)))
      (nth name-index path-parts))))

(defun javelin--get-branch-name ()
  "Get the branch name for javelin.
Returns nil if not in a git repository."
  (car (vc-git-branches)))

(defun javelin--bookmark-namespace ()
  "Namespace for javelin bookmark names."
  (let ((project-name (or (javelin--get-project-name) javelin-default-positions-namespace)))
    (if-let ((branch (and javelin-separate-by-branch (javelin--get-branch-name))))
        (format "%s#%s" project-name branch)
      project-name)))

(defun javelin--bookmark-name (javelin-position)
  "Bookmark name for JAVELIN-POSITION."
  (format "%s%s:%d" javelin--bookmark-prefix (javelin--bookmark-namespace) javelin-position))

(defun javelin--bookmark-positions ()
  "Return list of (position . bookmark-name) in the current namespace."
  (javelin--ensure-bookmarks-loaded)
  (let* ((namespace (javelin--bookmark-namespace))
         (pattern (format "^%s%s:\\([1-9]\\)$"
                          (regexp-quote javelin--bookmark-prefix)
                          (regexp-quote namespace)))
         (positions nil))
    (dolist (bm bookmark-alist)
      (let ((name (car bm)))
        (when (string-match pattern name)
          (push (cons (string-to-number (match-string 1 name)) name) positions))))
    (seq-sort-by #'car #'< positions)))

(defun javelin--position-for-current-buffer ()
  "Return the javelin position for the current buffer, or nil."
  (let ((current-file (buffer-file-name))
        (current-buffer-name (buffer-name))
        (positions (javelin--bookmark-positions))
        found)
    (dolist (pair positions)
      (let* ((record (bookmark-get-bookmark (cdr pair) 'noerror))
             (file (and record (bookmark-get-filename record)))
             (buffer-name (and record (bookmark-prop-get record 'buffer-name))))
        (when (or (and file current-file
                       (string= (expand-file-name file) (expand-file-name current-file)))
                  (and (null file) buffer-name (string= buffer-name current-buffer-name)))
          (setq found (car pair)))))
    found))

(defun javelin--bookmark-display-target (record)
  "Return a display-friendly target for RECORD."
  (let* ((file (bookmark-get-filename record))
         (buffer-name (bookmark-prop-get record 'buffer-name))
         (project-root (javelin--get-project-root)))
    (cond
     (file
      (if project-root
          (file-relative-name file project-root)
        file))
     (buffer-name buffer-name)
     (t "<unknown>"))))

;;; --- Go-to functions ---

;;;###autoload
(defun javelin-go-to (javelin-number)
  "Go to specific file or buffer of the javelin by JAVELIN-NUMBER."
  (interactive "nJavelin position: ")
  (javelin--ensure-bookmarks-loaded)
  (let ((name (javelin--bookmark-name javelin-number)))
    (if (bookmark-get-bookmark name 'noerror)
        (bookmark-jump name)
      (message "No file javelined to position %d" javelin-number))))

;;;###autoload
(defun javelin-go-to-1 ()
  "Go to the buffer at position 1 of the javelin."
  (interactive)
  (javelin-go-to 1))

;;;###autoload
(defun javelin-go-to-2 ()
  "Go to the buffer at position 2 of the javelin."
  (interactive)
  (javelin-go-to 2))

;;;###autoload
(defun javelin-go-to-3 ()
  "Go to the buffer at position 3 of the javelin."
  (interactive)
  (javelin-go-to 3))

;;;###autoload
(defun javelin-go-to-4 ()
  "Go to the buffer at position 4 of the javelin."
  (interactive)
  (javelin-go-to 4))

;;;###autoload
(defun javelin-go-to-5 ()
  "Go to the buffer at position 5 of the javelin."
  (interactive)
  (javelin-go-to 5))

;;;###autoload
(defun javelin-go-to-6 ()
  "Go to the buffer at position 6 of the javelin."
  (interactive)
  (javelin-go-to 6))

;;;###autoload
(defun javelin-go-to-7 ()
  "Go to the buffer at position 7 of the javelin."
  (interactive)
  (javelin-go-to 7))

;;;###autoload
(defun javelin-go-to-8 ()
  "Go to the buffer at position 8 of the javelin."
  (interactive)
  (javelin-go-to 8))

;;;###autoload
(defun javelin-go-to-9 ()
  "Go to the buffer at position 9 of the javelin."
  (interactive)
  (javelin-go-to 9))

;;; --- Delete functions ---

;;;###autoload
(defun javelin-delete (javelin-number)
  "Delete an item of the javelin. JAVELIN-NUMBER: Position to delete."
  (interactive "nJavelin position: ")
  (javelin--ensure-bookmarks-loaded)
  (let ((name (javelin--bookmark-name javelin-number)))
    (if (bookmark-get-bookmark name 'noerror)
        (progn
          (bookmark-delete name)
          (javelin--bookmark-save-silent)
          (message "Deleted javelin position %d" javelin-number))
      (message "No javelin position %d to delete" javelin-number))))

;;;###autoload
(defun javelin-delete-1 ()
  "Delete position 1 of the javelin."
  (interactive)
  (javelin-delete 1))

;;;###autoload
(defun javelin-delete-2 ()
  "Delete position 2 of the javelin."
  (interactive)
  (javelin-delete 2))

;;;###autoload
(defun javelin-delete-3 ()
  "Delete position 3 of the javelin."
  (interactive)
  (javelin-delete 3))

;;;###autoload
(defun javelin-delete-4 ()
  "Delete position 4 of the javelin."
  (interactive)
  (javelin-delete 4))

;;;###autoload
(defun javelin-delete-5 ()
  "Delete position 5 of the javelin."
  (interactive)
  (javelin-delete 5))

;;;###autoload
(defun javelin-delete-6 ()
  "Delete position 6 of the javelin."
  (interactive)
  (javelin-delete 6))

;;;###autoload
(defun javelin-delete-7 ()
  "Delete position 7 of the javelin."
  (interactive)
  (javelin-delete 7))

;;;###autoload
(defun javelin-delete-8 ()
  "Delete position 8 of the javelin."
  (interactive)
  (javelin-delete 8))

;;;###autoload
(defun javelin-delete-9 ()
  "Delete position 9 of the javelin."
  (interactive)
  (javelin-delete 9))

;;; --- Assign to functions ---

;;;###autoload
(defun javelin-assign-to (javelin-number)
  "Assign the current buffer to a specific position in javelin.
JAVELIN-NUMBER: The position (1-9) to assign the current buffer to."
  (interactive "nJavelin position: ")
  (javelin--ensure-bookmarks-loaded)
  (bookmark-set (javelin--bookmark-name javelin-number))
  (javelin--bookmark-save-silent)
  (message "Assigned current buffer to javelin position %d" javelin-number))

;;;###autoload
(defun javelin-assign-to-1 ()
  "Assign current buffer to position 1 of the javelin."
  (interactive)
  (javelin-assign-to 1))

;;;###autoload
(defun javelin-assign-to-2 ()
  "Assign current buffer to position 2 of the javelin."
  (interactive)
  (javelin-assign-to 2))

;;;###autoload
(defun javelin-assign-to-3 ()
  "Assign current buffer to position 3 of the javelin."
  (interactive)
  (javelin-assign-to 3))

;;;###autoload
(defun javelin-assign-to-4 ()
  "Assign current buffer to position 4 of the javelin."
  (interactive)
  (javelin-assign-to 4))

;;;###autoload
(defun javelin-assign-to-5 ()
  "Assign current buffer to position 5 of the javelin."
  (interactive)
  (javelin-assign-to 5))

;;;###autoload
(defun javelin-assign-to-6 ()
  "Assign current buffer to position 6 of the javelin."
  (interactive)
  (javelin-assign-to 6))

;;;###autoload
(defun javelin-assign-to-7 ()
  "Assign current buffer to position 7 of the javelin."
  (interactive)
  (javelin-assign-to 7))

;;;###autoload
(defun javelin-assign-to-8 ()
  "Assign current buffer to position 8 of the javelin."
  (interactive)
  (javelin-assign-to 8))

;;;###autoload
(defun javelin-assign-to-9 ()
  "Assign current buffer to position 9 of the javelin."
  (interactive)
  (javelin-assign-to 9))

;;;###autoload
(defun javelin-go-or-assign-to (javelin-number &optional force)
  "Go to javelin position if occupied, otherwise assign current buffer to it.
JAVELIN-NUMBER: The position (1-9) to go to or assign.
With FORCE (or prefix arg \[universal-argument]), always assign even if
position is occupied."
  (if (and (not force)
           (bookmark-get-bookmark (javelin--bookmark-name javelin-number) 'noerror))
      (javelin-go-to javelin-number)
    (javelin-assign-to javelin-number)))


;;;###autoload
(defun javelin-go-or-assign-to-1 (&optional force)
  "Go to or assign position 1 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 1 force))

;;;###autoload
(defun javelin-go-or-assign-to-2 (&optional force)
  "Go to or assign position 2 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 2 force))

;;;###autoload
(defun javelin-go-or-assign-to-3 (&optional force)
  "Go to or assign position 3 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 3 force))

;;;###autoload
(defun javelin-go-or-assign-to-4 (&optional force)
  "Go to or assign position 4 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 4 force))

;;;###autoload
(defun javelin-go-or-assign-to-5 (&optional force)
  "Go to or assign position 5 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 5 force))

;;;###autoload
(defun javelin-go-or-assign-to-6 (&optional force)
  "Go to or assign position 6 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 6 force))

;;;###autoload
(defun javelin-go-or-assign-to-7 (&optional force)
  "Go to or assign position 7 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 7 force))

;;;###autoload
(defun javelin-go-or-assign-to-8 (&optional force)
  "Go to or assign position 8 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 8 force))

;;;###autoload
(defun javelin-go-or-assign-to-9 (&optional force)
  "Go to or assign position 9 of the javelin.
With FORCE (or prefix arg \\[universal-argument]), always assign."
  (interactive "P")
  (javelin-go-or-assign-to 9 force))

(defun javelin--cycle-position (direction)
  "Cycle to the next or previous javelin position.
DIRECTION should be 1 for next, -1 for previous."
  (let ((positions (mapcar #'car (javelin--bookmark-positions))))
    (if (null positions)
        (message "No javelin positions set.")
      (let* ((current (javelin--position-for-current-buffer))
             (current-index (or (seq-position positions current) -1))
             (new-index (mod (+ current-index direction) (length positions)))
             (new-pos (nth new-index positions)))
        (when new-pos
          (javelin-go-to new-pos))))))

;;;###autoload
(defun javelin-go-to-next ()
  "Go to the next buffer in javelin."
  (interactive)
  (javelin--cycle-position 1))

;;;###autoload
(defun javelin-go-to-prev ()
  "Go to the previous buffer in javelin."
  (interactive)
  (javelin--cycle-position -1))

;;;###autoload
(defun javelin-add-file ()
  "Add current buffer to javelin at the next available position."
  (interactive)
  (if (javelin--position-for-current-buffer)
      (message "This buffer is already marked in javelin.")
    (let ((taken (mapcar #'car (javelin--bookmark-positions)))
          next-num)
      (setq next-num
            (cl-loop for pos from 1 to 9
                     unless (member pos taken)
                     return pos))
      (if (null next-num)
          (message "All javelin positions are already assigned.")
        (javelin-assign-to next-num)))))

;;;###autoload
(defun javelin-toggle-quick-menu ()
  "Open quick menu to select a javelined buffer."
  (interactive)
  (let* ((pairs (javelin--bookmark-positions))
         (candidates (mapcar (lambda (pair)
                               (let* ((pos (car pair))
                                      (bm-name (cdr pair))
                                      (record (bookmark-get-bookmark bm-name 'noerror))
                                      (display (if record
                                                   (javelin--bookmark-display-target record)
                                                 bm-name)))
                                 (cons (format "%d: %s" pos display) pos)))
                             pairs))
         (candidate-strings (mapcar #'car candidates))
         ;; Completion table that preserves order (disables sorting)
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata (display-sort-function . identity)
                             (cycle-sort-function . identity))
                         (complete-with-action action candidate-strings string pred)))))
    (if (null candidates)
        (message "No javelin positions set.")
      (when-let ((selection (completing-read "Javelin to file: " collection)))
        (let ((pos (cdr (assoc selection candidates))))
          (javelin-go-to pos))))))

;;;###autoload
(defun javelin-clear ()
  "Clear all javelin positions."
  (interactive)
  (when (or javelin-disable-confirmation
            (yes-or-no-p "Do you really want to clear all javelin positions?"))
    (dolist (pair (javelin--bookmark-positions))
      (bookmark-delete (cdr pair)))
    (javelin--bookmark-save-silent)
    (message "Javelin positions cleaned.")))

;;;###autoload
(defun javelin-clear-all ()
  "Delete all javelin position namespaces."
  (interactive)
  (when (or javelin-disable-confirmation
            (yes-or-no-p "Do you really want to clear all javelin positions across all javelin projects? "))
    (javelin--ensure-bookmarks-loaded)
    (let ((deleted 0))
      (dolist (bm bookmark-alist)
        (let ((name (car bm)))
          (when (string-prefix-p javelin--bookmark-prefix name)
            (bookmark-delete name)
            (setq deleted (1+ deleted)))))
      (javelin--bookmark-save-silent)
      (message "Deleted all javelin positions across %d project(s)." deleted))))

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
    (define-key delete-map (kbd "1") #'javelin-delete-1)
    (define-key delete-map (kbd "2") #'javelin-delete-2)
    (define-key delete-map (kbd "3") #'javelin-delete-3)
    (define-key delete-map (kbd "4") #'javelin-delete-4)
    (define-key delete-map (kbd "5") #'javelin-delete-5)
    (define-key delete-map (kbd "6") #'javelin-delete-6)
    (define-key delete-map (kbd "7") #'javelin-delete-7)
    (define-key delete-map (kbd "8") #'javelin-delete-8)
    (define-key delete-map (kbd "9") #'javelin-delete-9)
    (define-key delete-map (kbd "0") #'javelin-clear)
    (define-key map (kbd "M-0") delete-map)
    ;; Quick menu
    (define-key map (kbd "M--") #'javelin-toggle-quick-menu)
    map)
  "Keymap for `javelin-minor-mode'.")

;;;###autoload
(define-minor-mode javelin-minor-mode
  "Minor mode for quick buffer navigation with javelin.
Provides keybindings for jumping to javelined buffers:
  M-1 to M-9: Go to position (or assign if empty, \\[universal-argument] to force assign)
  M-0 0: Clear all positions
  M-0 1-9: Delete position
  M--: Toggle quick menu"
  :lighter " Jav"
  :keymap javelin-minor-mode-map
  :global nil)

;;;###autoload
(define-globalized-minor-mode global-javelin-minor-mode
  javelin-minor-mode
  (lambda () (javelin-minor-mode 1))
  :group 'javelin)

(provide 'javelin)
;;; javelin.el ends here
