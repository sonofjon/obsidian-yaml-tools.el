;;; obsidian-tools.el --- Obsidian Notes tools -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2023 Andreas Jonsson

;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/obsidian-tools.el
;; Package-Version:
;; Package-Commit:
;; Package-Requires: ((emacs "27.2") (yaml "0.5.1"))
;; Keywords: obsidian, pkm, convenience
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; Obsidian-tools.el provides a number of useful functions that let you
;; manipulate the front matter of Obsidian notes.

;;;; Installation

;;;;; MELPA

;; Not yet available.

;;;;; Manual

;; Install these required packages:

;; + yaml

;; Then put this file in your load-path, and put this in your init file:

;; (require 'obsidian-tools)

;;;; Usage

;; Run one of these commands:

;; `obsidian-tools-file-to-front-matter-title' : Copy title from filename to front matter.

;; `obsidian-tools-front-matter-title-to-file' : Rename file to match the front matter title.

;;;; Tips

;; + You can customize settings in the `package-name' group (not yet).

;;;; Credits

;; This package was inspired by: obsidian.el[1].
;;
;;  [1] https://github.com./licht1stein/obsidian.el

;; TODO:
;;   - Add my/rename-file-and-buffer
;;

;;; Code:
(require 'yaml)

;;;; Customization

;;;; Variables

;;;;; Keymaps

;;;; Commands

;;;###autoload
(defun obsidian-tools-file-to-front-matter-title ()
  "Copy title from filename to front matter.

The function changes the title in the YAML front matter of the
current buffer to match the filename of the buffer.

The function parses the YAML front matter using
`yaml-parse-string', replaces the title field with the filename,
and then rewrites the YAML front matter at the beginning of the
buffer, preserving the original order of the fields."
  (interactive)
  (let* ((base (file-name-base (buffer-file-name)))
         (fm-hash (yaml-parse-string (obsidian-tools--buffer-front-matter))))
    (if fm-hash
          ;; In order to preserve the order of the fields in the front
          ;; matter we create a copy of the original hash table by iterating
          ;; over its keys and values, and later use the same order with
          ;; `maphash' to insert the fields bask into the front matter.
        (let ((fm-hash-copy (make-hash-table :test 'equal)))
          (cl-loop for key in (hash-table-keys fm-hash)
                   for value in (hash-table-values fm-hash)
                   do (puthash key value fm-hash-copy))
          (puthash 'title base fm-hash-copy)
          (goto-char fm-start)
          (delete-region fm-start fm-end)
          (maphash (lambda (key value)
                     (insert (format "%s: %s\n" key value)))
                   fm-hash-copy)
          (message "Front matter title updated: '%s'" base))
      (user-error "There is no front matter in this file!"))))

;;;###autoload
(defun obsidian-tools-front-matter-title-to-file ()
  "Rename file to match the front matter title.

The function renames the current file using the title in the YAML
front matter as filename."
  (interactive)
  (let ((fm-hash (yaml-parse-string (obsidian-tools--buffer-front-matter))))
    (if fm-hash
        (let ((title (gethash 'title fm-hash)))
          (my/rename-file-and-buffer (concat title ".md")))
      (user-error "There is no front matter in this file!"))))

;;;; Functions

;;;;; Public

;;;;; Private

(defun obsidian-tools--buffer-front-matter-start ()
  "Return the starting position of the YAML front matter in the current buffer.

If no front matter is found, return nil."
  (save-excursion
    (goto-char (point-min))
    (when (and (looking-at "^---") (forward-line))
      (point))))

(defun obsidian-tools--buffer-front-matter-end ()
  "Return the ending position of the YAML front matter in the current buffer.

If no front matter is found, return nil."
  (save-excursion
    (goto-char (point-min))
    (when (and (looking-at "^---")
               (re-search-forward "^---" nil t 2))
      (- (point) 3))))

(defun obsidian-tools--buffer-front-matter ()
  "Return front matter string.

The function returns the YAML front matter at the beginning of
the current buffer.  The front matter is expected to be enclosed
in a pair of '---' lines at the beginning of the buffer.  If the
buffer does not contain front matter, signal an error.

The function uses `obsidian--buffer-front-matter-start' and
`obsidian--buffer-front-matter-end' to determine the boundaries
of the front matter."
  (let ((fm-start (obsidian-tools--buffer-front-matter-start))
        (fm-end (obsidian-tools--buffer-front-matter-end)))
    (if (and fm-start fm-end (> fm-end fm-start))
        (buffer-substring-no-properties fm-start fm-end)
      (user-error "There is no front matter in this file!"))))

;;;; Footer

(provide 'obsidian-tools)

;;; obsidian-tools.el ends here
