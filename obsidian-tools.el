;;; obsidian-tools.el --- Obsidian Notes tools -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2023 Andreas Jonsson <ajdev8@gmail.com>

;; Author: Andreas Jonsson
;; URL: https://github.com./sonofjon/obsidian-tools.el
;; Package-Version: 
;; Package-Commit: 
;; Keywords: obsidian, pkm, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "27.2") (yaml "0.5.1"))

;; This file is NOT part of GNU Emacs.

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; Obsidian-tools.el provides a number of useful functions that let you
;; manipulate the front matter of your Obsidian Notes vault.

;; TODO:
;;   - Add my/rename-file-and-buffer
;;

;;; Code:
(require 'yaml)

;;;###autoload
(defun aj8/obsidian--buffer-front-matter-start ()
  "Return the starting position of the YAML front matter in the
current buffer.

If no front matter is found, return nil."
  (save-excursion
    (goto-char (point-min))
    (when (and (looking-at "^---") (forward-line))
      (point))))

;;;###autoload
(defun aj8/obsidian--buffer-front-matter-end ()
  "Return the ending position of the YAML front matter in the
current buffer.

If no front matter is found, return nil."
  (save-excursion
    (goto-char (point-min))
    (when (and (looking-at "^---")
               (re-search-forward "^---" nil t 2))
      (- (point) 3))))

(defun obsidian-tools--buffer-front-matter ()
  "Return the front matter string at the beginning of the current buffer.

The front matter is expected to be enclosed in a pair of '---'
lines at the beginning of the buffer. If the buffer does not
contain front matter, signal an error.

The function uses `obsidian--buffer-front-matter-start' and
`obsidian--buffer-front-matter-end' to determine the boundaries
of the front matter."
  (let ((fm-start (obsidian-tools--buffer-front-matter-start))
        (fm-end (obsidian-tools--buffer-front-matter-end)))
    (if (and fm-start fm-end (> fm-end fm-start))
        (buffer-substring-no-properties fm-start fm-end)
      (user-error "There is no front matter in this file!"))))

;;;###autoload
(defun aj8/obsidian-file-to-front-matter-title ()
  "Change the title in the YAML front matter of the current buffer
to be identical to the filename of the buffer.

The function parses the YAML front matter using
`yaml-parse-string', replaces the title field with the
filename, and then rewrites the YAML front matter at the
beginning of the buffer, preserving the original order of the
fields."
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
(defun aj8/obsidian-front-matter-title-to-file ()
  "Rename the current file using the title in the front matter as
filename."
  (interactive)
  (let ((fm-hash (yaml-parse-string (obsidian-tools--buffer-front-matter))))
    (if fm-hash
        (let ((title (gethash 'title fm-hash)))
          (my/rename-file-and-buffer (concat title ".md")))
      (user-error "There is no front matter in this file!"))))

(provide 'obsidian-tools)
;;; obsidian-tools.el ends here
