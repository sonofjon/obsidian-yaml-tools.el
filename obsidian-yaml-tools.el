;;; obsidian-yaml-tools.el --- Obsidian YAML tools -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2023 Andreas Jonsson

;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/obsidian-yaml-tools.el
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

;; Obsidian-yaml-tools.el provides a number of useful functions that let you
;; manipulate the front matter of Obsidian notes.

;;;; Installation

;;;;; MELPA

;; Not yet available.

;;;;; Manual

;; Install these required packages:

;; + yaml

;; Then put this file in your load-path, and put this in your init file:

;; (require 'obsidian-yaml-tools)

;;;; Usage

;; Run one of these commands:

;; `oyt-update-yaml-title-from-filename' : Update the title in the YAML front matter to match the current filename.

;; `oyt-update-filename-from-yaml-title' : Rename the current file to match the title in the YAML front matter.

;; `oyt-update-yaml-date' : Update the date in the YAML front matter.

;;;; Tips

;; You can customize settings in the `package-name' group:

;; `oyt-time-string-format' : Time string format.

;; `oyt-storage-type' : Data structure for internal storage of YAML.

;;;; Credits

;; This package was inspired by: obsidian.el[1].
;;
;;  [1] https://github.com./licht1stein/obsidian.el

;; TODO:
;;   - Add my/rename-file-and-buffer

;;; Code:
(require 'yaml)

;;;; Customization

(defcustom oyt-time-string-format "%Y-%m-%d %H:%M:%S%z"
  "Time string format."
  :type 'string)

(defcustom oyt-storage-type 'alist
  "Data structure for internal storage of YAML."
  :type 'symbol
  :options '('alist 'hash-table))

;;;; Variables

;;;;; Keymaps

;;;; Commands

;;;###autoload
(defun oyt-update-filename-from-yaml-title ()
  "Rename the current file to match the title in the YAML front matter."
  (interactive)
  (if-let ((fm-hash (yaml-parse-string (oyt--buffer-yaml))))
      (let ((title (gethash 'title fm-hash)))
        (my/rename-file-and-buffer (concat title ".md")))
    (user-error "There is no front matter in this file!")))

;;;###autoload
(defun oyt-update-yaml-title-from-filename ()
  "Update the title in the YAML front matter to match the current filename.

The function updates the \\='title\\=' field in the YAML front
matter to match the current filename."
  (interactive)
  (let ((base-name (file-name-base (buffer-file-name))))
    (oyt--buffer-update-yaml-key-value 'title base-name t)))

;;;###autoload
(defun oyt-update-yaml-date ()
  "Update the date in the YAML front matter.

The function updates the \\='updated\\=' field in the YAML front
matter to match the current time."
  (interactive)
  (oyt--buffer-update-yaml-key-value
   'updated oyt-time-string-format))

;;;; Functions

;;;;; Public

;;;;; Private

(defun oyt--buffer-yaml-start ()
  "Return the starting position of the YAML front matter in the current buffer.

If no front matter is found, signal an error."
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at "^---") (forward-line))
        (point)
      (error "No YAML front matter found."))))

(defun oyt--buffer-yaml-end ()
  "Return the ending position of the YAML front matter in the current buffer.

If no front matter is found, signal an error."
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at "^---")
             (search-forward-regexp "^---" nil t 2))
        (- (point) 3)
      (error "No YAML front matter found."))))

(defun oyt--buffer-yaml (&optional start end)
"Return the YAML front matter at the beginning of the current buffer.

If START and END are specified, the function returns the front
matter within those bounds. Otherwise, the function uses
`'obsidian--buffer-yaml-start' and `obsidian--buffer-yaml-end' to
determine the boundaries of the front matter.

The front matter is expected to be enclosed in a pair of '---'
lines at the beginning of the buffer. If the buffer does not
contain front matter, the function signals an error."
  (let ((fm-start (or start (oyt--buffer-yaml-start)))
        (fm-end (or end (oyt--buffer-yaml-end))))
    (if (> fm-end fm-start)
        (buffer-substring-no-properties fm-start fm-end)
      (user-error "There is no YAML front matter in this file!"))))

(defun oyt--buffer-replace-yaml (yaml-text)
  "Replace the YAML front matter in the current buffer with YAML-TEXT."
  (let ((fm-start (oyt--buffer-yaml-start))
        (fm-end (oyt--buffer-yaml-end)))
    (if (> fm-end fm-start)
        (save-excursion
          (goto-char fm-start)
          (delete-region fm-start fm-end)
          (insert yaml-text))
      (user-error "There is no front matter in this file!"))))

(defun oyt--buffer-update-yaml-key-value (key value &optional update-time)
  "Update the VALUE of KEY in the YAML front matter of the current buffer.

If KEY does not exist in the front matter, it will be added with the
given VALUE.

If UPDATE-TIME is non-nil, the 'updated' field in the front matter
will be updated with the current time."

  (cond ((eq oyt-storage-type 'alist)
         (if-let ((fm-alist (yaml-parse-string (oyt--buffer-yaml)
                                               :object-type 'alist)))
             (progn
               ;; Update the VALUE of KEY in the alist
               (if (assoc key fm-alist)
                   (setcdr (assoc key fm-alist) value)
                 (push (cons key value) fm-alist))
               ;; Update the 'updated' field in the alist if requested
               (when update-time
                 (if (assoc 'updated fm-alist)
                     (setcdr (assoc 'updated fm-alist) oyt-time-string-format)
                   (push (cons 'updated oyt-time-string-format) fm-alist)))
               (let* ((fm-string-0 (yaml-encode fm-alist))
                      ;; Remove initial newline character if present, and add
                      ;; newline character at the end of the string
                      (fm-string (concat
                                  (string-trim fm-string-0 "\n" nil)
                                  "\n")))
                 (oyt--buffer-replace-yaml fm-string))
               (message "Front matter updated: '%s: %s'" key value))
           (user-error "There is no front matter in this file!")))

        ((eq oyt-storage-type 'hash-table)
         (if-let ((fm-hash (yaml-parse-string (oyt--buffer-yaml))))
             (let ((fm-hash-copy (make-hash-table :test 'equal))
                   fm-string)
               ;; In order to preserve the order of the fields in the
               ;; front matter we create a copy of the original hash table
               ;; by iterating over its keys and values, and later use the
               ;; same order with `maphash' to insert the fields back into
               ;; the front matter.
               (cl-loop for k in (hash-table-keys fm-hash)
                        for v in (hash-table-values fm-hash)
                        do (puthash k v fm-hash-copy))
               (puthash key value fm-hash-copy)
               (when update-time
                 (puthash 'updated oyt-time-string-format
                           fm-hash-copy))
               (maphash (lambda (k v)
                          (setq fm-string (concat
                                           fm-string
                                           (format "%s: %s\n" k v))))
                                        ; TODO: dates should be double-quoted
                        fm-hash-copy)
               (oyt--buffer-replace-yaml fm-string)
               (message "Front matter updated: '%s: %s'" key value))
           (user-error "There is no front matter in this file!")))))

;;;; Footer

(provide 'obsidian-yaml-tools)

;;; obsidian-yaml-tools.el ends here
