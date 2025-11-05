;;; xdg-launcher.el --- Launch applications -*- lexical-binding: t -*-

;; Author: Sebastien Waegeneire, Steven Allen
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/Stebalien/xdg-launcher.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; xdg-launcher define the `xdg-launcher-run-app' command which uses
;; Emacs standard completion feature to select an application installed
;; on your machine and launch it.

;;; Acknowledgements:

;; This package uses code from the Counsel package by Oleh Krehel.
;; https://github.com/abo-abo/swiper

;;; Code:

(require 'xdg)
(require 'cl-lib)
(require 'map)

(defvar nerd-icons-completion-icon-size)
(declare-function nerd-icons-mdicon "ext:nerd-icons")

(defgroup xdg-launcher nil
  "Customizable options for the `xdg-launcher' package."
  :group 'tools
  :prefix "xdg-launcher")

(defcustom xdg-launcher-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
          (cons (xdg-data-home)
                (xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))

(defcustom xdg-launcher-action-function #'xdg-launcher-action-function-default
  "Define the function that is used to run the selected application."
  :type 'function)

(defcustom xdg-launcher-terminal-function #'make-term
  "The function used to execute terminal applications.

Must be a function that takes the same arguments as `make-term',
which see."
  :type `(radio
          (function :tag "Term" make-term)
          ,@(when (fboundp 'eat-make)
              `((function :tag "EAT" eat-make)))
          (function :tag "Custom")
          (const :tag "Disable terminal support" nil)))


(defcustom xdg-launcher-icon-themes '("hicolor")
  "Icon themes to use for app icons in priority order."
  :type '(choice
          (const :tag "None" nil)
          (repeat :tag "Path" string)))

(defvar xdg-launcher--cache nil
  "Cache of desktop files data.")

(defvar xdg-launcher--cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar xdg-launcher--cached-files nil
  "List of cached desktop files.")

(defconst xdg-launcher--icon-sizes '("scalable" "22x22" "24x24" "32x32" "36x36"
                                     "48x48" "64x64" "72x72" "96x96" "128x128"
                                     "192x192" "256x256" "512x512" "16x16")
  "A list of possible icon-size directory names, ordered by preference.")

(defvar xdg-launcher-icons-directories
  (mapcar (lambda (dir) (expand-file-name "icons" dir))
          (cons (xdg-data-home)
                (xdg-data-dirs)))
  "Directories to search for icon themes.")

(defvar xdg-launcher-icon-extensions '(".svg" ".png")
  "Icon extensions to use for application icons in priority order.")

(defun xdg-launcher--icon-path ()
  "Return a list of directories to search for icons, in priority order."
  (let (search-path)
    (dolist (theme xdg-launcher-icon-themes)
      (dolist (dir xdg-launcher-icons-directories)
        (when-let* ((themedir (expand-file-name theme dir))
                    (file-directory-p themedir))
          (dolist (size xdg-launcher--icon-sizes)
            (when-let* ((path (expand-file-name (concat size "/apps/") themedir))
                        (file-directory-p path))
              (push path search-path))))))
    (nreverse search-path)))

(defun xdg-launcher--get-icon (path icon)
  "Find the requested ICON in the requested PATH."
  (when-let* ((icon-file (if (and (file-name-absolute-p icon) (file-exists-p icon))
                             icon
                           (locate-file icon path xdg-launcher-icon-extensions))))
    (create-image icon-file nil nil :ascent 'center
                  :scale 1.0
                  :height '(1.0 . ch))))

(defun xdg-launcher-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
        result)
    (dolist (dir xdg-launcher-apps-directories)
      (when (file-exists-p dir)
        (let ((dir (file-name-as-directory dir)))
          (dolist (file (directory-files-recursively dir ".*\\.desktop$" nil nil t))
            (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
              (when (and (not (gethash id hash)) (file-readable-p file))
                (push (cons id file) result)
                (puthash id file hash)))))))
    result))

(defun xdg-launcher--parse-exec (exec-string app-name icon-name desktop-file)
  "Parse a .desktop Exec key string, returning a list of command arguments.
EXEC-STRING is the Exec key from the desktop entry.
APP-NAME is the translated application name.
ICON-NAME is the icon name to use for %i expansion.
DESKTOP-FILE is the location of the desktop-file itself."
  (when (and icon-name (string-empty-p icon-name))
    (setq icon-name nil))
  (mapcan
   (lambda (arg)
     (if (string-prefix-p "%" arg)
         ;; Handle field code
         (if (length< arg 2)
             (error "Unescaped %%")
           (pcase (aref arg 1)
             ((or ?f ?u ?F ?U) nil)                      ; Skip file-related codes
             (?i (and icon-name `("--icon" ,icon-name))) ; Icon
             (?c (ensure-list app-name))                 ; Application name
             (?k (ensure-list desktop-file))             ; Desktop file location (skip)
             (?% "%")                                    ; Literal %
             ((or ?d ?D ?n ?N ?v ?m) nil)                ; Skip deprecated codes
             (_ (error "Invalid field code: %s" arg))))  ; Reject unrecognized field codes
       (list arg)))
   (split-string-and-unquote exec-string)))

(defun xdg-launcher--is-installed (tryexec)
  "Check if TRYEXEC file is installed in the \"exec-path\"."
  (or (not tryexec)
      (locate-file tryexec exec-path nil #'file-executable-p)))

(defun xdg-launcher-parse-files (files)
  "Parse the .desktop FILES and return a hash of parsed desktop files.
The hash-table maps desktop file base-names (sans directory or
extension) to alists of:

- name: the human-readable application name.
- file: the full path to the desktop file.
- exec: the command to execute (a list of strings).
- path: the directory from which the command should be launched.
- icon: an image descriptor containing the application's icon.
- terminal: t if the application must be opened in a terminal emulator.
- comment: a human readable comment describing the application.
- visible: t if the application should be displayed in an application
  launcher menu."
  (let ((hash (make-hash-table :test #'equal))
        (iconpath (xdg-launcher--icon-path)))
    (cl-loop
     for (_ . file) in files
     do (map-let (("Name" name) ("Type" type) ("TryExec" try-exec)
                  ("Exec" exec) ("Path" path) ("Comment" comment)
                  ("Icon" icon) ("Terminal" terminal)
                  ("Hidden" hidden) ("NoDisplay" no-display))
            (xdg-desktop-read-file file)
          (when (and exec name
                     (string= type "Application")
                     (xdg-launcher--is-installed try-exec))
            (puthash
             name
             `((name . ,name) (comment . ,comment)
               (path . ,path) (file . ,file)
               (exec . ,(xdg-launcher--parse-exec exec name icon file))
               (icon . ,(and iconpath icon
                             (xdg-launcher--get-icon iconpath icon)))
               (terminal . ,(string= terminal "true"))
               (visible . ,(not (or (string= hidden "true")
                                    (string= no-display "true")))))
             hash)))
     finally return hash)))

;;;###autoload
(defun xdg-launcher-list-apps ()
  "Return a hash-map of all Linux applications.

The return value is the hash table returned from
`xdg-launcher-parse-files', which see.

The return-value is cached and should not be modified by the caller."
  (let* ((new-desktop-alist (xdg-launcher-list-desktop-files))
         (new-files (mapcar #'cdr new-desktop-alist)))
    (unless (and (equal new-files xdg-launcher--cached-files)
                 (null (cl-find-if
                        (lambda (file)
                          (time-less-p
                           xdg-launcher--cache-timestamp
                           (file-attribute-modification-time
                            (file-attributes file))))
                        new-files)))
      (setq xdg-launcher--cache (xdg-launcher-parse-files new-desktop-alist)
            xdg-launcher--cache-timestamp (current-time)
            xdg-launcher--cached-files new-files)))
  xdg-launcher--cache)

(defun xdg-launcher-action-function-default (selected)
  "Default function used to run the SELECTED application."
  (let-alist selected
    (let ((cmd (car .exec))
          (args (cdr .exec))
          (default-directory (or .path default-directory)))
      (with-existing-directory
        (if .terminal
            (progn
              (unless xdg-launcher-terminal-function
                (user-error "Cannot launch %s: terminal support is disabled" .name))
              (pop-to-buffer
               (apply xdg-launcher-terminal-function .name cmd nil args)))
          (apply #'call-process cmd nil 0 nil args))))))

(defun xdg-launcher--affixate (align candidate)
  "Return the annotated CANDIDATE with the description aligned to ALIGN."
  (let-alist candidate
    (list (propertize .name 'xdg-launcher--icon .icon)
          ""
          (if .comment
              (concat (propertize " " 'display `(space :align-to ,align))
                      " "
                      (propertize .comment 'face 'completions-annotations))
            ""))))

(defun xdg-launcher--make-affixation-fn (table)
  "Return an affixation function for `xdg-launcher' completions TABLE."
  (let ((col 20))
    (lambda (completions)
      (setq col (max col (or (cl-loop for c in completions maximize (+ 10 (string-width c))) 0)))
      (mapcar (lambda (c) (xdg-launcher--affixate col (gethash c table))) completions))))

;;;###autoload
(defun xdg-launcher-run-app (&optional arg)
  "Launch an application installed on your machine.
When ARG is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive "P")
  (let* ((candidates (xdg-launcher-list-apps))
         (metadata `((affixation-function . ,(xdg-launcher--make-affixation-fn candidates))
                     (category . xdg-launcher)))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata . ,metadata)
                    (complete-with-action action candidates string pred))))
         (result (completing-read
                  "Run app: "
                  table
                  (lambda (_ candidate)
                    (let-alist candidate
                      (and (or arg .visible)
                           (or xdg-launcher-terminal-function (not .terminal)))))
                  t nil 'xdg-launcher nil nil)))
    (funcall xdg-launcher-action-function (gethash result candidates))))

(defconst xdg-launcher-consult-source
  `( :name          "Application"
     :narrow        ?a
     :category      xdg-launcher
     :require-match t
     :action        ,(lambda (cand) (funcall xdg-launcher-action-function cand))
     :annotate      ,(lambda (cand) (nth 2 (xdg-launcher--affixate 0 cand)))
     :items         ,(lambda () (map-filter (lambda (_k v) (alist-get 'visible v)) (xdg-launcher-list-apps))))
  "Application source for `consult-buffer'.")

;;;###autoload
(with-eval-after-load 'nerd-icons-completion
  (defconst xdg-launcher--nerd-icons-default-icon
    (nerd-icons-mdicon "nf-md-application_cog"
                       :face 'nerd-icons-dsilver
                       :height nerd-icons-completion-icon-size)
    "Default nerd-icons-completion icon for `xdg-launcher'.")
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql xdg-launcher)))
    "Return the icon for the candidate CAND of completion category `xdg-launcher'."
    (let ((icon xdg-launcher--nerd-icons-default-icon))
      (when-let* ((image (if (stringp cand)
                             (get-text-property 0 'xdg-launcher--icon cand)
                           (alist-get 'icon cand))))
        ;; Merge the image with the existing display property.
        (let ((prop (get-text-property 0 'display icon)))
          (unless (listp (car prop)) (setq prop (list prop)))
          (setq icon (propertize icon 'display (cons image prop)))))
      (concat icon " "))))

;; Provide the xdg-launcher feature
(provide 'xdg-launcher)

;;; xdg-launcher.el ends here
