;;; passage.el --- Major mode for passage-store.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019  Nicolas Petton & Damien Cassou

;; Author: Nicolas Petton <petton.nicolas@gmail.com>
;;         Damien Cassou <damien@cassou.me>
;; Version: 2.0
;; GIT: https://github.com/NicolasPetton/pass
;; Package-Requires: ((emacs "25") (f "0.17") (age "0.1.3"))
;; Created: 09 Jun 2015
;; Keywords: passage-store, password, keychain

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

;; Major mode for passage-store.el

;; Modified from pass.el for use with https://github.com/FiloSottile/passage

;; Maintainer: Bas Alberts <bas@anti.computer>
;; Url: https://github.com/anticomputer/passage.el

;;; Code:
(require 'passage-store-otp)
(require 'passage-store)
(require 'imenu)
(require 'button)
(require 'f)
(require 'subr-x)

(eval-when-compile (require 'age))

(defgroup pass '()
  "Major mode for passage-store."
  :group 'passage-store)

(defcustom passage-show-keybindings t
  "Whether the keybindings should be displayed in the pass buffer."
  :group 'pass
  :type 'boolean)

(defcustom passage-username-field "username"
  "Field name used in the files to indicate an username."
  :group 'pass
  :type 'string)

(defcustom passage-username-fallback-on-filename nil
  "Whether the entry's filename should be used as a fallback for the username field."
  :group 'pass
  :type 'boolean)

(defvar passage-buffer-name "*Passage-Store*"
  "Name of the pass buffer.")

(defvar passage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'passage-next-entry)
    (define-key map (kbd "p") #'passage-prev-entry)
    (define-key map (kbd "M-n") #'passage-next-directory)
    (define-key map (kbd "M-p") #'passage-prev-directory)
    (define-key map (kbd "e") #'passage-edit)
    (define-key map (kbd "k") #'passage-kill)
    (define-key map (kbd "s") #'isearch-forward)
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "g") #'passage-update-buffer)
    (define-key map (kbd "i") #'passage-insert)
    (define-key map (kbd "I") #'passage-insert-generated)
    (define-key map (kbd "j") #'passage-goto-entry)
    (define-key map (kbd "w") #'passage-copy)
    (define-key map (kbd "b") #'passage-copy-username)
    (define-key map (kbd "f") #'passage-copy-field)
    (define-key map (kbd "u") #'passage-copy-url)
    (define-key map (kbd "U") #'passage-browse-url)
    (define-key map (kbd "v") #'passage-view)
    (define-key map (kbd "r") #'passage-rename)
    (define-key map (kbd "o") #'passage-otp-options)
    (define-key map (kbd "RET") #'passage-view)
    (define-key map (kbd "q") #'passage-quit)
    map)
  "Keymap for `passage-mode'.")

(defface passage-mode-header-face '((t . (:inherit font-lock-keyword-face)))
  "Face for displaying the header of the pass buffer."
  :group 'pass)

(defface passage-mode-entry-face '((t . ()))
  "Face for displaying pass entry names."
  :group 'pass)

(defface passage-mode-directory-face '((t . (:inherit
                                          font-lock-function-name-face
                                          :weight
                                          bold)))
  "Face for displaying passage-store directory names."
  :group 'pass)

(define-derived-mode passage-mode special-mode "Passage-Store"
  "Major mode for editing passage-stores.

\\{passage-mode-map}"
  (setq default-directory (passage-store-dir))
  (setq imenu-prev-index-position-function
        'passage--imenu-prev-index-position-function)
  (setq imenu-extract-index-name-function
        'passage--imenu-extract-index-name-function)
  (read-only-mode))

(defun passage--imenu-prev-index-position-function ()
  "Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (passage-prev-entry)
  (not (bobp)))

(defun passage--imenu-extract-index-name-function ()
  "Return imenu name for pass entry at point.
This function is used as a value for
`imenu-extract-index-name-function'."
  (passage-entry-at-point))

(defun passage-setup-buffer ()
  "Setup the passage-store buffer."
  (passage-mode)
  (passage-update-buffer))

;;;###autoload
(defun passage ()
  "Open the passage-store buffer."
  (interactive)
  (if (get-buffer passage-buffer-name)
      (progn
        (switch-to-buffer passage-buffer-name)
        (passage-update-buffer))
    (let ((buf (get-buffer-create passage-buffer-name)))
      (pop-to-buffer buf)
      (passage-setup-buffer))))

(defmacro passage--with-writable-buffer (&rest body)
  "Evaluate BODY as if the current buffer was not in `read-only-mode'."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro passage--save-point (&rest body)
  "Evaluate BODY and restore the point.
Similar to `save-excursion' but only restore the point."
  (declare (indent 0) (debug t))
  (let ((point (make-symbol "point")))
    `(let ((,point (point)))
       ,@body
       (goto-char (min ,point (point-max))))))

(defun passage--find-file (filename)
  "Open passage file with appropriate age.el context."
  ;; make age.el respect passage env variables
  ;; see: https://github.com/FiloSottile/passage
  (cl-letf (((symbol-value 'age-default-identity)
             (or (getenv "PASSAGE_IDENTITIES_FILE") age-default-identity))
            ((symbol-value 'age-default-recipient)
             (or (getenv "PASSAGE_RECIPIENTS_FILE")
                 (let ((recipients (getenv "PASSAGE_RECIPIENTS")))
                   (when (stringp recipients)
                     (split-string recipients)))
                 age-default-recipient)))
    (find-file filename)))

(defun passage-quit ()
  "Kill the buffer quitting the window."
  (interactive)
  (when (y-or-n-p "Kill all passage entry buffers? ")
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'passage-view-mode)
          (kill-buffer buf)))))
  (quit-window t))

(defun passage-next-entry ()
  "Move point to the next entry found."
  (interactive)
  (passage--goto-next #'passage-entry-at-point))

(defun passage-prev-entry ()
  "Move point to the previous entry."
  (interactive)
  (passage--goto-prev #'passage-entry-at-point))

(defun passage--target-entry-pos (target)
  "Return position of TARGET entry or nil if it doesn't exist."
  (save-excursion
    (goto-char (point-min))
    (passage--goto-next (lambda () (equal (passage-entry-at-point) target)))
    (unless (eobp)
      (point))))

(defun passage-goto-entry (entry)
  "Move point to ENTRY and return its position.
If ENTRY doesn't exist in the buffer, then preserve point and return nil.

Note that ENTRY must reflect its deep level in the directory structure;
for instance, an entry `bar' inside the subdirectory `foo', must be
specified as `foo/bar'.

When called interactively, prompt users with completion using
all entries in the passage buffer."
  (interactive
   (list (completing-read "Jump to entry: "
                          (passage-store-list) nil 'match)))
  (let ((entry-pos (passage--target-entry-pos entry)))
    (if entry-pos
        (goto-char entry-pos)
      (message "No entry matches %s in current directory" entry)
      nil)))

(defun passage-next-directory ()
  "Move point to the next directory found."
  (interactive)
  (passage--goto-next #'passage-directory-at-point))

(defun passage-prev-directory ()
  "Move point to the previous directory."
  (interactive)
  (passage--goto-prev #'passage-directory-at-point))

(defmacro passage--with-closest-entry (varname &rest body)
  "Bound VARNAME to the closest entry before point and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((,varname (passage-closest-entry)))
     (if ,varname
         (progn ,@body)
       (message "No entry at point"))))

(defun passage-rename (new-name)
  "Rename the entry at point to NEW-NAME."
  (interactive (list (read-string "Rename entry to: " (passage-closest-entry))))
  (passage--with-closest-entry entry
    (passage-store-rename entry new-name)
    (passage-update-buffer)))

(defun passage-edit ()
  "Edit the entry at point."
  (interactive)
  (passage--with-closest-entry entry
    (when (yes-or-no-p (format "Do you want edit the entry %s? " entry))
      (passage-store-edit entry))))

(defun passage-kill ()
  "Remove the entry at point."
  (interactive)
  (passage--with-closest-entry entry
    (when (yes-or-no-p (format "Do you want remove the entry %s? " entry))
      (passage-store-remove entry)
      (passage-update-buffer))))

(defun passage-update-buffer ()
  "Update the current buffer contents."
  (interactive)
  (passage--save-point
    (passage--with-writable-buffer
      (delete-region (point-min) (point-max))
      (passage-display-data))))

(defun passage-insert (arg)
  "Insert an entry to the passage-store.
The password is read from user input.

When called with a prefix argument ARG, visit the entry file."
  (interactive "P")
  (if arg
      (passage-insert-multiline)
    (progn
      (call-interactively #'passage-store-insert)
      (passage-update-buffer))))

(defun passage-insert-multiline ()
  "This function behaves similarly to `passage -m'.
It creates an empty entry file, and visit it."
  (let ((entry (format "%s.age" (read-string "Password entry: ")))
        (default-directory (passage-store-dir)))
    (make-directory (file-name-directory entry) t)
    (passage--find-file (expand-file-name entry (passage-store-dir)))))

(defun passage-insert-generated ()
  "Insert an entry to the passage-store.
Use a generated password instead of reading the password from
user input."
  (interactive)
  (call-interactively #'passage-store-generate)
  (passage-update-buffer))

(defun passage-view ()
  "Visit the entry at point."
  (interactive)
  (passage--with-closest-entry entry
    (passage--find-file (concat (f-join (passage-store-dir) entry) ".age"))))

(defun passage-copy ()
  "Add the password of entry at point to kill ring."
  (interactive)
  (passage--with-closest-entry entry
    (passage-store-copy entry)))

(defun passage--copy-field (field)
  "Add FIELD of entry at point to kill ring."
  (passage--with-closest-entry entry
    (let* ((inhibit-message t)
           (parsed-entries (passage-store-parse-entry entry)))
      (unless (assoc field parsed-entries)
        (user-error "Field `%s' not in  %s" field entry))
      (passage-store-copy-field entry field))))

(defun passage-copy-field (field)
  "Add FIELD of entry at point to kill ring.

When called interactively, prompt users for field with completion
using all fields in the entry."
  (interactive
   (passage--with-closest-entry entry
     (list (passage-store-read-field entry))))
  (if (equal field "secret") (passage-copy)
    (passage--copy-field field)))

(defun passage-copy-username ()
  "Add username of entry at point to kill ring.

If the entry does not have a username field/value within the entry, and if
`passage-username-fallback-on-filename' is non-nil, copy the entry name instead."
  (interactive)
  (condition-case err
      (passage--copy-field passage-username-field)
    (user-error
     (if (not passage-username-fallback-on-filename)
         (signal (car err) (cdr err))) ;; rethrow
     (passage--with-closest-entry entry
       (let ((entry-name (file-name-nondirectory entry)))
         (passage-store--save-field-in-kill-ring entry entry-name "username"))))))

(defun passage-copy-url ()
  "Add url of entry at point to kill ring."
  (interactive)
  (passage--copy-field passage-store-url-field))

(defun passage-display-data ()
  "Display the passage-store data into the current buffer."
  (let ((items (passage--tree)))
    (passage-display-header)
    (passage-display-item items)))

(defun passage-display-header ()
  "Display the header in to the current buffer."
  (insert "Passage store directory:")
  (put-text-property (point-at-bol) (point) 'face 'passage-mode-header-face)
  (insert " ")
  (passage--display-keybindings-toggle)
  (insert "\n\n")
  (when passage-show-keybindings
    (passage--display-keybindings '((passage-copy . "Copy password")
                                    (passage-copy-username . "Copy username")
                                    (passage-copy-url . "Copy url")))
    (insert "\n")
    (passage--display-keybindings '((passage-copy-field . "Copy field")
                                    (passage-goto-entry . "Jump to Entry")
                                    (passage-browse-url . "Browse url")))

    (insert "\n")
    (passage--display-keybindings '((passage-insert . "Insert")
                                    (passage-next-entry . "Next")
                                    (passage-update-buffer . "Update")))
    (insert "\n")
    (passage--display-keybindings '((passage-insert-generated . "Generate")
                                    (passage-prev-entry . "Previous")
                                    (passage-otp-options . "OTP Support")))
    (insert "\n")
    (passage--display-keybindings '((passage-rename . "Rename")
                                    (passage-next-directory . "Next dir")
                                    (passage-view . "View entry")))
    (insert "\n")
    (passage--display-keybindings '((passage-kill . "Delete")
                                    (passage-prev-directory . "Previous dir")
                                    (describe-mode . "Help")))
    (insert "\n")
    (passage--display-keybindings '((passage-edit . "Edit")))
    (newline)
    (newline)))

(defun passage--display-keybindings-toggle ()
  "Display a button to toggle whether keybindings should be displayed."
  (let ((label (if passage-show-keybindings
                   "[Hide keybindings]"
                 "[Show keybindings]")))
    (insert-button label 'action #'passage--toggle-display-keybindings)))

(defun passage--toggle-display-keybindings (&rest _)
  "Toggle displaying the keybindings and update the buffer."
  (setq passage-show-keybindings (not passage-show-keybindings))
  (put passage-show-keybindings
       'customized-value
       (list (custom-quote (symbol-value passage-show-keybindings))))
  (passage-update-buffer))

(defun passage--display-keybindings (bindings)
  "Display the keybinding in each item of BINDINGS.
BINDINGS is an alist of bindings."
  (mapc (lambda (pair)
          (passage--display-keybinding (car pair) (cdr pair)))
        bindings))

(defun passage--display-keybinding (command label)
  "Insert the associated keybinding for COMMAND with LABEL."
  (insert (format "%8s %-13s \t "
                  (format "%s"
                          (propertize (substitute-command-keys
                                       (format "<\\[%s]>" (symbol-name command)))
                                      'face 'font-lock-constant-face))
                  label)))

(defun passage-display-item (item &optional indent-level)
  "Display the directory or entry ITEM into the current buffer.
If INDENT-LEVEL is specified, add enough spaces before displaying
ITEM."
  (unless indent-level (setq indent-level 0))
  (let ((directory (listp item)))
    (passage-display-item-prefix indent-level)
    (if directory
        (passage-display-directory item indent-level)
      (passage-display-entry item))))

(defun passage-display-entry (entry)
  "Display the passage-store entry ENTRY into the current buffer."
  (let ((entry-name (f-filename entry)))
    (insert entry-name)
    (add-text-properties (point-at-bol) (point)
                         `(face passage-mode-entry-face passage-entry ,entry))
    (newline)))

(defun passage-display-directory (directory indent-level)
  "Display the directory DIRECTORY into the current buffer.

DIRECTORY is a list, its CAR being the name of the directory and its CDR
the entries of the directory.  Add enough spaces so that each entry is
indented according to INDENT-LEVEL."
  (let ((name (car directory))
        (items (cdr directory)))
    (insert name)
    (add-text-properties (point-at-bol) (point)
                         `(face passage-mode-directory-face passage-directory ,name))
    (newline)
    (dolist (item items)
      (passage-display-item item (1+ indent-level)))))

(defun passage-display-item-prefix (indent-level)
  "Display some indenting text according to INDENT-LEVEL."
  (dotimes (_ (max 0 (* (1- indent-level) 4)))
    (insert " "))
  (unless (zerop indent-level)
    (insert "├── ")))

(defun passage-entry-at-point ()
  "Return the `passage-entry' property at point."
  (get-text-property (point-at-eol) 'passage-entry))

(defun passage-directory-at-point ()
  "Return the `passage-directory' property at point."
  (get-text-property (point) 'passage-directory))

(defun passage-closest-entry ()
  "Return the closest entry in the current buffer, looking backward."
  (save-excursion
    (or (passage-entry-at-point)
        (unless (bolp)
          (forward-line -1)
          (passage-closest-entry)))))

(defun passage-otp-options (option)
  "Dispatch otp actions depending on user OPTION input.
Display help message with OTP functionality options."
  (interactive
   (list
    (read-char-choice "[i] Insert, [a] Append, [s] Append from screenshot, [o] Copy token, [u] Copy URI, or [C-g] to abort: "
                      '(?i ?a ?s ?o ?u))))
  (unless (require 'passage-store-otp nil t)
    (user-error "You cannot do this without installing `passage-store-otp' first"))
  (pcase option
    (?i (passage-otp-insert))
    (?a (passage-otp-append))
    (?s (passage-otp-from-screenshot))
    (?o (passage-otp-token-copy))
    (?u (passage-otp-uri-copy))))

(defun passage-otp-token-copy ()
  "Add OTP Token from closest entry to kill ring."
  (interactive)
  (passage--with-closest-entry entry
    (passage-store-otp-token-copy entry)))

(defun passage-otp-uri-copy ()
  "Add OTP URI from closest entry to kill ring."
  (interactive)
  (passage--with-closest-entry entry
    (passage-store-otp-uri-copy entry)))

(defun passage-otp-insert ()
  "Insert an OTP URI entry to the passage-store.
The password is read from user input."
  (interactive)
  (call-interactively #'passage-store-otp-insert)
  (passage-update-buffer))

(defun passage-otp-append ()
  "Append an OTP URI to an existing entry in the passage-store.
The password is read from user input."
  (interactive)
  (passage--with-closest-entry entry
    (passage-store-otp-append entry (read-passwd "OTP URI: " t)))
  (passage-update-buffer))

(defun passage-otp-from-screenshot ()
  "Append an OTP URI taken from a screenshot to an existing entry in the passage-store."
  (interactive)
  (passage--with-closest-entry entry
    (passage-store-otp-append-from-image entry))
  (passage-update-buffer))

(defun passage--goto-next (pred)
  "Move point to the next match of PRED."
  (forward-line)
  (while (not (or (eobp) (funcall pred)))
    (forward-line)))

(defun passage--goto-prev (pred)
  "Move point to the previous match of PRED."
  (forward-line -1)
  (while (not (or (bobp) (funcall pred)))
    (forward-line -1)))

(defun passage--tree (&optional subdir)
  "Return a tree of all entries in SUBDIR.
If SUBDIR is nil, return the entries of `(passage-store-dir)'."
  (unless subdir (setq subdir ""))
  (let ((path (f-join (passage-store-dir) subdir)))
    (if (f-directory? path)
        (unless (string= (f-filename subdir) ".git")
          (cons (f-filename path)
                (delq nil
                      (mapcar 'passage--tree
                              (f-entries path)))))
      (when (and (equal (f-ext path) "age")
                 (not (backup-file-name-p path)))
        (passage-store--file-to-entry path)))))

;;; major mode for viewing entries

(defvar passage-view-mask "·············"
  "Mask used to hide passwords.")

(defvar passage-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'passage-view-toggle-password)
    (define-key map (kbd "C-c C-w") #'passage-view-copy-password)
    map))
(make-variable-buffer-local 'passage-view-mode-map)

(defun passage-view-entry-name (&optional buffer)
  "Return the entry name for BUFFER.
This function only works when `passage-view-mode' is enabled."
  (with-current-buffer (or buffer (current-buffer))
    (when (eq major-mode 'passage-view-mode)
      (f-no-ext (replace-regexp-in-string
                 (format "^%s/" (f-expand (passage-store-dir)))
                 ""
                 buffer-file-name)))))

(defun passage-view-toggle-password ()
  "Enable or disable password hiding."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((buf-modified (buffer-modified-p)))
      (if (string= (get-text-property (point) 'display)
                   passage-view-mask)
          (passage-view-unmask-password)
        (passage-view-mask-password))
      (set-buffer-modified-p buf-modified))))

(defun passage-view-copy-password ()
  "Copy the password of the entry in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (copy-region-as-kill (point) (line-end-position))
    (message "Password copied to kill ring.")))

(defun passage-view-mask-password ()
  "Mask the password of the current buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (set-text-properties (point-min) (line-end-position)
                           `(display ,passage-view-mask)))))

(defun passage-view-unmask-password ()
  "Show the password in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (remove-text-properties (point-min) (line-end-position)
                            '(display nil))))

(defun passage-view-copy-token ()
  "Copy current `passage-view' buffer's OTP token into clipboard."
  (interactive)
  (when-let (entry-name (passage-view-entry-name))
    (passage-store-otp-token-copy entry-name)))

(defun passage-view-qrcode ()
  "Open a new buffer that displays a QR Code for the current entry."
  (interactive)
  (when-let (entry-name (passage-view-entry-name))
    (let ((qrcode-buffer (get-buffer-create "*passage-view-qrcode*")))
      (with-current-buffer qrcode-buffer
        (fundamental-mode)  ;; Return buffer *back* to fundamental, in case it isn't already.
        (erase-buffer)
        (insert (passage-store-otp-qrcode entry-name "SVG"))
        (image-mode)
        (local-set-key (kbd "q") 'kill-this-buffer))
      (switch-to-buffer-other-window qrcode-buffer))))

(defun passage-view--otp-remaining-secs ()
  "Return a string with the remaining countdown base 30."
  (let* ((base 30)
         (remaining (- base (% (truncate (time-to-seconds (current-time)))
                               base)))
         (remaining-str (number-to-string remaining)))
    (if (< remaining 10)  ;; leftpad-ing
        (concat "0" remaining-str)
      remaining-str)))

(defun passage-view--set-otp-header (token remaining-secs)
  "Display OTP TOKEN and REMAINING-SECS in Header Line."
  (let ((otp-data (concat (propertize " " 'display '((space :align-to 0)))
                          (propertize "OTP: " 'face 'passage-mode-header-face)
                          token " - " remaining-secs "s remaining"))
        (key-binding (concat (propertize (substitute-command-keys
                                          (format "<\\[%s]>" "passage-view-copy-token"))
                                         'face 'font-lock-constant-face)
                             " Copy token")))
    (setq header-line-format (concat otp-data "    " key-binding))
    (force-mode-line-update)))

(defun passage-view--has-otp-p ()
  "Return t-ish value if there's an OTP URI in the current buffer.
nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (search-forward "otpauth://" nil t)))

(defun passage-view--otp-counter (buffer &optional last-token force-create)
  "Reload BUFFER's OTP token and countdown, using LAST-TOKEN if any, and if FORCE-CREATE, build Header Line from scratch."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (or header-line-format force-create)
        (let* ((remaining-secs (passage-view--otp-remaining-secs))
               (token (if (or (not last-token)
                              (string= remaining-secs "30"))
                          (passage-store-otp-token (passage-view-entry-name buffer))
                        last-token)))
          (passage-view--set-otp-header token remaining-secs)
          (run-at-time 1 nil #'passage-view--otp-counter buffer token))))))

(defun passage-view--prepare-otp ()
  "Start an OTP token/remaining secs counter in current buffer.
This function also binds a couple of handy OTP related key-bindings to
`passage-mode-map'."
  (when (and (require 'passage-store-otp nil t)
             (passage-view--has-otp-p))
    ;; Build OTP counter
    (passage-view--otp-counter (current-buffer) nil t)
    ;; Rebuild header after saving.
    (add-hook 'after-save-hook
              #'(lambda ()
                  (if (passage-view--has-otp-p)
                      (passage-view--otp-counter (current-buffer) nil t)
                    ;; Remove header line
                    (setq header-line-format nil)))
              t t)
    ;; Define a couple of OTP helper shortcuts
    (define-key passage-view-mode-map (kbd "C-c C-o") #'passage-view-copy-token)
    (define-key passage-view-mode-map (kbd "C-c C-q") #'passage-view-qrcode)))

(defun passage-browse-url ()
  "Browse URL in entry at point."
  (interactive)
  (passage--with-closest-entry entry
                               (passage-store-url entry)))

(defvar passage-view-font-lock-keywords '("\\(^[^:\t\n]+:\\) " 1 'font-lock-keyword-face)
  "Font lock keywords for ‘passage-view-mode’.")

(define-derived-mode passage-view-mode nil "Passage-View"
  "Major mode for viewing passage-store entries.

\\{passage-view-mode-map}"
  (passage-view-toggle-password)
  (passage-view--prepare-otp)
  (setq-local font-lock-defaults '(passage-view-font-lock-keywords t))
  (font-lock-mode 1)
  (message
   (substitute-command-keys
    "Press <\\[passage-view-toggle-password]> to display & edit the password")))

(add-to-list 'auto-mode-alist
             (cons
              (format "%s/.*\\.age\\'"
                      (expand-file-name (passage-store-dir)))
              'passage-view-mode))

(provide 'passage)
;;; passage.el ends here
