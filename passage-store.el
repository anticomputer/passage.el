;;; passage-store.el --- Password store (pass) support  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Svend Sorensen <svend@svends.net>

;; Author: Svend Sorensen <svend@svends.net>
;; Maintainer: Tino Calancha <tino.calancha@gmail.com>
;; Version: 2.1.4
;; URL: https://www.passwordstore.org/
;; Package-Requires: ((emacs "25") (s "1.9.0") (with-editor "2.5.11") (auth-source-passage "5.0.0"))
;; Keywords: tools pass password passage-store

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

;; This package provides functions for working with pass ("the
;; standard Unix password manager").
;;
;; http://www.passwordstore.org/

;; Modified from password-store.el for use with https://github.com/FiloSottile/passage

;; Maintainer: Bas Alberts <bas@anti.computer>
;; Url: https://github.com/anticomputer/passage.el

;;; Code:

(require 'with-editor)
(require 'auth-source-passage)

(defgroup passage-store '()
  "Emacs mode for passage-store."
  :prefix "passage-store-"
  :group 'passage-store)

(defcustom passage-store-passage-length 25
  "Default password length."
  :group 'passage-store
  :type 'number)

(defcustom passage-store-time-before-clipboard-restore
  (if (getenv "PASSWORD_STORE_CLIP_TIME")
      (string-to-number (getenv "PASSWORD_STORE_CLIP_TIME"))
    45)
  "Number of seconds to wait before restoring the clipboard."
  :group 'passage-store
  :type 'number)

(defcustom passage-store-url-field "url"
  "Field name used in the files to indicate an url."
  :group 'passage-store
  :type 'string)

(defvar passage-store-executable
  (executable-find "passage")
  "Passage executable.")

(defvar passage-store-timeout-timer nil
  "Timer for clearing clipboard.")

(defun passage-store-timeout ()
  "Number of seconds to wait before clearing the password.

This function just returns `passage-store-time-before-clipboard-restore'.
Kept for backward compatibility with other libraries."
  passage-store-time-before-clipboard-restore)

(defun passage-store--run-1 (callback &rest args)
  "Run pass with ARGS.

Nil arguments are ignored.  Calls CALLBACK with the output on success,
or outputs error message on failure."
  (let ((output ""))
    (make-process
     :name "passage-store-age"
     :command (cons passage-store-executable (delq nil args))
     :connection-type 'pipe
     :noquery t
     :filter (lambda (process text)
               (setq output (concat output text)))
     :sentinel (lambda (process state)
                 (cond
                  ((string= state "finished\n")
                   (funcall callback output))
                  ((string= state "open\n") (accept-process-output process))
                  (t (error (concat "passage-store: " state))))))))

(defun passage-store--run (&rest args)
  "Run pass with ARGS.

Nil arguments are ignored.  Returns the output on success, or
outputs error message on failure."
  (let ((output nil)
        (slept-for 0))
    (apply #'passage-store--run-1 (lambda (password)
                                     (setq output password))
           (delq nil args))
    (while (not output)
      (sleep-for .1))
    output))

(defun passage-store--run-async (&rest args)
  "Run pass asynchronously with ARGS.

Nil arguments are ignored.  Output is discarded."
  (let ((args (mapcar #'shell-quote-argument args)))
    (with-editor-async-shell-command
     (mapconcat 'identity
                (cons passage-store-executable
                      (delq nil args)) " "))))

;; XXX: not implemented in passage
;; (defun passage-store--run-init (age-ids &optional folder)
;;   (apply 'passage-store--run "init"
;;          (if folder (format "--path=%s" folder))
;;          age-ids))

(defun passage-store--run-list (&optional subdir)
  (error "Not implemented"))

(defun passage-store--run-grep (&optional string)
  (error "Not implemented"))

(defun passage-store--run-find (&optional string)
  (error "Not implemented"))

(defun passage-store--run-show (entry &optional callback)
  (if callback
      (passage-store--run-1 callback "show" entry)
    (passage-store--run "show" entry)))

(defun passage-store--run-insert (entry password &optional force)
  (error "Not implemented"))

(defun passage-store--run-edit (entry)
  (passage-store--run-async "edit"
                             entry))

(defun passage-store--run-generate (entry passage-length &optional force no-symbols)
  (passage-store--run "generate"
                       (if force "--force")
                       (if no-symbols "--no-symbols")
                       entry
                       (number-to-string passage-length)))

(defun passage-store--run-remove (entry &optional recursive)
  (passage-store--run "remove"
                       "--force"
                       (if recursive "--recursive")
                       entry))

(defun passage-store--run-rename (entry new-entry &optional force)
  (passage-store--run "rename"
                       (if force "--force")
                       entry
                       new-entry))

(defun passage-store--run-copy (entry new-entry &optional force)
  (passage-store--run "copy"
                       (if force "--force")
                       entry
                       new-entry))

(defun passage-store--run-git (&rest args)
  (apply 'passage-store--run "git"
         args))

(defun passage-store--run-version ()
  (passage-store--run "version"))

(defvar passage-store-kill-ring-pointer nil
  "The tail of of the kill ring ring whose car is the password.")

(defun passage-store-dir ()
  "Return password store directory."
  (or (bound-and-true-p auth-source-passage-filename)
      (getenv "PASSAGE_DIR")
      "~/.passage/store"))

(defun passage-store--entry-to-file (entry)
  "Return file name corresponding to ENTRY."
  (concat (expand-file-name entry (passage-store-dir)) ".age"))

(defun passage-store--file-to-entry (file)
  "Return entry name corresponding to FILE."
  (file-name-sans-extension (file-relative-name file (passage-store-dir))))

(defun passage-store--completing-read (&optional require-match)
  "Read a password entry in the minibuffer, with completion.

Require a matching password if `REQUIRE-MATCH' is 't'."
  (completing-read "Password entry: " (passage-store-list) nil require-match))

(defun passage-store-parse-entry (entry)
  "Return an alist of the data associated with ENTRY.

ENTRY is the name of a passage-store entry."
  (auth-source-passage-parse-entry entry))

(defun passage-store-read-field (entry)
  "Read a field in the minibuffer, with completion for ENTRY."
  (let* ((inhibit-message t)
         (valid-fields (mapcar #'car (passage-store-parse-entry entry))))
    (completing-read "Field: " valid-fields nil 'match)))

(defun passage-store-list (&optional subdir)
  "List password entries under SUBDIR."
  (unless subdir (setq subdir ""))
  (let ((dir (expand-file-name subdir (passage-store-dir))))
    (if (file-directory-p dir)
        (delete-dups
         (mapcar 'passage-store--file-to-entry
                 (directory-files-recursively dir ".+\\.age\\'"))))))

;;;###autoload
(defun passage-store-edit (entry)
  "Edit password for ENTRY."
  (interactive (list (passage-store--completing-read t)))
  (passage-store--run-edit entry))

;;;###autoload
(defun passage-store-get (entry &optional callback)
  "Return password for ENTRY.

Returns the first line of the password data.
When CALLBACK is non-`NIL', call CALLBACK with the first line instead."
  (let* ((inhibit-message t)
         (secret (auth-source-passage-get 'secret entry)))
    (if (not callback) secret
      (passage-store--run-show
       entry
       (lambda (_) (funcall callback secret))))))

;;;###autoload
(defun passage-store-get-field (entry field &optional callback)
  "Return FIELD for ENTRY.
FIELD is a string, for instance \"url\".
When CALLBACK is non-`NIL', call it with the line associated to FIELD instead.
If FIELD equals to symbol secret, then this function reduces to `passage-store-get'."
  (let* ((inhibit-message t)
         (secret (auth-source-passage-get field entry)))
    (if (not callback) secret
      (passage-store--run-show
       entry
       (lambda (_) (and secret (funcall callback secret)))))))


;;;###autoload
(defun passage-store-clear (&optional field)
  "Clear secret in the kill ring.

Optional argument FIELD, a symbol or a string, describes
the stored secret to clear; if nil, then set it to 'secret.
Note, FIELD does not affect the function logic; it is only used
to display the message:

\(message \"Field %s cleared.\" field)."
  (interactive "i")
  (unless field (setq field 'secret))
  (when passage-store-timeout-timer
    (cancel-timer passage-store-timeout-timer)
    (setq passage-store-timeout-timer nil))
  (when passage-store-kill-ring-pointer
    (setcar passage-store-kill-ring-pointer "")
    (setq passage-store-kill-ring-pointer nil)
    (message "Field %s cleared." field)))

(defun passage-store--save-field-in-kill-ring (entry secret field)
  (passage-store-clear field)
  (kill-new secret)
  (setq passage-store-kill-ring-pointer kill-ring-yank-pointer)
  (message "Copied %s for %s to the kill ring. Will clear in %s seconds."
           field entry passage-store-time-before-clipboard-restore)
  (setq passage-store-timeout-timer
        (run-at-time passage-store-time-before-clipboard-restore nil
                     (lambda () (funcall #'passage-store-clear field)))))

;;;###autoload
(defun passage-store-copy (entry)
  "Add password for ENTRY into the kill ring.

Clear previous password from the kill ring.  Pointer to the kill ring
is stored in `passage-store-kill-ring-pointer'.  Password is cleared
after `passage-store-time-before-clipboard-restore' seconds."
  (interactive (list (passage-store--completing-read t)))
  (passage-store-get
   entry
   (lambda (password)
     (passage-store--save-field-in-kill-ring entry password 'secret))))

;;;###autoload
(defun passage-store-copy-field (entry field)
  "Add FIELD for ENTRY into the kill ring.

Clear previous secret from the kill ring.  Pointer to the kill ring is
stored in `passage-store-kill-ring-pointer'.  Secret field is cleared
after `passage-store-timeout' seconds.
If FIELD equals to symbol secret, then this function reduces to `passage-store-copy'."
  (interactive
   (let ((entry (passage-store--completing-read)))
     (list entry (passage-store-read-field entry))))
  (passage-store-get-field
   entry
   field
   (lambda (secret-value)
     (passage-store--save-field-in-kill-ring entry secret-value field))))

;; XXX: not implemented in passage
;; ;;;###autoload
;; (defun passage-store-init (age-id)
;;   "Initialize new password store and use AGE-ID for encryption.
;;
;; Separate multiple IDs with spaces."
;;   (interactive (list (read-string "AGE ID: ")))
;;   (message "%s" (passage-store--run-init (split-string age-id))))

;;;###autoload
(defun passage-store-insert (entry password)
  "Insert a new ENTRY containing PASSWORD."
  (interactive (list (passage-store--completing-read)
                     (read-passwd "Password: " t)))
  (let* ((command (format "echo %s | %s insert -m -f %s"
                          (shell-quote-argument password)
                          passage-store-executable
                          (shell-quote-argument entry)))
         (ret (process-file-shell-command command)))
    (if (zerop ret)
        (message "Successfully inserted entry for %s" entry)
      (message "Cannot insert entry for %s" entry))
    nil))

;;;###autoload
(defun passage-store-generate (entry &optional passage-length)
  "Generate a new password for ENTRY with PASSAGE-LENGTH.

Default PASSAGE-LENGTH is `passage-store-passage-length'."
  (interactive (list (passage-store--completing-read)
                     (when current-prefix-arg
                       (abs (prefix-numeric-value current-prefix-arg)))))
  (unless passage-length (setq passage-length passage-store-passage-length))
  ;; A message with the output of the command is not printed because
  ;; the output contains the password.
  (passage-store--run-generate entry passage-length t)
  nil)

;;;###autoload
(defun passage-store-remove (entry)
  "Remove existing password for ENTRY."
  (interactive (list (passage-store--completing-read t)))
  (message "%s" (passage-store--run-remove entry t)))

;;;###autoload
(defun passage-store-rename (entry new-entry)
  "Rename ENTRY to NEW-ENTRY."
  (interactive (list (passage-store--completing-read t)
                     (read-string "Rename entry to: ")))
  (message "%s" (passage-store--run-rename entry new-entry t)))

;;;###autoload
(defun passage-store-version ()
  "Show version of pass executable."
  (interactive)
  (message "%s" (passage-store--run-version)))

;;;###autoload
(defun passage-store-url (entry)
  "Browse URL stored in ENTRY."
  (interactive (list (passage-store--completing-read t)))
  (let ((url (passage-store-get-field entry passage-store-url-field)))
    (if url (browse-url url)
      (error "Field `%s' not found" passage-store-url-field))))


(provide 'passage-store)

;;; passage-store.el ends here
