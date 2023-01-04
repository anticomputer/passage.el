;;; passage-store-otp.el --- Password store (pass) OTP extension support           -*- lexical-binding: t -*-
;;
;; Filename: passage-store-otp.el
;; Author: Daniel Barreto
;; Created: Tue Aug 22 13:46:01 2017 (+0200)
;; Version: 0.1.0
;; Package-Version: 20190713.1348
;; Package-Requires: ((emacs "25") (s "1.9.0"))
;; URL: https://github.com/volrath/passage-store-otp.el
;; Keywords: tools, pass
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This package provides functions for working with the passage-otp
;; extension for pass ("the standard Unix password manager").
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Modified from password-store-otp.el for use with https://github.com/FiloSottile/passage

;; Maintainer: Bas Alberts <bas@anti.computer>
;; Url: https://github.com/anticomputer/passage.el

;;; Code:

(require 'passage-store)
(require 'seq)
(require 's)

(defcustom passage-store-otp-screenshots-path nil
  "OTP screenshots directory."
  :group 'passage-store
  :type '(choice (const :tag "Off" nil)
                 (file :tag "Expandable file name")))

(defun passage-store-otp--get-screenshot-executable ()
  "Return the name of the executable that should be used to take screenshots."
  (if (eq window-system 'mac) "screencapture" "import"))

(defun passage-store-otp--otpauth-lines (lines)
  "Return from LINES those that are OTP urls."
  (seq-filter (lambda (l) (string-prefix-p "otpauth://" l))
              lines))

(defun passage-store-otp--get-uri (entry)
  "Own version that produces error if ENTRY has no otp uri."
  (let ((url (car (passage-store-otp--otpauth-lines
                   (s-lines (passage-store--run-show entry))))))
    (when (not url)
      (error "No OTP url found"))
    url))

(defun passage-store-otp--safe-copy (secret)
  "Add SECRET to kill ring.

Clear previous password from kill ring.  Pointer to kill ring is
stored in `passage-store-kill-ring-pointer'.  SECRET is cleared
after `passage-store-timeout' seconds."
  (passage-store-clear)
  (kill-new secret)
  (setq passage-store-kill-ring-pointer kill-ring-yank-pointer)
  (setq passage-store-timeout-timer
        (run-at-time (passage-store-timeout) nil 'passage-store-clear)))

(defun passage-store-otp--get-qr-image-filename (entry)
  "Return a qr-image-filename for given ENTRY."
  (let ((entry-base (file-name-nondirectory entry)))
    (if passage-store-otp-screenshots-path
        (let ((fname (format "%s-%s.png"
                             entry-base
                             (format-time-string "%Y-%m-%dT%T"))))
          (expand-file-name fname
                            passage-store-otp-screenshots-path))
      (format "/tmp/%s.png" (make-temp-name entry-base)))))

(defmacro passage-store-otp--related-error (&rest body)
  "Catch otp related errors in BODY and displays a better error message."
  (declare (indent defun))
  `(condition-case err
       ,@body
     (error
      (let ((error-msg (error-message-string err)))
        (if (string= error-msg "Error: otp is not in the password store.")
            (error "Error: pass extension `passage-otp' is not installed")
          (error error-msg))))))

(defun passage-store-otp-completing-read ()
  "Ask the user to select an entry from a list of all entries."
  (passage-store--completing-read))

(defun passage-store-otp-token (entry)
  "Return an OTP token from ENTRY."
  (passage-store-otp--related-error
    (passage-store--run "otp" entry)))

(defun passage-store-otp-uri (entry)
  "Return an OTP URI from ENTRY."
  (passage-store-otp--related-error
    (passage-store--run "otp" "uri" entry)))

(defun passage-store-otp-qrcode (entry &optional type)
  "Display a QR code from ENTRY's OTP, using TYPE."
  (if type
      (shell-command-to-string (format "qrencode -o - -t%s %s"
                                       type
                                       (shell-quote-argument (passage-store-otp--get-uri entry))))
    (passage-store-otp--related-error
      (passage-store--run "otp" "uri" "-q" entry))))

(defun passage-store-otp-add-uri (method entry uri)
  "Using METHOD, add in ENTRY a URI.

METHOD can be either `append' or `insert', and it will be used as the
primary \"pass otp\" command line verb."
  (unless (memq method '(append insert))
    (error (format "Unrecognized method %s" method)))
  (passage-store-otp--related-error
    (passage-store--run "otp" "--help"))  ;; make sure otp extension is installed.
  (message "%s" (shell-command-to-string (format "echo %s | %s otp %s -f %s"
                                                 (shell-quote-argument uri)
                                                 passage-store-executable
                                                 method
                                                 (shell-quote-argument entry)))))


;;; Interactive functions

;;;###autoload
(defun passage-store-otp-token-copy (entry)
  "Copy an OTP token from ENTRY to clipboard."
  (interactive (list (passage-store-otp-completing-read)))
  (passage-store-otp--safe-copy (passage-store-otp-token entry))
  (message "Copied %s to the kill ring. Will clear in %s seconds." entry (passage-store-timeout)))

;;;###autoload
(defun passage-store-otp-uri-copy (entry)
  "Copy an OTP URI from ENTRY to clipboard."
  (interactive (list (passage-store-otp-completing-read)))
  (passage-store-otp--safe-copy (passage-store-otp-uri entry))
  (message "Copied %s to the kill ring. Will clear in %s seconds." entry (passage-store-timeout)))

;;;###autoload
(defun passage-store-otp-insert (entry otp-uri)
  "Insert a new ENTRY containing OTP-URI."
  (interactive (list (passage-store-otp-completing-read)
                     (read-passwd "OTP URI: " t)))
  (passage-store-otp-add-uri 'insert entry otp-uri))

;;;###autoload
(defun passage-store-otp-append (entry otp-uri)
  "Append to an ENTRY the given OTP-URI."
  (interactive (list (passage-store-otp-completing-read)
                     (read-passwd "OTP URI: " t)))
  (passage-store-otp-add-uri 'append entry otp-uri))

;;;###autoload
(defun passage-store-otp-append-from-image (entry)
  "Check clipboard for an image and scan it to get an OTP URI, append it to ENTRY."
  (interactive (list (passage-store-otp-completing-read)))
  (let ((qr-image-filename (passage-store-otp--get-qr-image-filename entry))
        (screenshot-executable (passage-store-otp--get-screenshot-executable)))
    (when (not (zerop (call-process screenshot-executable nil nil nil qr-image-filename)))
      (error "Couldn't get image from clipboard"))
    (with-temp-buffer
      (condition-case nil
          (call-process "zbarimg" nil t nil "-q" "--raw"
                        qr-image-filename)
        (error
         (error "It seems you don't have `zbar-tools' installed")))
      (passage-store-otp-append
       entry
       (buffer-substring (point-min) (point-max))))
    (when (not passage-store-otp-screenshots-path)
      (delete-file qr-image-filename))))

(provide 'passage-store-otp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; passage-store-otp.el ends here
