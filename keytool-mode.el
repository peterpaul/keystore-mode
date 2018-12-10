;;; keytool-mode.el --- A major mode for displaying JKS keystore
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Peterpaul Taekele Klein Haneveld

;; Author: Peterpaul Taekele Klein Haneveld <pp.kleinhaneveld@gmail.com>
;; URL: https://github.com/peterpaul/keytool-mode.el
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((origami "1.0") (s "1.12.0"))

;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (C) 2018 Peterpaul Taekele Klein Haneveld
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

;;; Commentary:

;; Elisp wrapper around the java `keytool` command

;;; Code:

(require 'origami)
(require 's)

(setq keytool-syntax-keywords
      '(
        "Alias name"
        "AuthorityInfoAccess"
        "AuthorityKeyIdentifier"
        "BasicConstraints"
        "CRLDistributionPoints"
        "Certificate chain length"
        "Certificate fingerprints"
        "Certificate fingerprint"
        "CertificatePolicies"
        "CertificatePolicyId"
        "Certificate"
        "Creation date"
        "DistributionPoint"
        "Entry type"
        "ExtendedKeyUsages"
        "Extensions"
        "Issuer"
        "KeyIdentifier"
        "KeyUsage"
        "Keystore provider"
        "Keystore type"
        "ObjectId"
        "Owner"
        "PolicyQualifierInfo"
        "Serial number"
        "Signature algorithm name"
        "Subject Public Key Algorithm"
        "SubjectAlternativeName"
        "SubjectKeyIdentifier"
        "URIName"
        "Valid from"
        "Version"
        "accessLocation"
        "accessMethod"
        "qualifierID"
        "qualifier"
        "until"
        ))

(setq keytool-syntax-constants
      '(
        "[0-9A-F][0-9A-F]\\(:[0-9A-F][0-9A-F]\\)+"
        "[0-9]+\\([\\.][0-9]+\\)+"
        "[0-9A-F]+:\\( +[0-9A-F][0-9A-F]\\)+.*$"
        "caIssuers"
        "ocsp"
        "serverAuth"
        "clientAuth"
        "DigitalSignature"
        "Key_Encipherment"
        "DNSName"
        "trustedCertEntry"
        "PrivateKeyEntry"
        "MD5"
        "SHA1withRSA"
        "SHA1"
        "SHA256withRSA"
        "SHA256"
        "SHA384withRSA"
        "SHA384"
        ))

(defun keytool-reduce-list-of-strings-to-regex (list-of-strings)
  "Reduce LIST-OF-STRINGS to a regex with multiple or clauses."
  (seq-reduce (lambda (x y) (format "%s\\|%s" x y))
              (cdr list-of-strings)
              (car list-of-strings)))

(setq keytool-highlights
      (list (cons (keytool-reduce-list-of-strings-to-regex keytool-syntax-constants)
                  font-lock-constant-face)
            (cons (keytool-reduce-list-of-strings-to-regex keytool-syntax-keywords)
                  font-lock-keyword-face)))

(define-derived-mode keytool-details-mode special-mode "keytool"
  (define-key keytool-details-mode-map (kbd "<tab>") 'origami-recursively-toggle-node)
  (setq font-lock-defaults '(keytool-highlights)))

(map-put origami-parser-alist 'keytool-details-mode (origami-markers-parser "-----BEGIN CERTIFICATE-----" "-----END CERTIFICATE-----"))

(defun keytool-get-passphrase-lazy ()
  "Get keystore passphrase and remember for next time."
  (when (not keystore-passphrase)
    (setq keystore-passphrase
          (read-passwd (format "Enter keystore passphrase of '%s': " keystore-filename))))
  keystore-passphrase)

(defun keytool--record-of-size-5 (record)
  (eq 5 (length record)))

(defun keytool--prepare-record (record)
  (let* ((alias (nth 0 record))
         (type (nth 3 record))
         (fingerprint-field (nth 4 record))
         (fingerprint (s-replace ":" ""
                                 (substring fingerprint-field (+ 2 (s-index-of ": " fingerprint-field))))))
    (vector fingerprint type alias)))

(defun keytool--parse-keystore ()
  (let* ((out)
         (entry-index 0)
         (keytool-info (keytool--do-list keystore-filename (keytool-get-passphrase-lazy) ""))
         (keystore-entries
          (split-string (s-replace ", \n" ", " keytool-info)
                        "[\n\r]+" t)))
    (dolist (entry keystore-entries out)
      (let ((record (split-string entry "," nil " \t")))
        (when (keytool--record-of-size-5 record)
          (progn
            (setq entry-index (+ 1 entry-index))
            (setq out (cons (list (number-to-string entry-index) (keytool--prepare-record record))
                            out))))))))

(defun keytool--do-list (keystore-filename keystore-password style)
  (shell-command-to-string
   (format "keytool -list -keystore '%s' -storepass '%s' -storetype '%s' %s"
           keystore-filename
           keystore-password
           (keytool--storetype-from-name keystore-filename)
           style)))

(defun keytool--read-entries-from-keystore ()
  "Recompute tabulated-list-entries."
  (setq tabulated-list-entries (keytool--parse-keystore)))

(defun keytool-mark-delete (&optional _num)
  "Mark a keystore entrie for deletion and move to the next line."
  (interactive "p")
  (tabulated-list-put-tag "D" t))

(defun keytool--get-alias (id)
  "Retrieve alias for keystore entry with ID."
  (elt (cadr (assoc id tabulated-list-entries)) 2))

(defun keytool-execute ()
  "Execute marked changes (i.e. deletes)."
  (interactive)
  (let (delete-list cmd keytool-entry)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (unless (eq cmd ?\s)
          ;; This is the key KEYTOOL-ENTRY.
          (setq keytool-entry (tabulated-list-get-id))
          (cond ((eq cmd ?D)
                 (push keytool-entry delete-list))))
        (forward-line)))
    (dolist (entry-id delete-list)
      (message "Deleting: '%s'" (keytool--get-alias entry-id))
      (keytool--do-delete keystore-filename (keytool-get-passphrase-lazy) (keytool--get-alias entry-id)))
    (revert-buffer)
    (goto-char (point-min))))

(defun keytool-list-style (style)
  "Invoke `keytool -list' command with STYLE."
  (when keystore-filename
    (let ((inhibit-read-only t)
          (keystore keystore-filename)
          (storepass (keytool-get-passphrase-lazy))
          (buf (get-buffer-create (format "*Keystore details: %s*" keystore-filename))))
      (with-current-buffer buf
        (keytool-details-mode)
        (origami-mode)
        (erase-buffer)
        (insert (keytool--do-list keystore storepass style))
        (goto-char (point-min))
        (while (re-search-forward "\n\n+" nil t)
          (replace-match "\n\n" nil nil))
        (goto-char (point-min))
        (origami-close-all-nodes buf)
        (switch-to-buffer-other-window buf)
        ))))

(defun keytool-list ()
  "Invoke `keytool -list'."
  (interactive)
  (keytool-list-style ""))

(defun keytool-list-verbose ()
  "Invoke `keytool -list -v'."
  (interactive)
  (keytool-list-style "-v"))

(defun keytool-list-rfc ()
  "Invoke `keytool -list -rfc'."
  (interactive)
  (keytool-list-style "-rfc"))

(defun keytool-importcert (cert-buffer cert-alias)
  "Import certificate from CERT-BUFFER with alias CERT-ALIAS."
  (interactive "bBuffer with certificate to import: \nsSet alias for certificate: ")
  (let ((keystore-file keystore-filename)
        (keystore-pass (read-passwd (format "Enter keystore passphrase to import certificate from '%s' to '%s': "
                                            cert-buffer
                                            keystore-filename))))
    (save-excursion
      (set-buffer cert-buffer)
      (shell-command-on-region (point-min)
                               (point-max)
                               (format "keytool -importcert -keystore '%s' -storepass '%s' -storetype '%s' -alias '%s' -noprompt"
                                       keystore-file
                                       keystore-pass
                                       (keytool--storetype-from-name keystore-file)
                                       cert-alias)))
    (revert-buffer)
    (goto-char (point-min))))

(defun keytool--do-delete (keystore storepass alias)
  "Delete an entry with ALIAS from KEYSTORE with STOREPASS."
  (shell-command (format "keytool -delete -keystore '%s' -storepass '%s' -storetype '%s' -alias '%s'"
                         keystore
                         storepass
                         (keytool--storetype-from-name keystore)
                         alias)))

(defun keytool-changealias (pos destalias)
  "Move an existing keystore entry from the line at POS to DESTALIAS."
  (interactive "d\nsDestination alias: ")
  (save-excursion
    (let* ((alias (keytool--get-alias (tabulated-list-get-id)))
           (keystore-pass (read-passwd (format "Enter keystore passphrase to change alias '%s' to '%s' in '%s': "
                                               alias
                                               destalias
                                               keystore-filename))))
      (shell-command (format "keytool -changealias -keystore '%s' -storepass '%s' -storetype '%s' -alias '%s' -destalias '%s'"
                             keystore-filename
                             keystore-pass
                             (keytool--storetype-from-name keystore-filename)
                             alias
                             destalias))))
  (revert-buffer)
  (goto-char (point-min)))

(defun keytool-exportcert (pos)
  "Export the certificate from the line at POS."
  (interactive "d")
  (save-excursion
    (let* ((alias (keytool--get-alias (tabulated-list-get-id)))
           (cert-buffer (get-buffer-create (format "%s.pem" alias))))
      (shell-command (format "keytool -exportcert -keystore '%s' -storepass '%s' -storetype '%s' -alias '%s' -rfc"
                             keystore-filename
                             keystore-passphrase
                             (keytool--storetype-from-name keystore-filename)
                             alias) cert-buffer))))

(defun keytool-importkeystore (srckeystore)
  "Import SRCKEYSTORE into this one."
  (interactive "fKeystore to import: ")
  (let ((srcstorepass (read-passwd (format "Enter keystore passphrase of '%s': " srckeystore)))
        (deststorepass (read-passwd (format "Enter keystore passphrase of '%s': " keystore-filename))))
    (shell-command (format "keytool -importkeystore -srckeystore '%s' -srcstorepass '%s' -srcstoretype '%s' -destkeystore '%s' -deststorepass '%s' -deststoretype '%s' -noprompt"
                           srckeystore
                           srcstorepass
                           (keytool--storetype-from-name srckeystore)
                           keystore-filename
                           deststorepass
                           (keytool--storetype-from-name keystore-filename)))
    (revert-buffer)
    (goto-char (point-min))))

(defun keytool--do-genkeypair (keystore storepass keysize validity alias)
  (async-shell-command (format "keytool -genkeypair -keyalg RSA -keysize '%s' -validity '%s' -alias '%s' -keystore '%s' -storepass '%s' -storetype '%s'"
                               keysize
                               validity
                               alias
                               keystore
                               storepass
                               (keytool--storetype-from-name keystore-filename))))

(defun keytool-genkeypair (keystore storepass storepass-repeat keysize validity alias)
  ""
  (interactive
   (list (read-file-name "Keystore File: ")
         (read-passwd "Keystore Passphrase: ")
         (read-passwd "Keystore Passphrase (repeat): ")
         (completing-read "Key Size: " '("1024" "2048" "4096" "8192") nil t nil nil "4096")
         (read-number "Validity (Days): " 365)
         (read-string "Alias: ")))
  (if (equal storepass storepass-repeat)
      (keytool--do-genkeypair keystore storepass keysize validity alias)
    (error "The two provided Keystore Passphrases do not match")))

(defun keytool--jks? (keystore)
  (s-ends-with? ".jks" keystore t))

(defun keytool--pkcs12? (keystore)
  (s-ends-with? ".p12" keystore t))

(setq keytool-default-storetype "JKS")

(defun keytool--storetype-from-name (keystore)
  "Try to determine the keystore type from the KEYSTORE filename extension.

When the type cannot be determined from the extension, this function returns
`keytool-default-storetype'.

Returns \"JKS\" or \"PKCS12\"."
  (pcase keystore
    ((pred keytool--jks?)    "JKS")
    ((pred keytool--pkcs12?) "PKCS12")
    (_                       keytool-default-storetype)))

(defvar keytool-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "d" 'keytool-mark-delete)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "x" 'keytool-execute)
    (define-key map "c" 'keytool-changealias)
    (define-key map "e" 'keytool-exportcert)
    (define-key map "i" 'keytool-importcert)
    (define-key map "I" 'keytool-importkeystore)
    (define-key map "l" 'keytool-list)
    (define-key map "r" 'keytool-list-rfc)
    (define-key map "v" 'keytool-list-verbose)
    map)
  "Local keymap for `keytool-mode' buffers.")

(define-derived-mode keytool-mode tabulated-list-mode "keytool"
  "\\<keytool-mode-map>
\\{keytool-mode-map}"
  (setq tabulated-list-format
        `[("fingerprint" 64 nil)
          ("type"        20 t)
          ("alias"       64 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "alias" nil))
  (add-hook 'tabulated-list-revert-hook 'keytool--read-entries-from-keystore nil t)
  (tabulated-list-init-header))

(defun list-keytool (file)
  "Open keytool from FILE."
  (interactive "fKeystore File: ")
  (message "Opening keystore: '%s'" file)
  (let ((buf (get-buffer-create file)))
    (with-current-buffer buf
      (keytool-mode)
      (make-local-variable 'keystore-filename)
      (setq keystore-filename file)
      (make-local-variable 'keystore-passphrase)
      (setq keystore-passphrase nil)
      (keytool--read-entries-from-keystore)  
      (tabulated-list-print t)
      (switch-to-buffer file))))

(provide 'keytool-mode)

;;; keytool-mode.el ends here
