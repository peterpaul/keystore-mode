;;; keytool-mode.el --- A major mode for displaying JKS keystore
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Peterpaul Taekele Klein Haneveld

;; Author: Peterpaul Taekele Klein Haneveld <pp.kleinhaneveld@gmail.com>
;; Version: 0.0.1
;; Keywords: keytool tools
;; Package-Requires: (origami s)

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
  (seq-reduce (lambda (x y) (format "%s\\|%s" x y))
              (cdr list-of-strings)
              (car list-of-strings)))

(setq keytool-highlights
      (list (cons (keytool-reduce-list-of-strings-to-regex keytool-syntax-constants)
                  font-lock-constant-face)
            (cons (keytool-reduce-list-of-strings-to-regex keytool-syntax-keywords)
                  font-lock-keyword-face)))

(define-derived-mode keytool-mode special-mode "keytool"
  (define-key keytool-mode-map (kbd "<tab>") 'origami-recursively-toggle-node)
  (define-key keytool-mode-map (kbd "c") 'keytool-changealias)
  (define-key keytool-mode-map (kbd "d") 'keytool-delete)
  (define-key keytool-mode-map (kbd "D") 'keytool-delete-force)
  (define-key keytool-mode-map (kbd "e") 'keytool-exportcert)
  (define-key keytool-mode-map (kbd "g") 'keytool-list)
  (define-key keytool-mode-map (kbd "i") 'keytool-importcert)
  (define-key keytool-mode-map (kbd "I") 'keytool-importkeystore)
  (define-key keytool-mode-map (kbd "q") 'kill-this-buffer)
  (define-key keytool-mode-map (kbd "r") 'keytool-list-rfc)
  (define-key keytool-mode-map (kbd "v") 'keytool-list-verbose)
  (setq font-lock-defaults '(keytool-highlights)))

(map-put origami-parser-alist 'keytool-mode (origami-markers-parser "-----BEGIN CERTIFICATE-----" "-----END CERTIFICATE-----"))

(defun keytool-init (file)
  "Initialize a new keytool buffer with a file."
  (interactive "fKeystore File: ")
  (message "Opening keystore: '%s'" file)
  (switch-to-buffer file)
  (keytool-mode)
  (make-local-variable 'keystore-filename)
  (setq keystore-filename file) 
  (make-local-variable 'keystore-passphrase)
  (setq keystore-passphrase nil)
  (origami-mode)
  (keytool-list))

(defun keytool-get-passphrase-lazy ()
  (when (not keystore-passphrase)
    (setq keystore-passphrase
          (read-passwd (format "Enter keystore passphrase of '%s': " keystore-filename))))
  keystore-passphrase)

(defun keytool-list-style (style)
  (when keystore-filename
    (let ((inhibit-read-only t)
          (keystore-password (keytool-get-passphrase-lazy)))
      (erase-buffer)
      (insert
       (shell-command-to-string
        (format "keytool -list -keystore '%s' -storepass '%s' %s"
                keystore-filename
                keystore-password
                style)))
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil t)
        (replace-match "\n\n" nil nil))
      (goto-char (point-min))
      (origami-close-all-nodes (get-buffer keystore-filename)))))

(defun keytool-list ()
  (interactive)
  (keytool-list-style ""))

(defun keytool-list-verbose ()
  (interactive)
  (keytool-list-style "-v"))

(defun keytool-list-rfc ()
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
                               (format "keytool -importcert -keystore '%s' -storepass '%s' -alias '%s' -noprompt"
                                       keystore-file
                                       keystore-pass
                                       cert-alias)))
    (keytool-list)))

(defun keytool--parse-alias-from-line-at-pos (pos)
  "Try to parse an aliase from the line at POS.

This function changes the position of the point, so wrap calls to this in `save-excursion'"
  (let* ((beg (progn (beginning-of-line) (point)))
         (end (progn (end-of-line) (point)))
         (line (buffer-substring-no-properties beg end)))
    (if (s-starts-with? "Alias name: " line)
        (s-replace-regexp "Alias name: " "" line)
      (if (s-contains? "," line)
          (substring line 0 (s-index-of "," line))
        nil))))

(defun keytool-delete (pos)
  "Delete the keystore entry at point POS."
  (interactive "d")
  (save-excursion
    (let* ((alias (or (keytool--parse-alias-from-line-at-pos pos)
                     (error "Current line does not contain an alias")))
           (keystore-pass (read-passwd (format "Enter keystore passphrase to delete certificate '%s' from '%s': "
                                               alias
                                               keystore-filename))))
      (shell-command (format "keytool -delete -keystore '%s' -storepass '%s' -alias '%s'"
                             keystore-filename
                             keystore-pass
                             alias))
      (keytool-list))))

(defun keytool-delete-force (pos)
  "Delete the keystore entry at point POS."
  (interactive "d")
  (save-excursion
    (let* ((alias (or (keytool--parse-alias-from-line-at-pos pos)
                     (error "Current line does not contain an alias"))))
      (shell-command (format "keytool -delete -keystore '%s' -storepass '%s' -alias '%s'"
                             keystore-filename
                             keystore-passphrase
                             alias))
      (keytool-list))))

(defun keytool-changealias (pos destalias)
  "Move an existing keystore entry from the line at POS to DESTALIAS."
  (interactive "d\nsDestination alias: ")
  (save-excursion
    (let* ((alias (or (keytool--parse-alias-from-line-at-pos pos)
                     (error "Current line does not contain an alias")))
           (keystore-pass (read-passwd (format "Enter keystore passphrase to change alias '%s' to '%s' in '%s': "
                                               alias
                                               destalias
                                               keystore-filename))))
      (shell-command (format "keytool -changealias -keystore '%s' -storepass '%s' -alias '%s' -destalias '%s'"
                             keystore-filename
                             keystore-pass
                             alias
                             destalias))
      (keytool-list))))

(defun keytool-exportcert (pos)
  "Export the certificate from the line at POS."
  (interactive "d")
  (save-excursion
    (let* ((alias (or (keytool--parse-alias-from-line-at-pos pos)
                     (error "Current line does not contain an alias")))
           (cert-buffer (get-buffer-create (format "%s.pem" alias))))
      (shell-command (format "keytool -exportcert -keystore '%s' -storepass '%s' -alias '%s' -rfc"
                             keystore-filename
                             keystore-passphrase
                             alias) cert-buffer))))

(defun keytool-importkeystore (srckeystore)
  "Import an entire keystore into this one."
  (interactive "fKeystore to import: ")
  (let ((srcstorepass (read-passwd (format "Enter keystore passphrase of '%s': " srckeystore)))
        (deststorepass (read-passwd (format "Enter keystore passphrase of '%s': " keystore-filename))))
    (shell-command (format "keytool -importkeystore -srckeystore '%s' -srcstorepass '%s' -destkeystore '%s' -deststorepass '%s' -noprompt"
                           srckeystore
                           srcstorepass
                           keystore-filename
                           deststorepass))
    (keytool-list)))

(provide 'keytool-mode)

;;; keytool-mode.el ends here
