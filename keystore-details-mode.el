;;; keystore-details-mode.el --- A major mode for displaying JKS keystore deatils -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019 Peterpaul Taekele Klein Haneveld

;; Author: Peterpaul Taekele Klein Haneveld <pp.kleinhaneveld@gmail.com>
;; URL: https://github.com/peterpaul/keystore-mode
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "24") (origami "1.0"))

;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (C) 2018,2019 Peterpaul Taekele Klein Haneveld
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
(require 'seq)

(defvar keystore-details-syntax-keywords
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
    )
  "List of keywords, used for syntax highlighting.")

(defvar keystore-details-syntax-constants
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
    )
  "List of constants, used for syntax highlighting.")

(defun keystore-details-reduce-list-of-strings-to-regex (list-of-strings)
  "Reduce LIST-OF-STRINGS to a regex with multiple or clauses."
  (seq-reduce (lambda (x y) (format "%s\\|%s" x y))
              (cdr list-of-strings)
              (car list-of-strings)))

(defvar keystore-details-highlights
  (list (cons (keystore-details-reduce-list-of-strings-to-regex keystore-details-syntax-constants)
              font-lock-constant-face)
        (cons (keystore-details-reduce-list-of-strings-to-regex keystore-details-syntax-keywords)
              font-lock-keyword-face))
  "Define faces for syntax highlighting.")

(define-derived-mode keystore-details-mode special-mode "keystore-details"
  (define-key keystore-details-mode-map (kbd "<tab>") 'origami-recursively-toggle-node)
  (setq font-lock-defaults '(keystore-details-highlights)))

(setq origami-parser-alist (cons `(keystore-details-mode . ,(origami-markers-parser "-----BEGIN CERTIFICATE-----" "-----END CERTIFICATE-----"))  origami-parser-alist))

(provide 'keystore-details-mode)

;;; keystore-details-mode.el ends here
