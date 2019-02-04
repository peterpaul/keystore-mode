;;; keystore-mode.el --- A major mode for viewing and managing (java) keystores -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019 Peterpaul Taekele Klein Haneveld

;; Author: Peterpaul Taekele Klein Haneveld <pp.kleinhaneveld@gmail.com>
;; URL: https://github.com/peterpaul/keystore-mode
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "24.3") (origami "1.0") (s "1.12.0") (seq "2.20"))

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
(require 'keystore-details-mode)
(require 's)
(require 'seq)

(defun keystore-get-passphrase-lazy ()
  "Get the keystore passphrase lazily.
This function checks the (buffer local) variable `keystore-passphrase' for the
presence of a passphrase.  When the passphrase is not defined, the user is
prompted to enter it, and the passphrase is stored in `keystore-passphrase'.

Returns the value of `keystore-passphrase'."
  (unless (equalp major-mode 'keystore-mode)
    (error "Major mode of buffer `%s' is not `keystore-mode', but `%s'" (current-buffer) major-mode))
  (unless (boundp 'keystore-passphrase)
    (setq-local keystore-passphrase nil))
  (unless keystore-passphrase
    (setq keystore-passphrase
          (read-passwd (format "Enter keystore passphrase of '%s': "
                               buffer-file-name))))
  keystore-passphrase)

(defun keystore--prepare-record (record)
  "Convert RECORD to a table row for `tabulated-list'.
Takes a keystore entry RECORD as parsed from the output of 'keytool -list', and
transforms it to a table row for the tabulated-list."
  (let* ((alias (nth 0 record))
         (type (nth 3 record))
         (fingerprint-field (nth 4 record))
         (fingerprint (s-replace ":" ""
                                 (substring fingerprint-field
                                            (+ 2 (s-index-of ": "
                                                             fingerprint-field))))))
    (vector fingerprint type alias)))

(defun keystore--flatten-list (list)
  "Flatten LIST."
  (apply #'append
         (mapcar (lambda (x) (cond
                              ((listp x) (keystore--flatten-list x))
                              (t         (list x))))
                 list)))

(defun keystore-command (command &rest args)
  "Create commandline for COMMAND with ARGS.
Note that all lists in ARGS are flattened.  I.e.

 (keystore-command \"keytool\" \"-list\" '(\"-keystore\" \"/tmp/keystore.jks\"))

will return

 \"keytool -list -keystore /tmp/keystore.jks\""
  (mapconcat #'shell-quote-argument
             (cons command
                   (keystore--flatten-list args))
             " "))

(defun keystore--arg-keystore (&optional keystore storepass storetype prefix)
  "Create keytool argument list for KEYSTORE STOREPASS and STORETYPE.
The list will contain `-keystore', `-storepass' and `-storetype' arguments.
When STORETYPE is not passed, the type is determined from the KEYSTORE filename.

When PREFIX is set, add this as a prefix to the option names, e.g. with `src'
the keystore argument becomes `-srckeystore'."
  (let* ((keystore-filename (or keystore
                                buffer-file-name))
         (keystore-password (or storepass
                                (keystore-get-passphrase-lazy)))
         (keystore-type (or storetype
                            (keystore--storetype-from-name keystore-filename)))
         (option-prefix (or prefix "")))
    (list (format "-%skeystore" option-prefix) keystore-filename
          (format "-%sstorepass" option-prefix) keystore-password
          (format "-%sstoretype" option-prefix) keystore-type)))

(defun keystore--do-list (keystore-filename keystore-password &optional style)
  "Execute 'keytool -list' for KEYSTORE-FILENAME with KEYSTORE-PASSWORD.
You can pass an optional STYLE, which can actually be any parameter that
keytool accepts, but is typically either `-rfc' or `-v'."
  (let ((inhibit-message t))
    (shell-command-to-string
     (keystore-command "keytool"
                       "-list"
                       (keystore--arg-keystore keystore-filename keystore-password)
                       style))))

(defun keystore--read-entries-from-keystore ()
  "Recompute `tabulated-list-entries' from the output of 'keytool -list'."
  (setq tabulated-list-entries
        (let* ((out)
               (entry-index 0)
               (keystore-info (keystore--do-list buffer-file-name
                                                 (keystore-get-passphrase-lazy)))
               (keystore-entries
                (split-string (s-replace ", \n" ", " keystore-info)
                              "[\n\r]+" t)))
          (dolist (entry keystore-entries out)
            (let ((record (split-string entry "," nil " \t")))
              (when (eq (length record) 5)
                (setq entry-index (+ 1 entry-index))
                (setq out (cons (list (number-to-string entry-index)
                                      (keystore--prepare-record record))
                                out))))))))

(defun keystore-toggle-mark-delete (&optional _num)
  "Mark a keystore entry for deletion and move to the next line."
  (interactive "p")
  (if (save-excursion
        (beginning-of-line)
        (eq (char-after) ?\s))
      (tabulated-list-put-tag "D" t)
    (tabulated-list-put-tag " " t)))

(defun keystore--get-alias (id)
  "Retrieve alias for keystore entry with ID."
  (elt (cadr (assoc id tabulated-list-entries)) 2))

(defun keystore-render ()
  "Render buffer."
  (revert-buffer)
  (goto-char (point-min)))

(defun keystore-execute ()
  "Execute marked entries (i.e. deletes)."
  (interactive)
  (let (delete-list cmd keystore-entry)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (unless (eq cmd ?\s)
          ;; This is the key KEYSTORE-ENTRY.
          (setq keystore-entry (tabulated-list-get-id (point)))
          (pcase cmd
            (?D (push keystore-entry delete-list))))
        (forward-line)))
    (when (y-or-n-p (format "Are you sure you want to delete: %s? "
                            (mapcar #'keystore--get-alias delete-list)))
      (backup-buffer)
      (dolist (entry-id delete-list)
        ;; (message "Deleting: '%s'" (keystore--get-alias entry-id))
        (keystore--do-delete buffer-file-name (keystore-get-passphrase-lazy)
                             (keystore--get-alias entry-id)))
      (keystore-render))))

(defun keystore-list (&optional style)
  "Open a new buffer with the output of `keytool -list STYLE'."
  (interactive)
  (when buffer-file-name
    (let ((inhibit-read-only t)
          (keystore buffer-file-name)
          (storepass (keystore-get-passphrase-lazy))
          (buf (get-buffer-create (format "*Keystore details: %s*"
                                          buffer-file-name))))
      (with-current-buffer buf
        (keystore-details-mode)
        (erase-buffer)
        (insert (keystore--do-list keystore storepass style))
        (goto-char (point-min))
        (while (re-search-forward "\n\n+" nil t)
          (replace-match "\n\n" nil nil))
        (goto-char (point-min))
        (switch-to-buffer-other-window buf)))))

(defun keystore-list-verbose ()
  "Open a new buffer with the output of `keytool -list -v'."
  (interactive)
  (keystore-list "-v"))

(defun keystore-list-rfc ()
  "Open a new buffer with the output of `keytool -list -rfc'."
  (interactive)
  (keystore-list "-rfc"))

(defun keystore-importcert-buffer (cert-buffer cert-alias)
  "Import certificate from CERT-BUFFER with alias CERT-ALIAS."
  (interactive "bBuffer with certificate to import: \nsSet alias for certificate: ")
  (when (y-or-n-p (format "Are you sure you want to import '%s' with alias '%s'? "
                          cert-buffer
                          cert-alias))
    (backup-buffer)
    (let ((keystore-file buffer-file-name)
          (keystore-pass (keystore-get-passphrase-lazy))
          (inhibit-message t))
      (save-excursion
        (set-buffer cert-buffer)
        (shell-command-on-region
         (point-min)
         (point-max)
         (keystore-command "keytool"
                           "-importcert"
                           (keystore--arg-keystore keystore-file
                                                   keystore-pass)
                           "-alias" cert-alias
                           "-noprompt"))))
    (keystore-render)))

(defun keystore-importcert-file (cert-file cert-alias)
  "Import certificate from CERT-FILE with alias CERT-ALIAS."
  (interactive "fFile with certificate to import: \nsSet alias for certificate: ")
  (when (y-or-n-p (format "Are you sure you want to import '%s' with alias '%s'? "
                          cert-file
                          cert-alias))
    (backup-buffer)
    (let ((inhibit-message t))
      (shell-command
       (keystore-command "keytool"
                         "-importcert"
                         (keystore--arg-keystore)
                         "-alias" cert-alias
                         "-file" cert-file
                         "-noprompt"))))
  (keystore-render))

(defun keystore--do-delete (keystore storepass alias)
  "Delete entry from KEYSTORE with STOREPASS by ALIAS."
  (let ((inhibit-message t))
    (shell-command
     (keystore-command "keytool"
                       "-delete"
                       (keystore--arg-keystore keystore storepass)
                       "-alias" alias))))

(defun keystore-changealias (pos destalias)
  "Rename keystore entry at POS to DESTALIAS."
  (interactive "d\nsDestination alias: ")
  
  (save-excursion
    (let* ((alias (keystore--get-alias (tabulated-list-get-id pos)))
           (inhibit-message t))
      (when (y-or-n-p (format "Are you sure you want to change alias '%s' to '%s'? "
                              alias
                              destalias))
        (backup-buffer)
        (shell-command
         (keystore-command "keytool"
                           "-changealias"
                           (keystore--arg-keystore)
                           "-alias" alias
                           "-destalias" destalias))
        (keystore-render)))))

(defun keystore-certreq (pos csr-file)
  "Generate a Certificate Signing Request (CSR) for the entry at POS.
The CSR is saved in CSR-FILE."
  (interactive "d\nfCSR output file: ")
  (let ((alias (keystore--get-alias (tabulated-list-get-id pos)))
        (inhibit-message t))
    (shell-command
     (keystore-command "keytool"
                       "-certreq"
                       "-alias" alias
                       "-file" csr-file
                       (keystore--arg-keystore)))))

(defun keystore-gencert (pos csr-file)
  "Issue a certificate by the key entry at POS as a response to the certificate request CSR-FILE."
  (interactive "d\nfCSR file: ")
  (let ((alias (keystore--get-alias (tabulated-list-get-id pos)))
        (cert-file (format "%s.pem" csr-file))
        (inhibit-message t))
    (shell-command
     (keystore-command "keytool"
                       "-gencert"
                       "-alias" alias
                       (keystore--arg-keystore)
                       "-infile" csr-file
                       "-outfile" cert-file
                       "-rfc"))))

(defun keystore-exportcert (pos)
  "Export the certificate from the line at POS.
Returns the buffer containing the certificate."
  (interactive "d")
  (save-excursion
    (let* ((alias (keystore--get-alias (tabulated-list-get-id pos)))
           (cert-buffer (get-buffer-create (format "%s.pem" alias)))
           (inhibit-message t))
      (shell-command
       (keystore-command "keytool"
                         "-exportcert"
                         (keystore--arg-keystore)
                         "-alias" alias
                         "-rfc")
       cert-buffer)
      cert-buffer)))

(defun keystore-printcert (pos)
  "Open a new buffer with the output of 'keytool -printcert' for entry at POS."
  (interactive "d")
  (let* ((pem-buffer (keystore-exportcert pos))
         (alias (keystore--get-alias (tabulated-list-get-id pos)))
         (target-buffer (get-buffer-create (format "*printcert: %s*" alias)))
         (inhibit-read-only t)
         (inhibit-message t))
    (with-current-buffer target-buffer
      (keystore-details-mode)
      (erase-buffer))
    (with-current-buffer pem-buffer
      (shell-command-on-region
       (point-min)
       (point-max)
       (keystore-command "keytool"
                         "-printcert")
       target-buffer)
      (kill-this-buffer))
    (with-current-buffer target-buffer
      (goto-char (point-min)))))

(defun keystore-importkeystore (srckeystore srcstorepass)
  "Import SRCKEYSTORE with password SRCSTOREPASS into this keys."
  (interactive
   (list (read-file-name "Keystore to import: ")
         (read-passwd "Enter keystore passphrase")))
  (when (y-or-n-p (format "Are you sure you want to import keystore '%s' to '%s'? "
                          srckeystore
                          buffer-file-name))
    (backup-buffer)
    (let ((inhibit-message t))
      (shell-command
       (keystore-command
        "keytool"
        "-importkeystore"
        (keystore--arg-keystore srckeystore srcstorepass nil "src")
        (keystore--arg-keystore buffer-file-name (keystore-get-passphrase-lazy) nil "dest")
        "-noprompt")))
    (keystore-render)))

(defun keystore--blank-string-p (str)
  "Return t if STR is blank, empty or nil."
  (if str
      (s-matches? "^[[:space:]]*$" str)
    't))

(defun keystore--dname-prompt-element (keyname prompt &optional previous-result)
  "Prompt the user with PROMPT to enter a value for dname element KEYNAME.
Returns a string with the dname element, including the KEYNAME, like
\"CN=<value>\", or nil if the user entered a blank string.

If previous-result is not nil, then the default value is parsed from the
previous result.

TODO escape commas in the value, and unescape when parsing."
  (let* ((default-value (and previous-result
                             (nth 1 (s-split "=" previous-result))))
         (actual-prompt (if default-value
                            (format "%s[%s] " prompt default-value)
                          prompt))
         (value (read-string actual-prompt nil nil default-value)))
    (if (keystore--blank-string-p value)
        nil
      (format "%s=%s" keyname value))))

(defun keystore-ask-dname ()
  "Ask the user for dname entries and return a dname."
  (interactive)
  (let (common-name organization-unit organization-name locality-name state-name country dname dname-elements)
    (while (not (and dname
                     (y-or-n-p (format "Do you accept: '%s'? " dname))))
      (setq dname-elements
            (list
             (setq common-name (keystore--dname-prompt-element "CN" "Common Name: " common-name))
             (setq organization-unit (keystore--dname-prompt-element "OU" "Organization Unit: " organization-unit))
             (setq organization-name (keystore--dname-prompt-element "O" "Organization Name: " organization-name))
             (setq locality-name (keystore--dname-prompt-element "L" "Locality Name/City: " locality-name))
             (setq state-name (keystore--dname-prompt-element "S" "State Name/Province: " state-name))
             (setq country (keystore--dname-prompt-element "C" "2 Character Country Id: " country))))
      (setq dname-elements (seq-filter #'identity dname-elements))
      (setq dname (mapconcat #'identity dname-elements ", ")))
    dname))

(defun keystore--prompt-passwd-twice (prompt)
  "Prompt the user with PROMPT to enter a password twice.
The password is only returned when both passwords match, otherwise the user is
asked whether he wants to try again."
  (let (val1 val2)
    (while (or (not val1)
               (not (string-equal val1 val2)))
      (setq val1 (read-passwd prompt))
      (setq val2 (read-passwd (format "Repeat %s" prompt)))
      (unless (or (string-equal val1 val2)
                  (y-or-n-p "The two provided values do not match, retry? "))
        (error "The two provided values do not match")))
    val1))

(defun keystore--buffer-major-mode (buffer)
  "Return the major mode associated with BUFFER."
  (with-current-buffer buffer
    major-mode))

(defun keystore-genkeypair (keystore storepass keyalg keysize validity alias dname)
  "Generate a self-signed keypair in KEYSTORE.
Argument KEYSTORE The keystore file that will contain the generated key pair.
Argument STOREPASS The password for the target keystore.
Argument KEYALG The key algorithm.
Argument KEYSIZE The size of the generated key.
Argument VALIDITY The validity period of the certificate in days, starting now.
Argument ALIAS The alias by which the keypair is stored in the keystore.
Argument DNAME The subject distinguished name of the (self-signed) certificate."
  (interactive
   (list (read-file-name "Keystore File: ")
         (keystore--prompt-passwd-twice "Keystore Passphrase: ")
         "RSA"
         (completing-read "Key Size: " '("1024" "2048" "4096" "8192") nil t nil nil "4096")
         (read-number "Validity (Days): " 365)
         (read-string "Alias: ")
         (keystore-ask-dname)))
  (let ((inhibit-message t))
    (shell-command
     (keystore-command "keytool"
                       "-genkeypair"
                       "-keyalg" "RSA"
                       "-keysize" keysize
                       "-validity" (number-to-string validity)
                       "-alias" alias
                       (keystore--arg-keystore keystore storepass)
                       "-dname" dname)))
  (if (and (get-buffer keystore)
           (equalp (keystore--buffer-major-mode keystore) 'keystore-mode))
      (with-current-buffer (get-buffer keystore)
        (keystore-render))
    (keystore-visit keystore storepass)))

(defun keystore-genkeypair-list (keyalg keysize validity alias dname)
  "Generate a self-signed keypair in the current keystore.
Argument KEYALG The key algorithm.
Argument KEYSIZE The size of the generated key.
Argument VALIDITY The validity period of the certificate in days, starting now.
Argument ALIAS The alias by which the keypair is stored in the keystore.
Argument DNAME The subject distinguished name of the (self-signed) certificate."
  (interactive
   (list "RSA"
         (completing-read "Key Size: " '("1024" "2048" "4096" "8192") nil t nil nil "4096")
         (read-number "Validity (Days): " 365)
         (read-string "Alias: ")
         (keystore-ask-dname)))
  (keystore-genkeypair buffer-file-name (keystore-get-passphrase-lazy) keyalg keysize validity alias dname))

(defun keystore--jks? (keystore)
  "Return t when the string KEYSTORE ends with \".jks\", nil otherwise."
  (s-ends-with? ".jks" keystore t))

(defun keystore--pkcs12? (keystore)
  "Return t when the string KEYSTORE ends with \".p12\", nil otherwise."
  (s-ends-with? ".p12" keystore t))

;; TODO make customize variable
(defvar keystore-default-storetype "JKS"
  "The default keystore type to use when it could not be determined from the filename extension.")

(defun keystore--storetype-from-name (keystore)
  "Try to determine the keystore type from the KEYSTORE filename extension.
When the type cannot be determined from the extension, this function returns
`keystore-default-storetype'.

Returns \"JKS\" or \"PKCS12\"."
  (pcase keystore
    ((pred keystore--jks?)    "JKS")
    ((pred keystore--pkcs12?) "PKCS12")
    (_                       keystore-default-storetype)))

(defvar keystore-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "d" #'keystore-toggle-mark-delete)
    (define-key map "x" #'keystore-execute)
    (define-key map "c" #'keystore-changealias)
    (define-key map "e" #'keystore-exportcert)
    (define-key map "G" #'keystore-genkeypair-list)
    (define-key map "p" #'keystore-printcert)
    (define-key map "ib" #'keystore-importcert-buffer)
    (define-key map "if" #'keystore-importcert-file)
    (define-key map "I" #'keystore-importkeystore)
    (define-key map "l" #'keystore-list)
    (define-key map "r" #'keystore-list-rfc)
    (define-key map "s" #'keystore-certreq)
    (define-key map "S" #'keystore-gencert)
    (define-key map "v" #'keystore-list-verbose)
    (define-key map "q" #'kill-this-buffer)
    map)
  "Local keymap for `keystore-mode' buffers.")

(define-derived-mode keystore-mode tabulated-list-mode "keystore"
  "\\<keystore-mode-map>
\\{keystore-mode-map}"
  ;; Modifications to the keystore are made using `keytool' on a file level.
  ;; Therefor we must ensure that creating the backup does not remove the
  ;; original file.
  (setq-local backup-by-copying t)
  ;; Setup tabulated-list-mode
  (setq-local tabulated-list-format
              `[("fingerprint" 64 nil)
                ("type"        20 t)
                ("alias"       64 t)])
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-sort-key (cons "alias" nil))
  (add-hook 'tabulated-list-revert-hook 'keystore--read-entries-from-keystore nil t)
  (tabulated-list-init-header))

(defun keystore-visit (file &optional password)
  "Open keystore from FILE.
Optional argument PASSWORD The password of KEYSTORE."
  (interactive "fKeystore File: ")
  ;; (message "Opening keystore: '%s'" file)
  (let ((buf (get-buffer-create file)))
    (with-current-buffer buf
      (keystore-mode)
      (setq-local buffer-file-name file)
      (when password
        (setq-local keystore-passphrase password))
      (keystore--read-entries-from-keystore)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(provide 'keystore-mode)

;;; keystore-mode.el ends here
