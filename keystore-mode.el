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

(defgroup keystore-mode nil
  "Manage JKS/PKCS12 keystores."
  :group 'data)

(defcustom keystore-default-storetype "JKS"
  "The default keystore type to use when it could not be determined from the filename extension."
  :type '(choice (const :tag "JKS" "JKS")
                 (const :tag "PKCS12" "PKCS12"))
  :group 'keystore-mode)

(defcustom keystore-validate-passwords t
  "When t, validate passwords entered for existing keystores."
  :type 'boolean
  :group 'keystore-mode)

(defcustom keystore-display-columns `(("fingerprint (SHA256)" 64 nil)
                                      ;;("fingerprint (SHA1)" 40 nil)
                                      ("type"        20 t)
                                      ("alias"       64 t))
  "The columns to display in the keystore overview."
  :type '(repeat (list (string :tag "Column name")
                       (integer :tag "Column width")
                       (choice (boolean :tag "Sort by value?")
                               (function :tag "Sorting predicate"))))
  :group 'keystore-mode)

(defcustom keystore-column-name-to-key-name-alist `(("fingerprint (SHA256)" . "SHA256")
                                                    ("fingerprint (SHA1)" . "SHA1")
                                                    ("type" . "Entry type")
                                                    ("alias" . "Alias name"))
  "Mapping of column display names, as defined in `keystore-display-columns', to key names as parsed from the output of `keytool -list -v'."
  :type '(alist :key-type (string :tag "Column name")
                :value-type (string :tag "Column key"))
  :group 'keystore-mode)

(defcustom keystore-column-name-to-formatter-alist `(("fingerprint (SHA256)" . keystore--formatter-remove-colons)
                                                     ("fingerprint (SHA1)" . keystore--formatter-remove-colons))
  "Mapping of column display names, as defined in `keystore-display-columns', to formatter functions for display purposes."
  :type '(alist :key-type (string :tag "Column name")
                :value-type (function :tag "Formatter function"))
  :group 'keystore-mode)

(defcustom keystore-column-keys nil
  "Discovered column keys.  This variable only exists for display purposes, changing it has no effect."
  :type '(repeat string)
  :group 'keystore-mode)

(defun keystore--formatter-remove-colons (string)
  "Remove all colons (':') from STRING."
  (s-replace ":" "" string))

(defun keystore--validate-password (password)
  "Try PASSWORD on keystore buffer by listing the keystore contents.
Return PASSWORD if accepted, otherwise nil."
  (let ((inhibit-message t))
    (condition-case nil
        (progn
          (and keystore-validate-passwords
               (keystore-command "keytool"
                                 nil
                                 "-list"
                                 (keystore--arg-keystore buffer-file-name password)))
          password)
      (error nil))))

(defun keystore-get-passphrase-lazy ()
  "Get the keystore passphrase lazily.
This function checks the (buffer local) variable `keystore-passphrase' for the
presence of a passphrase.  When the passphrase is not defined, the user is
prompted to enter it, and the passphrase is stored in `keystore-passphrase'.

The entered password is validated.  When the password is not accepted, the
user is given the choice to try again.  When the user does not try again, an
error is raised.

Returns the value of `keystore-passphrase'."
  (unless (equalp major-mode 'keystore-mode)
    (error "Major mode of buffer `%s' is not `keystore-mode', but `%s'" (current-buffer) major-mode))
  (unless (boundp 'keystore-passphrase)
    (setq-local keystore-passphrase nil))
  (while (not keystore-passphrase)
    (setq keystore-passphrase
          (keystore--validate-password
           (read-passwd (format "Enter keystore passphrase of '%s': "
                                buffer-file-name))))
    (unless keystore-passphrase
      (and (not (y-or-n-p "Entered password is not accepted, try again? "))
           (error "Entered password is not accepted"))))
  keystore-passphrase)

(defun keystore--flatten-list (list)
  "Flatten LIST."
  (apply #'append
         (mapcar (lambda (x) (cond
                              ((listp x) (keystore--flatten-list x))
                              (t         (list x))))
                 list)))

(defun keystore--buffer-string (buffer)
  "Return BUFFER content as string."
  (with-current-buffer buffer
    (buffer-string)))

(defun keystore-command/execute (command arguments &optional target-buffer input-file)
  "Execute COMMAND with ARGUMENTS.
Output is redirected to TARGET-BUFFER when non-nil, otherwise
output is captured in a temporary buffer so that it can be used
for error reporting.  Command input is taken from INPUT-FILE or
/dev/null.

When COMMAND exits with a non-zero exit code, an error is raised
with standard output and standard error concatenated."
  (let ((keytool-errors (make-temp-file "keytool-errors")))
    (unwind-protect
        (with-temp-buffer
          (let* ((destination (list (or target-buffer (current-buffer)) keytool-errors)))
            (unless (eq 0 (apply #'call-process command input-file destination nil (keystore--flatten-list arguments)))
              (let ((output (keystore--buffer-string (or target-buffer (current-buffer))))
                    (errors (with-temp-buffer
                              (insert-file-contents-literally keytool-errors)
                              (buffer-string))))
                (error "%s" (s-trim (format "%s\n%s" output errors)))))))
      (delete-file keytool-errors))))

(defun keystore-command (command &optional target-buffer &rest arguments)
  "Execute COMMAND synchronously, and pass output to TARGET-BUFFER.
COMMAND is executed with ARGUMENTS.  When COMMAND exits with a
non-zero exit code, an error is raised with standard output and
standard error concatenated.

When TARGET-BUFFER is passed, standard output is redirected to
that buffer."
  (keystore-command/execute command arguments target-buffer))

(defun keystore-command-on-region (command beg end &optional target-buffer &rest arguments)
  "Execute COMMAND synchronously with region (BEG END) as input.
COMMAND is executed with ARGUMENTS.  When COMMAND exits with a
non-zero exit code, an error is raised with standard output and
standard error concatenated.

When TARGET-BUFFER is passed, standard output is redirected to
that buffer."
  (let ((input-file  (make-temp-file "keytool-input")))
    (unwind-protect
        (progn
          (write-region beg end input-file)
          (keystore-command/execute command arguments target-buffer input-file))
      (delete-file input-file))))

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

(defun keystore--get-current-line ()
  "Return the current line as string."
  (interactive)
  (save-excursion
    (let ((beg (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (buffer-substring beg end))))

(defun keystore--get-current-line-as-kvp ()
  "Return the current line as key-value-pair (kvp)."
  (interactive)
  (let ((kvp  (mapcar #'s-trim (s-split ": " (keystore--get-current-line)))))
    (if (= (length kvp) 2)
        (let ((key (car kvp))
              (value (cadr kvp)))
          (setq keystore-column-keys (cons key keystore-column-keys))
          (cons key value))
      nil)))

(defun keystore--replace-all (search-regexp replacement)
  "Replace all occurrences of SEARCH-REGEXP with REPLACEMENT in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward search-regexp nil t)
      (replace-match replacement t nil))))

(defun keystore--alist-get-value (key alist &optional default)
  "Retrieve value by KEY from ALIST, when not found return DEFAULT."
  (let ((kvp (assoc key alist)))
    (if kvp
        (cdr kvp)
      default)))

(defun keystore--get-entries ()
  "Read all keystore entries from the keystore associated with current buffer.
Return a list of lists with key-value-pairs of the form (\"KEY\" . \"VALUE\")."
  (interactive)
  (let ((buf (get-buffer-create "*keystore entries*"))
        entries
        entry-point-min
        entry-point-max)
    (with-current-buffer buf
      (erase-buffer))
    (keystore-command "keytool"
                      buf
                      "-list"
                      (keystore--arg-keystore)
                      "-v")
    (with-current-buffer buf
      (keystore--replace-all " until: " "\nValid until: ")
      (setq entry-point-max (point-max))
      (goto-char entry-point-max)
      (while (search-backward "Alias name:" nil t)
        (let (entry)
          (setq entry-point-min (point))
          (delete-non-matching-lines "^[[:space:]]*[A-Z].*: .+" entry-point-min entry-point-max)
          (goto-char (point-max))
          (while (>= (point) entry-point-min)
            (let ((line-as-kvp (keystore--get-current-line-as-kvp)))
              (when line-as-kvp
                (setq entry (cons line-as-kvp entry)))
              (previous-line)))
          (kill-region entry-point-min (point-max))
          (setq entry-point-max entry-point-min)
          (goto-char entry-point-max)
          (setq entries (cons entry entries)))))
    (kill-buffer buf)
    (setq keystore-column-keys (seq-sort #'string-lessp (seq-uniq keystore-column-keys)))
    entries))

(defun keystore--get-column-key (name)
  "Get column key by column NAME to lookup values in the keystore entries.
Return NAME when no mapping defined in `keystore-column-name-to-key-name-alist'."
  (or (cdr (assoc name keystore-column-name-to-key-name-alist))
      name))

(defun keystore--get-formatter (name)
  "Get formatter by column NAME to format values before displaying.
A formatter is a function that takes a string argument and returns a string.

Return `identity' when no mapping defined in `keystore-column-name-to-formatter-alist'."
  (or (cdr (assoc name keystore-column-name-to-formatter-alist))
      #'identity))

(defun keystore--read-entries-from-keystore ()
  "Recompute `tabulated-list-entries' from the output of 'keytool -list'."
  (setq tabulated-list-entries
        (let* ((out)
               (entry-index 0)
               (keystore-entries (keystore--get-entries)))
          (dolist (entry keystore-entries out)
            (setq entry-index (+ 1 entry-index))
            (setq out (cons (list (number-to-string entry-index)
                                  (vconcat
                                   (mapcar
                                    (lambda (x)
                                      (let* ((column-name (car x))
                                             (column-key (keystore--get-column-key column-name))
                                             (formatter (keystore--get-formatter column-name)))
                                        (apply formatter (list (keystore--alist-get-value column-key entry "")))))
                                    tabulated-list-format)))
                            out))))))

(defun keystore-toggle-mark-delete (&optional _num)
  "Mark a keystore entry for deletion and move to the next line."
  (interactive "p")
  (if (save-excursion
        (beginning-of-line)
        (eq (char-after) ?\s))
      (tabulated-list-put-tag "D" t)
    (tabulated-list-put-tag " " t)))

(defun keystore--get-alias-index ()
  "Get index of `alias' column."
  (seq-position tabulated-list-format "alias"
                (lambda (x y) (s-equals-p (car x) y))))

(defun keystore--get-alias (&optional entry)
  "Retrieve alias for keystore ENTRY or current position."
  (elt (or entry
           (tabulated-list-get-entry (point)))
       (keystore--get-alias-index)))

(defun keystore-render ()
  "Render the keystore buffer, move point to the beginning of the buffer."
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
          (setq keystore-entry (tabulated-list-get-entry (point)))
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
        (keystore-command "keytool"
                          buf
                          "-list"
                          (keystore--arg-keystore keystore storepass)
                          style)
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
          (keystore-pass (keystore-get-passphrase-lazy)))
      (with-current-buffer cert-buffer
        (keystore-command-on-region "keytool"
                                    (point-min)
                                    (point-max)
                                    nil
                                    "-importcert"
                                    (keystore--arg-keystore keystore-file
                                                            keystore-pass)
                                    "-alias" cert-alias
                                    "-noprompt")))
    (keystore-render)))

(defun keystore-importcert-file (cert-file cert-alias)
  "Import certificate from CERT-FILE with alias CERT-ALIAS."
  (interactive "fFile with certificate to import: \nsSet alias for certificate: ")
  (when (y-or-n-p (format "Are you sure you want to import '%s' with alias '%s'? "
                          cert-file
                          cert-alias))
    (backup-buffer)
    (keystore-command "keytool"
                      nil
                      "-importcert"
                      (keystore--arg-keystore)
                      "-alias" cert-alias
                      "-file" cert-file
                      "-noprompt"))
  (keystore-render))

(defun keystore--do-delete (keystore storepass alias)
  "Delete entry from KEYSTORE with STOREPASS by ALIAS."
  (keystore-command "keytool"
                    nil
                    "-delete"
                    (keystore--arg-keystore keystore storepass)
                    "-alias" alias))

(defun keystore-changealias (pos destalias)
  "Rename keystore entry at POS to DESTALIAS."
  (interactive "d\nsDestination alias: ")
  
  (save-excursion
    (let ((alias (keystore--get-alias)))
      (when (y-or-n-p (format "Are you sure you want to change alias '%s' to '%s'? "
                              alias
                              destalias))
        (backup-buffer)
        (keystore-command "keytool"
                          nil
                          "-changealias"
                          (keystore--arg-keystore)
                          "-alias" alias
                          "-destalias" destalias)
        (keystore-render)))))

(defun keystore-certreq (pos csr-file)
  "Generate a Certificate Signing Request (CSR) for the entry at POS.
The CSR is saved in CSR-FILE."
  (interactive "d\nfCSR output file: ")
  (keystore-command "keytool"
                    nil
                    "-certreq"
                    "-alias" (keystore--get-alias)
                    "-file" csr-file
                    (keystore--arg-keystore)))

(defun keystore-gencert (pos csr-file)
  "Issue a certificate by the key entry at POS as a response to the certificate request CSR-FILE."
  (interactive "d\nfCSR file: ")
  (keystore-command "keytool"
                    nil
                    "-gencert"
                    "-alias" (keystore--get-alias)
                    (keystore--arg-keystore)
                    "-infile" csr-file
                    "-outfile" (format "%s.pem" csr-file)
                    "-rfc"))

(defun keystore-exportcert (pos)
  "Export the certificate from the line at POS.
Return the buffer containing the certificate."
  (interactive "d")
  (save-excursion
    (let* ((alias (keystore--get-alias))
           (cert)
           (cert-buffer (get-buffer-create (format "%s.pem" alias))))
      (keystore-command "keytool"
                        cert-buffer
                        "-exportcert"
                        (keystore--arg-keystore)
                        "-alias" alias
                        "-rfc")
      (view-buffer-other-window cert-buffer)
      cert-buffer)))

(defun keystore-empty (keystore storepass &optional prefixed)
  "Create empty KEYSTORE file with password STOREPASS.
When the KEYSTORE already exists, an error is raised.  This behaviour can be
overridden by the universal argument PREFIXED.  When the universal argument
is given, the current file is deleted.

The `keytool' command does not have a way of creating an empty keystore, so
this function works by first creating a keystore with one entry in it using
`keytool -genkeypair', and then deleting the entry using `keytool -delete'."
  (interactive
   (list (read-file-name "Keystore File: ")
         (keystore--prompt-passwd-twice "Keystore Passphrase: ")
         current-prefix-arg))
  (when (file-exists-p keystore)
    (if prefixed
        (delete-file keystore)
      (error "File '%s' already exists, not generating empty keystore" keystore)))
  (keystore-genkeypair keystore storepass "RSA" "1024" 365 "a" "CN=a")
  (with-current-buffer keystore
    (keystore--do-delete keystore storepass "a")
    (keystore-render)))

(defun keystore-printcert (pos)
  "Open a new buffer with the output of 'keytool -printcert' for entry at POS."
  (interactive "d")
  (let* ((pem-buffer (keystore-exportcert pos))
         (alias (keystore--get-alias))
         (target-buffer (get-buffer-create (format "*printcert: %s*" alias)))
         (inhibit-read-only t))
    (with-current-buffer target-buffer
      (keystore-details-mode)
      (erase-buffer))
    (with-current-buffer pem-buffer
      (keystore-command-on-region "keytool"
                                  (point-min)
                                  (point-max)
                                  target-buffer
                                  "-printcert")
      (kill-this-buffer))
    (with-current-buffer target-buffer
      (goto-char (point-min))
      (view-buffer-other-window target-buffer))))

(defun keystore-importkeystore (srckeystore srcstorepass)
  "Import SRCKEYSTORE with password SRCSTOREPASS into this keys."
  (interactive
   (list (read-file-name "Keystore to import: ")
         (read-passwd "Enter keystore passphrase")))
  (when (y-or-n-p (format "Are you sure you want to import keystore '%s' to '%s'? "
                          srckeystore
                          buffer-file-name))
    (backup-buffer)
    (keystore-command "keytool"
                      nil
                      "-importkeystore"
                      (keystore--arg-keystore srckeystore srcstorepass nil "src")
                      (keystore--arg-keystore buffer-file-name (keystore-get-passphrase-lazy) nil "dest")
                      "-noprompt")
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

(defun keystore-genseckey (keystore storepass keyalg keysize alias)
  "Generate a secure key in KEYSTORE with STOREPASS for KEYALG KEYSIZE and ALIAS."
  (interactive
   (list (read-file-name "Keystore File: ")
         (keystore--prompt-passwd-twice "Keystore Passphrase: ")
         "AES"
         (completing-read "Key Size: " '("128" "256") nil t nil nil "128")
         (read-string "Alias: ")))
  (keystore-command "keytool"
                    nil
                    "-genseckey"
                    "-keyalg" keyalg
                    "-keysize" keysize
                    "-alias" alias
                    (keystore--arg-keystore keystore storepass))
  (keystore-visit keystore storepass))

(defun keystore-genseckey-list (keyalg keysize alias)
  "Generate a secure key with KEYALG KEYSIZE and ALIAS."
  (interactive
   (list "AES"
         (completing-read "Key Size: " '("128" "256") nil t nil nil "128")
         (read-string "Alias: ")))
  (keystore-genseckey buffer-file-name (keystore-get-passphrase-lazy) keyalg keysize alias))

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
  (keystore-command "keytool"
                    nil
                    "-genkeypair"
                    "-keyalg" keyalg
                    "-keysize" keysize
                    "-validity" (number-to-string validity)
                    "-alias" alias
                    (keystore--arg-keystore keystore storepass)
                    "-dname" dname)
  (keystore-visit keystore storepass))

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
    (define-key map "Gp" #'keystore-genkeypair-list)
    (define-key map "Gs" #'keystore-genseckey-list)
    (define-key map "P" #'keystore-printcert)
    (define-key map "ib" #'keystore-importcert-buffer)
    (define-key map "if" #'keystore-importcert-file)
    (define-key map "I" #'keystore-importkeystore)
    (define-key map "l" #'keystore-list)
    (define-key map "r" #'keystore-list-rfc)
    (define-key map "R" #'keystore-revisit)
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
  (setq-local tabulated-list-format (vconcat keystore-display-columns))
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-sort-key (cons "alias" nil))
  (add-hook 'tabulated-list-revert-hook 'keystore--read-entries-from-keystore nil t)
  (add-hook 'write-contents-functions (lambda () (error "Keystore buffers are not supposed to be saved")) nil t)
  (tabulated-list-init-header)
  (run-mode-hooks))

(defun keystore-visit (file &optional password)
  "Open keystore from FILE.
Optional argument PASSWORD The password of KEYSTORE."
  (interactive "fKeystore File: ")
  ;; (message "Opening keystore: '%s'" file)
  (let ((buf (get-buffer-create file)))
    (with-current-buffer buf
      (keystore-mode)
      (setq tabulated-list-format (vconcat keystore-display-columns))
      (setq-local buffer-file-name file)
      (when password
        (setq-local keystore-passphrase password))
      (keystore--read-entries-from-keystore)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun keystore-revisit ()
  "Reload current keystore"
  (interactive)
  (keystore-visit buffer-file-name (keystore-get-passphrase-lazy)))

(provide 'keystore-mode)

;;; keystore-mode.el ends here
