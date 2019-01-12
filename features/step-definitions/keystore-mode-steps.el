;;; -*- lexical-binding: t; -*-
;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(defun keystore-steps-delete-file-if-exists (file)
  "Delete FILE if it exists."
  (when (file-exists-p file)
    (delete-file file)))

(defun keystore-steps-generate-keypairs-from-table (keystore-file keystore-password keys)
  "Execute 'keytool -genkeypair' on KEYSTORE-FILE with KEYSTORE-PASSWORD for KEYS.

KEYS is a table with two columns: 'alias' and 'subject'."
  (let* ((table keys)
         (header (car table))
         (rows (cdr table)))
    (dolist (row rows)
      (let ((alias (car row))
            (subject (cadr row)))
        (keystore--do-genkeypair keystore-file keystore-password "RSA" "1024" 365 alias subject)))))

(Given "^keystore \"\\(.+\\)\" does not exist$"
       'keystore-steps-delete-file-if-exists)

(Given "^I open keystore \"\\(.+\\)\" with password \"\\(.+\\)\" and these keys:$"
       (lambda (keystore-file keystore-password keys)
         (keystore-steps-delete-file-if-exists keystore-file)
         (keystore-steps-generate-keypairs-from-table keystore-file keystore-password keys)
         (list-keystore keystore-file keystore-password)))

(Given "^keystore \"\\(.+\\)\" with password \"\\(.+\\)\" and these keys:$"
       (lambda (keystore-file keystore-password keys)
         (keystore-steps-delete-file-if-exists keystore-file)
         (keystore-steps-generate-keypairs-from-table keystore-file keystore-password keys)))

(When "^I create a keypair with alias \"\\(.+\\)\" and subject \"\\(.+\\)\"$"
      (lambda (alias subject)
        (keystore-genkeypair-list "RSA" "1024" 365 alias subject)))

(When "^I create a keypair with alias \"\\(.+\\)\" and subject \"\\(.+\\)\" in keystore \"\\(.+\\)\" with password \"\\(.+\\)\"$"
      (lambda (alias subject keystore-file keystore-password)
        (keystore-genkeypair keystore-file keystore-password "RSA" "1024" 365 alias subject)))

(When "^I open keystore \"\\(.+\\)\" with password \"\\(.+\\)\"$"
      (lambda (keystore-file keystore-password)
        (list-keystore keystore-file keystore-password)))

(When "^I switch to buffer \"\\(.+\\)\"$"
      (lambda (buffer) (let ((buf (get-buffer buffer)))
                    (switch-to-buffer buf))))

(Then "^buffer \"\\(.+\\)\" should exist$"
      (lambda (buffer)
        (let ((message "Expected buffer '%s' to exist, but all I got was %s"))
          (cl-assert (get-buffer buffer) nil message buffer (buffer-list)))))

(Then "^buffer \"\\(.+\\)\" should contain\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (buffer expected)
        (Then (format "buffer \"%s\" should exist" buffer))
        (let ((actual (save-excursion
                        (switch-to-buffer (get-buffer buffer))
                        (buffer-string)))
              (message "Expected\n%s\nto be part of:\n%s"))
          (cl-assert (s-contains? expected actual) nil message expected actual))))

(Then "^buffer \"\\(.+\\)\" should contain pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (buffer expected)
        (Then (format "buffer \"%s\" should exist" buffer))
        (let ((actual (save-excursion
                        (switch-to-buffer (get-buffer buffer))
                        (buffer-string)))
              (message "Expected to see pattern '%s' in '%s', but did not."))
          (cl-assert
           (s-matches? expected actual) nil message expected actual))))
