;;; -*- lexical-binding: t; -*-
;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^keystore \"\\(.+\\)\" does not exist"
       (lambda (keystore-file)
         (when (file-exists-p keystore-file)
           (delete-file keystore-file))))

(Given "^keystore \"\\(.+\\)\" with password \"\\(.+\\)\" and these keys:"
       (lambda (keystore-file keystore-password keys)
         (when (file-exists-p keystore-file)
           (delete-file keystore-file))
         (let* ((table keys)
                (header (car table))
                (rows (cdr table)))
           (dolist (row rows)
             (let ((alias (car row))
                   (subject (cadr row)))
               (keystore--do-genkeypair keystore-file keystore-password "1024" 365 alias subject))))))

(When "^I create a keypair with alias \"\\(.+\\)\" and subject \"\\(.+\\)\" in keystore \"\\(.+\\)\" with password \"\\(.+\\)\""
      (lambda (alias subject keystore-file keystore-password)
        (keystore-genkeypair keystore-file keystore-password "1024" 365 alias subject)))

(When "^I open keystore \"\\(.+\\)\" with password \"\\(.+\\)\""
      (lambda (keystore-file keystore-password)
        (list-keystore keystore-file keystore-password)))

(When "^I switch to buffer \"\\(.+\\)\""
      (lambda (buffer) (let ((buf (get-buffer buffer)))
                    (switch-to-buffer buf))))
