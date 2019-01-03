;;; -*- lexical-binding: t; -*-
;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I create a new keystore \"\\(.+\\)\" with subject \"\\(.+\\)\" and password \"\\(.+\\)\""
       (lambda (keystore-file dname keystore-password)
         (when (file-exists-p keystore-file)
           (delete-file keystore-file))
         (keystore--do-genkeypair keystore-file keystore-password "1024" 365 "root" dname)))

(Given "^I create a new keystore \"\\(.+\\)\" with subject \"\\(.+\\)\""
       (lambda (keystore-file dname)
         (when (file-exists-p keystore-file)
           (delete-file keystore-file))
         (keystore-genkeypair keystore-file "insecure" "1024" 365 "root" dname)))

(When "^I open keystore \"\\(.+\\)\" with password \"\\(.+\\)\""
      (lambda (keystore-file keystore-password)
        (list-keystore keystore-file keystore-password)))

(When "^I switch to buffer \"\\(.+\\)\""
      (lambda (buffer) (let ((buf (get-buffer buffer)))
                    (switch-to-buffer buf))))
