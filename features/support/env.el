(require 'f)
(require 'cl)

(defvar keystore-mode-support-path
  (f-dirname load-file-name))

(defvar keystore-mode-features-path
  (f-parent keystore-mode-support-path))

(defvar keystore-mode-root-path
  (f-parent keystore-mode-features-path))

(defvar keystore-mode-yes-or-no
  't
  "Stub value to return from `yes-or-no-p' and `y-or-n-p'.")

(defun keystore-mode-y-or-n-p (prompt)
  keystore-mode-yes-or-no)

(defalias 'yes-or-no-p 'keystore-mode-y-or-n-p)

(defalias 'y-or-n-p 'keystore-mode-y-or-n-p)

(add-to-list 'load-path keystore-mode-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'keystore-mode)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
