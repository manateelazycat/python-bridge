;;; python-bridge.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: python-bridge.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2023-08-12 21:08:48
;;           By: Andy Stewart
;; URL: https://github.com/manateelazycat/python-bridge
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Python-Bridge
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET python-bridge RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'python-bridge-epc)

(defgroup python-bridge nil
  "Python-Bridge group."
  :group 'applications)

(defvar python-bridge-server nil
  "The Python-Bridge Server.")

(defvar python-bridge-python-file (expand-file-name "python_bridge.py" (if load-file-name
                                                                           (file-name-directory load-file-name)
                                                                         default-directory)))

(defvar python-bridge-server-port nil)

(defun python-bridge--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p python-bridge-server)
    (setq python-bridge-server
          (python-bridge-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (python-bridge-epc-define-method mngr 'eval-in-emacs 'python-bridge--eval-in-emacs-func)
               (python-bridge-epc-define-method mngr 'get-emacs-var 'python-bridge--get-emacs-var-func)
               (python-bridge-epc-define-method mngr 'get-emacs-vars 'python-bridge--get-emacs-vars-func)
               (python-bridge-epc-define-method mngr 'get-user-emacs-directory 'python-bridge--user-emacs-directory)
               ))))
    (if python-bridge-server
        (setq python-bridge-server-port (process-contact python-bridge-server :service))
      (error "[Python-Bridge] python-bridge-server failed to start")))
  python-bridge-server)

(defun python-bridge--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun python-bridge--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun python-bridge--get-emacs-vars-func (&rest vars)
  (mapcar #'python-bridge--get-emacs-var-func vars))

(defvar python-bridge-epc-process nil)

(defvar python-bridge-internal-process nil)
(defvar python-bridge-internal-process-prog nil)
(defvar python-bridge-internal-process-args nil)

(defcustom python-bridge-name "*python-bridge*"
  "Name of Python-Bridge buffer."
  :type 'string)

(defcustom python-bridge-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run python_bridge.py."
  :type 'string)

(defcustom python-bridge-enable-debug nil
  "If you got segfault error, please turn this option.
Then Python-Bridge will start by gdb, please send new issue with `*python-bridge*' buffer content when next crash."
  :type 'boolean)

(defcustom python-bridge-enable-log nil
  "Enable this option to print log message in `*python-bridge*' buffer, default only print message header."
  :type 'boolean)

(defcustom python-bridge-enable-profile nil
  "Enable this option to output performance data to ~/python-bridge.prof."
  :type 'boolean)

(defun python-bridge--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun python-bridge-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (python-bridge-epc-live-p python-bridge-epc-process)
      (python-bridge-deferred-chain
       (python-bridge-epc-call-deferred python-bridge-epc-process (read method) args))
    (setq python-bridge-first-call-method method)
    (setq python-bridge-first-call-args args)
    ))

(defun python-bridge-call--sync (method &rest args)
  "Call Python EPC function METHOD and ARGS synchronously."
  (if (python-bridge-epc-live-p python-bridge-epc-process)
      (python-bridge-epc-call-sync python-bridge-epc-process (read method) args)
    (message "python-bridge not started.")
    ))

(defvar python-bridge-first-call-method nil)
(defvar python-bridge-first-call-args nil)

(defun python-bridge-restart-process ()
  "Stop and restart Python-Bridge process."
  (interactive)
  (python-bridge-kill-process)
  (python-bridge-start-process)
  (message "[Python-Bridge] Process restarted."))

(defun python-bridge-start-process ()
  "Start Python-Bridge process if it isn't started."
  (if (python-bridge-epc-live-p python-bridge-epc-process)
      (remove-hook 'post-command-hook #'python-bridge-start-process)
    ;; start epc server and set `python-bridge-server-port'
    (python-bridge--start-epc-server)
    (let* ((python-bridge-args (append
                                (list python-bridge-python-file)
                                (list (number-to-string python-bridge-server-port))
                                (when python-bridge-enable-profile
                                  (list "profile"))
                                )))

      ;; Set process arguments.
      (if python-bridge-enable-debug
          (progn
            (setq python-bridge-internal-process-prog "gdb")
            (setq python-bridge-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" python-bridge-python-command) python-bridge-args)))
        (setq python-bridge-internal-process-prog python-bridge-python-command)
        (setq python-bridge-internal-process-args python-bridge-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq python-bridge-internal-process
              (apply 'start-process
                     python-bridge-name python-bridge-name
                     python-bridge-internal-process-prog python-bridge-internal-process-args)))
      (set-process-query-on-exit-flag python-bridge-internal-process nil))))

(defvar python-bridge-stop-process-hook nil)

(defun python-bridge-kill-process ()
  "Stop Python-Bridge process and kill all Python-Bridge buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'python-bridge-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (python-bridge--kill-python-process))

(add-hook 'kill-emacs-hook #'python-bridge-kill-process)

(defun python-bridge--kill-python-process ()
  "Kill Python-Bridge background python process."
  (when (python-bridge-epc-live-p python-bridge-epc-process)
    ;; Cleanup before exit Python-Bridge server process.
    (python-bridge-call-async "cleanup")
    ;; Delete Python-Bridge server process.
    (python-bridge-epc-stop-epc python-bridge-epc-process)
    ;; Kill *python-bridge* buffer.
    (when (get-buffer python-bridge-name)
      (kill-buffer python-bridge-name))
    (setq python-bridge-epc-process nil)
    (message "[Python-Bridge] Process terminated.")))

(defun python-bridge--first-start (python-bridge-epc-port)
  "Call `python-bridge--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq python-bridge-epc-process (make-python-bridge-epc-manager
                                   :server-process python-bridge-internal-process
                                   :commands (cons python-bridge-internal-process-prog python-bridge-internal-process-args)
                                   :title (mapconcat 'identity (cons python-bridge-internal-process-prog python-bridge-internal-process-args) " ")
                                   :port python-bridge-epc-port
                                   :connection (python-bridge-epc-connect "127.0.0.1" python-bridge-epc-port)
                                   ))
  (python-bridge-epc-init-epc-layer python-bridge-epc-process)

  (when (and python-bridge-first-call-method
             python-bridge-first-call-args)
    (python-bridge-deferred-chain
     (python-bridge-epc-call-deferred python-bridge-epc-process
                                      (read python-bridge-first-call-method)
                                      python-bridge-first-call-args)
     (setq python-bridge-first-call-method nil)
     (setq python-bridge-first-call-args nil)
     ))

  (message "*******"))

(defun python-bridge-enable ()
  (add-hook 'post-command-hook #'python-bridge-start-process))

(provide 'python-bridge)

;;; python-bridge.el ends here
