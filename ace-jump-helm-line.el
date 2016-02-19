;;; ace-jump-helm-line.el --- Ace-jump to a candidate in helm window

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; URL: https://github.com/cute-jumper/ace-jump-helm-line
;; Keywords: extensions
;; Version: 0.3.2
;; Package-Requires: ((avy "0.4.0") (helm "1.6.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Demos: see https://github.com/cute-jumper/ace-jump-helm-line

;;                           ____________________

;;                            ACE-JUMP-HELM-LINE

;;                               Junpeng Qiu
;;                           ____________________


;; Table of Contents
;; _________________

;; 1 Setup
;; 2 Usage
;; 3 Acknowledgment


;; Ace-jump to a candidate in helm window.

;; This package makes use of the `avy.el'.


;; 1 Setup
;; =======

;;   ,----
;;   | (add-to-list 'load-path "/path/to/ace-jump-helm-line.el")
;;   | (require 'ace-jump-helm-line)
;;   `----

;;   You can use the following code to bind `ace-jump-helm-line' (or
;;   `ace-jump-helm-line-execute-action'. See later) to a key(say, "C-'"):
;;   ,----
;;   | (eval-after-load "helm"
;;   | '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))
;;   `----


;; 2 Usage
;; =======

;;   When in a helm session, for example, after you call `helm-M-x', you
;;   can use your key binding(for example, "C-'") to invoke
;;   `ace-jump-helm-line'. See demos.

;;   Two commands are available:
;;   - `ace-jump-helm-line': Jump to a candidate in helm window. You can
;;   still move around in the helm window after this command.
;;   - `ace-jump-helm-line-execute-action': Jump to a candidate and
;;   automatically select the candidate to execute the persistent action.
;;   The helm session will end after this command.

;;   There are two kinds of styles: avy style and ace-jump-mode style.
;;   By default, this package uses `avy' style(anyway, it uses
;;   `avy.el'!). You can certainly change to `ace-jump-mode-style' by:
;;   ,----
;;   | (setq ace-jump-helm-line-use-avy-style nil)
;;   `----

;;   *UPDATE(11/08/2015)*: As `avy' has a way to configure the keys and
;;   styles for the `avy-*' commands(see `avy-keys-alist' and
;;   `avy-styles-alist'), I add following three variables:
;;   1. `ace-jump-helm-line-keys': You can set the keys to be used for
;;   `ace-jump-helm-line'.
;;   2. `ace-jump-helm-line-style': You can set the styles to be used for
;;   `ace-jump-helm-line'. The values can be `pre', `at', `at-full',
;;   `post', and `de-bruijn', the same as `avy-style'.
;;   3. `ace-jump-helm-line-background': Whether you want to use a background
;;   or not.

;;   You can set the values of the above three variables to overwrite the
;;   effect of `ace-jump-helm-line-use-avy-style'.
;;   `ace-jump-helm-line-use-avy-style' is *obsolete* now!

;; 3 Acknowledgment
;; ================

;;   - Thank [Oleh Krehel] for the awesome [avy] package.
;;   - Thank @hick for the original idea.


;;   [Oleh Krehel] https://github.com/abo-abo/

;;   [avy] https://github.com/abo-abo/avy

;;; Code:

(require 'avy)
(require 'helm)

(defvar ace-jump-helm-line-keys nil
  "Keys used for `ace-jump-helm-line'.")
(defvar ace-jump-helm-line-style nil
  "Style used for `ace-jump-helm-line'.")
(defvar ace-jump-helm-line-background nil
  "Use background or not in `ace-jump-helm-line'.")

(defvar ace-jump-helm-line-use-avy-style t
  "Useless variable since v0.4.
Please set `ace-jump-helm-line-keys', `ace-jump-helm-line-style'
and `ace-jump-helm-line-background' instead.")

(defvar ace-jump-helm-line-persistent-key nil
  "The key to perform persistent action.")

(defvar ace-jump-helm-line-select-key nil
  "The key to select.
Used for `ace-jump-helm-line'.")

(defvar ace-jump-helm-line-move-only-key nil
  "The key to only move the selection.
 Used for `ace-jump-helm-line-and-select'.")

(defvar ace-jump-helm-line-default-action nil
  "The default action when jumping to a candidate.")

(defvar ace-jump-helm-line--action-type nil)

(defun ace-jump-helm-line-action-persistent (pt)
  (goto-char pt)
  (setq ace-jump-helm-line--action-type 'persistent)
  (ace-jump-helm-line--move-selection)
  (helm-execute-persistent-action))

(defun ace-jump-helm-line-action-select (pt)
  (goto-char pt)
  (setq ace-jump-helm-line--action-type 'select)
  (ace-jump-helm-line--move-selection)
  (helm-exit-minibuffer))

(defun ace-jump-helm-line-action-move-only (pt)
  (goto-char pt)
  (setq ace-jump-helm-line--action-type 'move-only)
  (ace-jump-helm-line--move-selection))

(defun ace-jump-helm-line--move-selection ()
  (let ((orig-point (point)))
    (helm-move-selection-common :where 'line :direction 'previous)
    (unless (= (point) orig-point)
      (helm-move-selection-common :where 'line :direction 'next))))

(defun ace-jump-helm-line--get-dispatch-alist ()
  (when (boundp 'avy-dispatch-alist)
    (let* ((default-action (or ace-jump-helm-line-default-action
                               'move-only))
           (full-list (list 'persistent 'select 'move-only))
           (action-list (delete default-action full-list))
           dispatch-alist)
      (dolist (w action-list dispatch-alist)
        (let ((key-sym (intern (format "ace-jump-helm-line-%s-key" w)))
              (action-sym (intern (format "ace-jump-helm-line-action-%s" w))))
          (eval `(and ,key-sym
                      (push (cons ,key-sym ',action-sym) dispatch-alist))))))))

(defun ace-jump-helm-line--collect-lines ()
  "Select lines in helm window."
  (let (candidates)
    (save-excursion
      (save-restriction
        (narrow-to-region (window-start) (window-end (selected-window) t))
        (goto-char (point-min))
        (while (or (helm-pos-header-line-p)
                   (helm-pos-candidate-separator-p))
          (forward-line 1))
        (while (< (point) (point-max))
          (push (cons (point) (selected-window))
                candidates)
          (forward-line 1)
          (while (and (or (helm-pos-header-line-p)
                          (helm-pos-candidate-separator-p))
                      (< (point) (point-max)))
            (forward-line 1)))))
    (nreverse candidates)))

(defun ace-jump-helm-line--do ()
  (if helm-alive-p
      (let* ((orig-window (selected-window))
             (avy-background ace-jump-helm-line-background)
             (avy-keys (or ace-jump-helm-line-keys
                           avy-keys))
             (avy-dispatch-alist (ace-jump-helm-line--get-dispatch-alist))
             avy-action
             avy-all-windows)
        (unwind-protect
            (with-selected-window (helm-window)
              (avy--process (ace-jump-helm-line--collect-lines)
                            (avy--style-fn
                             (or ace-jump-helm-line-style
                                 avy-style)))
              (or avy-action
                  (ace-jump-helm-line--move-selection))))
        (select-window orig-window))
    (error "No helm session is running")))

(defun ace-jump-helm-line--post ()
  (and helm-alive-p
       (eq ace-jump-helm-line-default-action
           ace-jump-helm-line--action-type)
       (cond
        ((eq ace-jump-helm-line-default-action 'select)
         (helm-maybe-exit-minibuffer))
        ((eq ace-jump-helm-line-default-action 'persistent)
         (helm-execute-persistent-action)))))

;;;###autoload
(defun ace-jump-helm-line ()
  "Jump to a candidate and execute the default action."
  (interactive)
  (let ((ace-jump-helm-line--action-type
         ace-jump-helm-line-default-action))
    (ace-jump-helm-line--do)
    (ace-jump-helm-line--post)))

;;;###autoload
(defun ace-jump-helm-line-and-select ()
  "Jump to and select the candidate in helm window."
  (interactive)
  (let ((ace-jump-helm-line-default-action 'select))
    (ace-jump-helm-line)))

;;;###autoload
(defalias 'ace-jump-helm-line-execute-action 'ace-jump-helm-line-and-select)

(make-obsolete-variable 'ace-jump-helm-line-use-avy-style nil "0.4")

(provide 'ace-jump-helm-line)
;;; ace-jump-helm-line.el ends here
