;;; ac-cider-compliment.el --- auto-complete sources for Clojure using
;;; CIDER and compliment

;; Copyright (C) 2012-2014 Alex Yakushev <alex@bytopia.org>

;; Author: Alex Yakushev <alex@bytopia.org>
;;         Steve Purcell <steve@sanityinc.com>
;;         Sam Aaron <samaaron@gmail.com>
;;
;; URL: https://github.com/alexander-yakushev/ac-cider-compliment
;; Keywords: languages, clojure, nrepl, cider, compliment
;; Version: 0.1.0
;; Package-Requires: ((cider "0.5.0") (auto-complete "1.4"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a number of auto-complete sources for Clojure projects
;; using CIDER and compliment. This is a fork of ac-nrepl project by
;; Steve Purcell, with the difference that ac-cider-compliment uses
;; Compliment as a completion provider.

;;; Installation:

;; Available as a package in melpa.milkbox.net.
;; M-x package-install ac-cider-compliment

;;; Usage:

;;     (load "path/to/ac-nrepl-compliment.el")
;;     (require 'ac-cider-compliment)
;;     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;     (add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'cider-mode))

;; If you want to trigger auto-complete using TAB in CIDER buffers, you may
;; want to use auto-complete in your `completion-at-point-functions':

;;     (defun set-auto-complete-as-completion-at-point-function ()
;;       (setq completion-at-point-functions '(auto-complete)))
;;     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;;     (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;; You might consider using ac-cider-compliment's popup documentation in place of `nrepl-doc':
;;
;;     (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-cider-compliment-popup-doc)

;;; Code:

(require 'cider)
(require 'auto-complete)

(defvar ac-cider-compliment-core-required ())

(defun ac-cider-compliment-require-core ()
  (let* ((t-session (nrepl-current-tooling-session))
        (status (cdr (assoc t-session ac-cider-compliment-core-required))))
    (if status
        status
      (let ((status
             (if (string= (plist-get
                           (ac-cider-compliment-sync-eval
                            "(try (require 'compliment.core) (catch Exception e :error))")
                           :value)
                          "nil")
                 t :na)))
        (add-to-list 'ac-cider-compliment-core-required (cons t-session status))
        status))))

(defun ac-cider-compliment-available-p ()
  "Return t if nrepl is available for completion, otherwise nil."
  (when (eq (ac-cider-compliment-require-core) t)
    t))

(defun ac-cider-compliment-sync-eval (clj)
  "Synchronously evaluate CLJ.
Result is a plist, as returned from `nrepl-send-string-sync'."
  (nrepl-send-string-sync clj (cider-current-ns) (nrepl-current-tooling-session)))

(defun ac-cider-compliment-candidates* (clj)
  "Return completion candidates produced by evaluating CLJ."
  (when (eq (ac-cider-compliment-require-core) t)
    (let* ((response (plist-get (ac-cider-compliment-sync-eval clj) :value)))
      (when response
        (car (read-from-string response))))))

(defvar ac-cider-compliment-last-context nil)

(defun ac-cider-compliment-get-context-at-point ()
  "Extract the context at point. If point is not inside the list,
returns nil; otherwise return top-level form, with symbol at
point replaced by __prefix__."
  (when (save-excursion
          (condition-case foo
              (progn
                (up-list)
                (check-parens)
                t)
            (scan-error nil)
            (user-error nil)))
    (save-excursion
      (let* ((pref-end (point))
             (pref-start (ac-cider-compliment-symbol-start-pos))
             (context (cider-defun-at-point))
             (_ (beginning-of-defun))
             (expr-start (point)))
        (replace-regexp-in-string
         "}" ")"
         (replace-regexp-in-string
          "{" "(compliment-hashmap "
          (concat (substring context 0 (- pref-start expr-start))
                  "__prefix__"
                  (substring context (- pref-end expr-start)))))))))

(defun ac-cider-compliment-get-context ()
  (let ((context (ac-cider-compliment-get-context-at-point)))
    (if (string= ac-cider-compliment-last-context context)
        ":same"
      (setq ac-cider-compliment-last-context context)
      context)))

(defun ac-cider-compliment-candidates-everything ()
  "Return all candidates for a symbol at point."
  (setq ac-cider-compliment-documentation-cache nil)
  (ac-cider-compliment-candidates*
   (concat "(compliment.core/completions \"" ac-prefix "\" "
           (let ((context (ac-cider-compliment-get-context)))
             (if context
                 (concat "'" context)
               "nil")) ")")))

(defvar ac-cider-compliment-documentation-cache '())

(defun ac-cider-compliment-documentation (symbol)
  "Return documentation for the given SYMBOL, if available.
Caches fetched documentation for the current completion call."
  (when symbol
    (let ((cached-doc (assoc (substring-no-properties symbol)
                             ac-cider-compliment-documentation-cache)))
      (if cached-doc
          (cdr cached-doc)
        (let* ((doc
                (substring-no-properties
                 (replace-regexp-in-string
                  "\\\\n" "\n"
                  (replace-regexp-in-string
                   "\"$" ""
                   (replace-regexp-in-string
                    "^\"" ""
                    (plist-get (ac-cider-compliment-sync-eval
                                (format "(compliment.core/documentation \"%s\")" symbol))
                               :value))))))
               (doc (if (string= "\"\"" doc)
                        "No documentation available."
                      doc)))
          (add-to-list 'ac-cider-compliment-documentation-cache
                       (cons (substring-no-properties symbol) doc))
          doc)))))

(defun ac-cider-compliment-symbol-start-pos ()
  "Find the starting position of the symbol at point, unless inside a string."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
      (car (bounds-of-thing-at-point 'symbol)))))

(defun ac-cider-compliment-match-everything (prefix candidates)
  candidates)

;;;###autoload
(defface ac-cider-compliment-candidate-face
  '((t (:inherit ac-candidate-face)))
  "Face for nrepl candidates."
  :group 'auto-complete)

;;;###autoload
(defface ac-cider-compliment-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the nrepl selected candidate."
  :group 'auto-complete)

;;;###autoload
(defconst ac-cider-compliment-source-defaults
  '((available . ac-cider-compliment-available-p)
    (candidate-face . ac-cider-compliment-candidate-face)
    (selection-face . ac-cider-compliment-selection-face)
    (prefix . ac-cider-compliment-symbol-start-pos)
    (match . ac-cider-compliment-match-everything)
    (document . ac-cider-compliment-documentation)
    (cache))
  "Defaults common to the various completion sources.")

;;;###autoload
(defvar ac-source-compliment-everything
  (append
   '((candidates . ac-cider-compliment-candidates-everything)
     (symbol . "v"))
   ac-cider-compliment-source-defaults)
  "Auto-complete source for nrepl var completion.")

;;;###autoload
(defun ac-cider-compliment-setup ()
  "Add the nrepl completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-compliment-everything))

;;;###autoload
(defun ac-cider-compliment-popup-doc ()
  "A popup alternative to `nrepl-doc'."
  (interactive)
  (popup-tip (ac-cider-compliment-documentation (symbol-name (symbol-at-point)))
             :point (ac-cider-compliment-symbol-start-pos)
             :around t
             :scroll-bar t
             :margin t))

(provide 'ac-cider-compliment)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; ac-cider-compliment.el ends here
