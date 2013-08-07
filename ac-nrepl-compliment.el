;;; ac-nrepl-compliment.el --- auto-complete sources for Clojure using
;;; nrepl and compliment

;; Copyright (C) 2012-2013 Alex Yakushev <alex@bytopia.org>

;; Author: Alex Yakushev <alex@bytopia.org>
;;         Steve Purcell <steve@sanityinc.com>
;;         Sam Aaron <samaaron@gmail.com>
;;
;; URL: https://github.com/alexander-yakushev/ac-nrepl-compliment
;; Keywords: languages, clojure, nrepl, compliment
;; Version: DEV
;; Package-Requires: ((nrepl "0.1") (auto-complete "1.4"))

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
;; using nrepl and compliment. This is a fork of ac-nrepl project by
;; Steve Purcell, with the difference that ac-nrepl-compliment which
;; uses compliment as completion provider.

;;; Installation:

;; Currently the only way of installation is to put
;; ac-nrepl-compliment.el somewhere into ~/.emacs.d/ directory and
;; load it from there. Installation using package manager is coming
;; soon.

;;; Usage:

;;     (load "path/to/ac-nrepl-compliment.el")
;;     (require 'ac-nrepl-compliment)
;;     (add-hook 'nrepl-mode-hook 'ac-nrepl-compliment-setup)
;;     (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-compliment-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'nrepl-mode))

;; If you want to trigger auto-complete using TAB in nrepl buffers, you may
;; want to use auto-complete in your `completion-at-point-functions':

;;     (defun set-auto-complete-as-completion-at-point-function ()
;;       (setq completion-at-point-functions '(auto-complete)))
;;     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;;     (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;     (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;; You might consider using ac-nrepl-compliment's popup documentation in place of `nrepl-doc':
;;
;;     (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-compliment-popup-doc)

;;; Code:

(require 'nrepl)
(require 'auto-complete)

(defvar ac-nrepl-compliment-complete-core-required nil)

(defun ac-nrepl-compliment-require-complete-core ()
  (setq ac-nrepl-compliment-complete-core-required
        (if (string= (plist-get
                      (ac-nrepl-compliment-sync-eval
                       "(try (require 'compliment.core) (catch Exception e :error))")
                      :value)
                     "nil")
            t :na)))

(defun ac-nrepl-compliment-available-p ()
  "Return t if nrepl is available for completion, otherwise nil."
  (if (eq ac-nrepl-compliment-complete-core-required t)
      t
    (cond ((eq ac-nrepl-compliment-complete-core-required :na) nil)
          (t (when (condition-case nil (nrepl-current-tooling-session)
                     (error nil))
               (ac-nrepl-compliment-require-complete-core))))))

(defun ac-nrepl-compliment-sync-eval (clj)
  "Synchronously evaluate CLJ.
Result is a plist, as returned from `nrepl-send-string-sync'."
  (nrepl-send-string-sync clj (nrepl-current-ns) (nrepl-current-tooling-session)))

(defun ac-nrepl-compliment-candidates* (clj)
  "Return completion candidates produced by evaluating CLJ."
  (let* ((response (plist-get (ac-nrepl-compliment-sync-eval clj) :value)))
    (when response
      (car (read-from-string response)))))

(defvar ac-nrepl-compliment-last-context nil)

(defun ac-nrepl-compliment-get-context-at-point ()
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
             (pref-start (ac-nrepl-compliment-symbol-start-pos))
             (context (nrepl-expression-at-point))
             (_ (beginning-of-defun))
             (expr-start (point)))
        (concat (substring context 0 (- pref-start expr-start))
                "__prefix__"
                (substring context (- pref-end expr-start)))))))

(defun ac-nrepl-compliment-get-context ()
  (let ((context (ac-nrepl-compliment-get-context-at-point)))
    (if (string= ac-nrepl-compliment-last-context context)
        ":same"
      (setq ac-nrepl-compliment-last-context context)
      context)))

(defun ac-nrepl-compliment-candidates-everything ()
  "Return all candidates for a symbol at point."
  (setq ac-nrepl-compliment-documentation-cache nil)
  (ac-nrepl-compliment-candidates*
   (concat "(compliment.core/completions \"" ac-prefix "\" "
           (let ((context (ac-nrepl-compliment-get-context)))
             (if context
                 (concat "'" context)
               "nil")) ")")))

(defvar ac-nrepl-compliment-documentation-cache '())

(defun ac-nrepl-compliment-documentation (symbol)
  "Return documentation for the given SYMBOL, if available.
Caches fetched documentation for the current completion call."
  (when symbol
    (let ((cached-doc (assoc (substring-no-properties symbol)
                             ac-nrepl-compliment-documentation-cache)))
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
                    (plist-get (ac-nrepl-compliment-sync-eval
                                (format "(compliment.core/documentation \"%s\")" symbol))
                               :value))))))
               (doc (if (string= "\"\"" doc)
                        "No documentation available."
                      doc)))
          (add-to-list 'ac-nrepl-compliment-documentation-cache
                       (cons (substring-no-properties symbol) doc))
          doc)))))

(defun ac-nrepl-compliment-symbol-start-pos ()
  "Find the starting position of the symbol at point, unless inside a string."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
      (car (bounds-of-thing-at-point 'symbol)))))

(defun ac-nrepl-compliment-match-everything (prefix candidates)
  candidates)

;;;###autoload
(defface ac-nrepl-compliment-candidate-face
  '((t (:inherit ac-candidate-face)))
  "Face for nrepl candidates."
  :group 'auto-complete)

;;;###autoload
(defface ac-nrepl-compliment-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the nrepl selected candidate."
  :group 'auto-complete)

;;;###autoload
(defconst ac-nrepl-compliment-source-defaults
  '((available . ac-nrepl-compliment-available-p)
    (candidate-face . ac-nrepl-compliment-candidate-face)
    (selection-face . ac-nrepl-compliment-selection-face)
    (prefix . ac-nrepl-compliment-symbol-start-pos)
    (match . ac-nrepl-compliment-match-everything)
    (document . ac-nrepl-compliment-documentation)
    (cache))
  "Defaults common to the various completion sources.")

;;;###autoload
(defvar ac-source-nrepl-everything
  (append
   '((candidates . ac-nrepl-compliment-candidates-everything)
     (symbol . "v"))
   ac-nrepl-compliment-source-defaults)
  "Auto-complete source for nrepl var completion.")

;;;###autoload
(defun ac-nrepl-compliment-setup ()
  "Add the nrepl completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-nrepl-everything))

;;;###autoload
(defun ac-nrepl-compliment-popup-doc ()
  "A popup alternative to `nrepl-doc'."
  (interactive)
  (popup-tip (ac-nrepl-compliment-documentation (symbol-name (symbol-at-point)))
             :point (ac-nrepl-compliment-symbol-start-pos)
             :around t
             :scroll-bar t
             :margin t))

(provide 'ac-nrepl-compliment)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; ac-nrepl-compliment.el ends here
