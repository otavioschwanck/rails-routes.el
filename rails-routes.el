;;; rails-routes.el ---  Search and insert rails routes through emacs                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Otávio Schwanck

;; Author: Otávio Schwanck <otavioschwanck@gmail.com>
;; Keywords: lisp ruby rails routes
;; Version: 0.1

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

;; A package to help you to find and insert routes into your code.  It fetches all routes and cache it.
;; You will not need to use the terminal to search and insert your routes.  Just call it and select
;; the route will want.

;;; Code:

(defvar rails-routes-search-command "RUBYOPT=-W0 rails routes" "Command executed to search the routes.")
(defvar rails-routes-insert-after-path "_path" "What will be inserted after call rails-routes-find.")
(defvar rails-routes-use-cache t "If t, will enable caching.  You need to have projectile to use it.")
(defvar rails-routes-class-name "Rails.application.routes.url_helpers."
  "Name the prefix to use on rails routes outside the views.")

(defvar rails-routes-cache '())
(defvar rails-routes-cache-validations '())
(defvar savehist-additional-variables '())

(defun rails-routes--set-cache (VAL)
  "Set de cache values. VAL:  Value to set."
  (when (assoc (projectile-project-name) rails-routes-cache)
    (setq rails-routes-cache (remove (assoc (projectile-project-name) rails-routes-cache) rails-routes-cache)))
  (setq rails-routes-cache (cons `(,(projectile-project-name) . ,VAL) rails-routes-cache))
  (add-to-list 'savehist-additional-variables 'rails-routes-cache))

(defun rails-routes--set-cache-validations (VAL)
  "Set de cache validations. VAL:  Value to set."
  (when (assoc (projectile-project-name) rails-routes-cache-validations)
    (setq rails-routes-cache-validations
          (remove (assoc (projectile-project-name) rails-routes-cache-validations) rails-routes-cache-validations)))
  (setq rails-routes-cache-validations (cons `(,(projectile-project-name) . ,VAL) rails-routes-cache-validations))
  (add-to-list 'savehist-additional-variables 'rails-routes-cache-validations))

(defun rails-routes-clear-cache ()
  "Clear rails routes cache."
  (interactive)
  (setq rails-routes-cache '())
  (setq rails-routes-cache-validations '()))

(defun rails-routes--run-command ()
  "Run rails-routes-search-command and return it."
  (message "Fetching routes.  Please wait.")
  (let ((COMMAND-RESULT (cl-remove-if-not
                         (lambda (element)
                           (or (eq (length (split-string element " +")) 5)
                               (eq (length (split-string element " +")) 4)))
                         (split-string (shell-command-to-string rails-routes-search-command) "\n"))))

    (if rails-routes-use-cache
        (progn
          (rails-routes--set-cache COMMAND-RESULT)
          (rails-routes--set-cache-validations t)))

    COMMAND-RESULT))

(defun rails-routes--get-routes-cached ()
  "Get the cached routes if not exists."
  (let ((ROUTES-RESULT (if (cdr (assoc (projectile-project-name) rails-routes-cache-validations))
                           (cdr (assoc (projectile-project-name) rails-routes-cache))
                         (rails-routes--run-command))))
    (if (eq ROUTES-RESULT nil) (rails-routes--run-command) ROUTES-RESULT)))

(defun rails-routes--get-routes ()
  "Handle if need to call cache or run directly the command."
  (if rails-routes-use-cache (rails-routes--get-routes-cached) (rails-routes--run-command)))

(defun rails-routes--guess-route (CONTROLLER-FULL-PATH)
  "Try to get the route name when rails routes don't show to us. CONTROLLER-FULL-PATH:  controller name plus action."
  (let ((CONTROLLER-PATH (nth 0 (split-string CONTROLLER-FULL-PATH "#"))))
    (replace-regexp-in-string "\/" "_" CONTROLLER-PATH)))

(defun rails-routes--find (INSERT-CLASS)
  "Ask for the route you want and insert on code.  INSERT-CLASS: if t, insert the prefix class."
  (let* ((selected-value (split-string (completing-read "Route: " (rails-routes--get-routes)) " +"))
         (selected-route (nth (if (eq (length selected-value) 5) 3 2) selected-value)))
    (when INSERT-CLASS (insert rails-routes-class-name))
    (rails-routes--insert-value selected-value)
    (when (or (string-match-p ":id" selected-route)
              (string-match-p ":[a-zA-Z0-9]+_id" selected-route))
      (progn (insert "()") (backward-char)))))

(defun rails-routes--insert-value (SELECTED-VALUE)
  "Insert the selected_value.  SELECTED-VALUE: Item im list."
  (insert (if (eq (length SELECTED-VALUE) 5) (nth 1 SELECTED-VALUE)
            (rails-routes--guess-route (nth 3 SELECTED-VALUE))) rails-routes-insert-after-path))

;;;###autoload
(defun rails-routes-find ()
  "Find rails routes on current project."
  (interactive)
  (rails-routes--find nil))

;;;###autoload
(defun rails-routes-find-with-class ()
  "Find rails routes on current project.  Also insert a prefix class.  This can be used outside views."
  (interactive)
  (rails-routes--find t))

;;;###autoload
(defun rails-routes-invalidate-cache ()
  "Invalidate cache when the file that will be saved is routes.rb."
  (when (string-match-p "routes.rb" (buffer-file-name)) (rails-routes--set-cache-validations nil)))

(defun rails-routes--add-alist ()
  "Add the rails-routes-cache and rails-routes-cache-validations to alist."
  (if rails-routes-use-cache
      (progn
        (message "routes.rb has changed.  Yhe rails routes cache will be invalidated...")
        (add-to-list 'savehist-additional-variables 'rails-routes-cache)
        (add-to-list 'savehist-additional-variables 'rails-routes-cache-validations))))

(defun rails-routes--set-routes-hook ()
  "Set the hook for 'after-save-hook' only for routes.rb."
  (make-local-variable 'after-save-hook)
  (if (string-match-p "routes.rb" (buffer-file-name))
    (progn
      (add-hook 'after-save-hook 'rails-routes-invalidate-cache))))

(add-hook 'ruby-mode-hook #'rails-routes--set-routes-hook)

(with-eval-after-load 'savehist
  '(add-hook 'savehist-mode-hook #'rails-routes--add-alist))

(provide 'rails-routes)
;;; rails-routes.el ends here
