;;; emacs-rails-routes.el -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2020 Otávio Schwanck <otavioschwanck@gmail.com>

;; Author: Otávio Schwanck <otavioschwanck@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.0"))
;; Keywords: ruby rails routes
;;
;;; Commentary
;;
;; A package to help you to find and insert routes into your code

(defvar rails-routes-default-search 'ivy "Default search to search for the routes.  Available values: 'helm 'ivy")
(defvar rails-routes-search-command "RUBYOPT=-W0 rails routes" "Command executed to search the routes")
(defvar rails-routes-insert-after-path "_path" "What will be inserted after call rails-routes-find")
(defvar rails-routes-use-cache t "If t, will enable caching.  You need to have projectile to use it.")
(defvar rails-routes-class-name "Rails.application.routes.url_helpers.admin_user_url." "Name the prefix to use on rails routes outside the views")

(defvar rails-routes-cache '())
(defvar rails-routes-cache-validations '())

(defun rails-routes--set-cache (VAL)
  (when (assoc (projectile-project-name) rails-routes-cache)
    (setq rails-routes-cache (remove (assoc (projectile-project-name) rails-routes-cache) rails-routes-cache)))
  (setq rails-routes-cache (cons `(,(projectile-project-name) . ,VAL) rails-routes-cache))
  (add-to-list 'savehist-additional-variables 'rails-routes-cache))

(defun rails-routes--set-cache-validations (VAL)
  (when (assoc (projectile-project-name) rails-routes-cache-validations)
    (setq rails-routes-cache-validations (remove (assoc (projectile-project-name) rails-routes-cache-validations) rails-routes-cache-validations)))
  (setq rails-routes-cache-validations (cons `(,(projectile-project-name) . ,VAL) rails-routes-cache-validations))
  (add-to-list 'savehist-additional-variables 'rails-routes-cache-validations))

(defun rails-routes-clear-cache ()
  (interactive)
  (setq rails-routes-cache '())
  (setq rails-routes-cache-validations '()))

(defun rails-routes--run-command ()
  (message "Fetching routes.  Please wait.")
  ;; Get only elements that has a path to set
  (setq RESULT (cl-remove-if-not
                (lambda (element)
                  (eq (length (split-string element " +")) 5))
                (split-string (shell-command-to-string rails-routes-search-command) "\n")))

  ;; Save on cache
  (if rails-routes-use-cache
      (progn
        (rails-routes--set-cache RESULT)
        (rails-routes--set-cache-validations t)))

  RESULT)

(defun rails-routes--get-routes-cached ()
  (setq RESULT (if (cdr (assoc (projectile-project-name) rails-routes-cache-validations)) (cdr (assoc (projectile-project-name) rails-routes-cache)) (rails-routes--run-command)))
  (if (eq RESULT nil) (rails-routes--run-command) RESULT))

(defun rails-routes--get-routes ()
  (if rails-routes-use-cache (rails-routes--get-routes-cached) (rails-routes--run-command)))

(defun rails-routes--find-with-ivy (INSERT-CLASS)
  (setq selected-value (split-string (ivy-read "Route: " (rails-routes--get-routes)) " +"))
  (when INSERT-CLASS (insert rails-routes-class-name))
  (insert (nth 1 selected-value) rails-routes-insert-after-path)
  (setq ROUTE (nth 3 selected-value))
  (when (or (string-match-p ": +id" ROUTE) (string-match-p ":[a-zA-Z0-9]+_id" ROUTE)) (progn (insert "()") (backward-char))))

(defun rails-routes--find-with-helm (INSERT-CLASS)
  (setq selected-value (split-string (helm-comp-read "Route: " (rails-routes--get-routes)) " +"))
  (when INSERT-CLASS (insert rails-routes-class-name))
  (insert (nth 1 selected-value) rails-routes-insert-after-path)
  (setq ROUTE (nth 3 selected-value))
  (when (or (string-match-p ":id" ROUTE) (string-match-p ":[a-zA-Z0-9]+_id" ROUTE)) (progn (insert "()") (backward-char))))

(defun rails-routes-find ()
  (interactive)
  (if (eq rails-routes-default-search 'ivy) (rails-routes--find-with-ivy nil))
  (if (eq rails-routes-default-search 'helm) (rails-routes--find-with-helm nil)))

(defun rails-routes-find-with-class ()
  (interactive)
  (if (eq rails-routes-default-search 'ivy) (rails-routes--find-with-ivy t))
  (if (eq rails-routes-default-search 'helm) (rails-routes--find-with-helm t)))

(defun rails-routes-invalidate-cache ()
  (if (string-match-p "routes.rb" (buffer-file-name)) (rails-routes--set-cache-validations nil)))

(add-hook 'after-save-hook 'rails-routes-invalidate-cache)

(provide 'emacs-rails-routes)
