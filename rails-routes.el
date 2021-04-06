;;; rails-routes.el --- Search for and insert rails routes    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Otávio Schwanck

;; Author: Otávio Schwanck <otavioschwanck@gmail.com>
;; Keywords: tools languages
;; Homepage: https://github.com/otavioschwanck/rails-routes
;; Version: 0.2
;; Package-Requires: ((emacs "26.0") (inflections "2.5") (projectile "2.3.0"))

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

;; This is a package to help you find and insert rails routes into your code.
;; Instead of going to the terminal or loading the path in the browser, you just call 'rails-routes-insert',
;; It will fetch and save on cache all routes used by your application, so you have a reliable and easy way to search
;; and insert your routes.
;;
;; New on 0.2
;; Add rails-routes-jump to jump to the route controller.  Works with activeadmin.

;;; Code:
(require 'savehist)
(require 'projectile)
(require 'subr-x)
(require 'inflections)

(defgroup rails-routes nil
  "Search for and insert rails routes."
  :group 'tools
  :group 'languages)

(defcustom rails-routes-search-command "RUBYOPT=-W0 rails routes"
  "Command executed to search the routes."
  :type 'string)

(defcustom rails-routes-insert-after-path "_path"
  "What will be inserted after calling `rails-routes-insert'."
  :type 'string)

(defcustom rails-routes-use-cache t
  "If non-nil, will enable caching."
  :type 'bool)

(defcustom rails-routes-class-name "Rails.application.routes.url_helpers."
  "Prefix used to access rails routes outside the views."
  :type 'string)

(defvar rails-routes-cache '())
(defvar rails-routes-cache-validations '())

(defun rails-routes--set-cache (val)
  "Set routes cache to VAL."
  (when (assoc (projectile-project-name) rails-routes-cache)
    (setq rails-routes-cache (remove (assoc (projectile-project-name) rails-routes-cache) rails-routes-cache)))
  (setq rails-routes-cache (cons `(,(projectile-project-name) . ,val) rails-routes-cache)))

(defun rails-routes--set-cache-validations (val)
  "Set validations cache to VAL."
  (when (assoc (projectile-project-name) rails-routes-cache-validations)
    (setq rails-routes-cache-validations
          (remove (assoc (projectile-project-name) rails-routes-cache-validations) rails-routes-cache-validations)))
  (setq rails-routes-cache-validations (cons `(,(projectile-project-name) . ,val) rails-routes-cache-validations)))

(defun rails-routes-clear-cache ()
  "Clear rails routes cache."
  (interactive)
  (setq rails-routes-cache '())
  (setq rails-routes-cache-validations '()))

(defun rails-routes--run-command ()
  "Run rails-routes-search-command and return it."
  (message "Fetching routes.  Please wait.")
  (let ((command-result (cl-remove-if-not
                         (lambda (element)
                           (let ((len (length (split-string element " +"))))
                             (or (eq len 5) (eq len 4))))
                         (split-string (shell-command-to-string rails-routes-search-command) "\n"))))

    (when rails-routes-use-cache
      (rails-routes--set-cache command-result)
      (rails-routes--set-cache-validations t))

    command-result))

(defun rails-routes--get-routes-cached ()
  "Get the routes, using the cache if possible."
  (let ((routes-result (if (cdr (assoc (projectile-project-name) rails-routes-cache-validations))
                           (cdr (assoc (projectile-project-name) rails-routes-cache))
                         (rails-routes--run-command))))
    (if (eq routes-result nil)
        (rails-routes--run-command)
      routes-result)))

(defun rails-routes--get-routes ()
  "Get the routes, trying the cache first if `rails-routes-use-cache' is enabled."
  (if rails-routes-use-cache
      (rails-routes--get-routes-cached)
    (rails-routes--run-command)))

(defun rails-routes--guess-route (controller-full-path)
  "Guess the route name from CONTROLLER-FULL-PATH.
CONTROLLER-FULL-PATH is the controller name plus action."
  (let ((controller-path (nth 0 (split-string controller-full-path "#"))))
    (replace-regexp-in-string "\/" "_" controller-path)))

(defun rails-routes--insert (insert-class)
  "Ask for the route you want and insert on code.
With prefix argument INSERT-CLASS, fully-qualify the route with
the `rails-routes-class-name' prefix."
  (let* ((selected-value (split-string (completing-read "Route: " (rails-routes--get-routes)) " +"))
         (selected-route (nth (if (eq (length selected-value) 5) 3 2) selected-value)))
    (when insert-class (insert rails-routes-class-name))
    (rails-routes--insert-value selected-value)
    (when (or (string-match-p ":id" selected-route)
              (string-match-p ":[a-zA-Z0-9]+_id" selected-route))
      (progn (insert "()") (backward-char)))))

(defun rails-routes--insert-value (selected-value)
  "Insert the selected_value.  SELECTED-VALUE: Item im list."
  (insert (if (eq (length selected-value) 5) (nth 1 selected-value)
            (rails-routes--guess-route (nth 3 selected-value)))
          rails-routes-insert-after-path))

;;;###autoload
(defun rails-routes-insert ()
  "Find rails routes on current project."
  (interactive)
  (rails-routes--insert nil))

;;;###autoload
(defun rails-routes-insert-with-class ()
  "Find rails routes on current project.  Also insert a prefix class.  This can be used outside views."
  (interactive)
  (rails-routes--insert t))

;;;###autoload
(defun rails-routes-invalidate-cache ()
  "Invalidate cache when the file that will be saved is routes.rb."
  (when (string-match-p "routes.rb" (buffer-file-name))
    (rails-routes--set-cache-validations nil)))

(defun rails-routes--add-alist ()
  "Add the rails-routes-cache and rails-routes-cache-validations to alist."
  (when rails-routes-use-cache
    (add-to-list 'savehist-additional-variables 'rails-routes-cache)
    (add-to-list 'savehist-additional-variables 'rails-routes-cache-validations)))

(defun rails-routes--remove-path-or-url (path)
  "Remove any \"_path\" or \"_url\" suffix from PATH."
  (replace-regexp-in-string "\\(_path\\|_url\\)\\'" "" path))

(defun rails-routes--find-controller (path)
  "Find controller for path in routes list.
PATH: a rails routes path or url."
  (let ((routes (rails-routes--get-routes))
        (response nil))
    (dolist (item routes)
      (let ((parsed_item (split-string item " +")))
        (when (string-equal (nth 1 parsed_item) path)
          (setq response (nth 4 parsed_item)))))
    response))

(defun rails-routes--singularize-string (word)
  "Singularize all words in a route.  WORD: any_word."
  (setq word (substring word 5))
  (let ((words (split-string word "_")))
    (string-join (mapcar #'inflection-singularize-string words) "_")))

(defun rails-routes--goto-activeadmin-controller (controller_name action)
  "Try to go to activeadmin first, if not exists, go to app/controllers.
CONTROLLER_NAME: Path of controller.  ACTION:  Action of the path."
  (let* ((project-root (projectile-project-root))
         (moved nil)
         (normal-path (expand-file-name (concat "app/admin" (rails-routes--singularize-string controller_name) ".rb") project-root))
         (expanded-path
          (expand-file-name (concat "app/admin"
                                    (replace-regexp-in-string "_" "/" (rails-routes--singularize-string controller_name)) ".rb")
                            project-root)))

    (when (file-exists-p normal-path)
      (find-file normal-path)
      (setq moved t))

    (when (and (not moved) (file-exists-p expanded-path))
      (find-file expanded-path)
      (setq moved t))

    (if moved
        (progn
          (goto-char (point-min))
          (search-forward (concat "member_action :" action) (point-max) t)
          (search-forward (concat "collection_action :" action) (point-max) t))
      (rails-routes--go-to-controller controller_name action))))

(defun rails-routes--go-to-controller-and-action (full_action)
  "Go to controller and then, go to def action_name.  FULL_ACTION: action showed on rails routes."
  (let ((controller_name (nth 0 (split-string full_action "#"))) (action (nth 1 (split-string full_action "#"))))
    (if (string-match-p "admin" controller_name)
        (rails-routes--goto-activeadmin-controller controller_name action)
      (rails-routes--go-to-controller controller_name action))))

(defun rails-routes--go-to-controller (controller action)
  "Go to controller using action.  CONTROLLER: controller showed on rails routes. ACTION: action showed on rails routes."
  (find-file (rails-routes--controller-full-path controller))
  (search-forward (concat "def " action) (point-max) t))

(defun rails-routes--controller-full-path (controller_name)
  "Return the path of a rails controller using only the name.  CONTROLLER_NAME: Name of the controller."
  (concat (projectile-project-root) "app/controllers/" controller_name "_controller.rb"))

(defun rails-routes--find-in-controllers (path)
  "Find route in controllers when route is not found.  PATH: a rails path."
  (let* ((project-root (projectile-acquire-root))
         (result (projectile-completing-read "Find your controller: "
                                             (projectile-project-files project-root)
                                             :initial-input
                                             (concat (rails-routes--remove-path-or-url path) " controller"))))

    (find-file (expand-file-name result project-root))))

;;;###autoload
(defun rails-routes-jump ()
  "Go to the route at point."
  (interactive)
  (let* ((path (symbol-name (symbol-at-point)))
         (controller (rails-routes--find-controller (rails-routes--remove-path-or-url path))))
    (if controller
        (rails-routes--go-to-controller-and-action controller)
      (rails-routes--find-in-controllers path))
    (recenter)))

(defun rails-routes--set-routes-hook ()
  "Set the hook for 'after-save-hook' only for routes.rb."
  (when (and (buffer-file-name)
             (string-equal "routes.rb" (file-name-nondirectory (buffer-file-name))))
    (add-hook 'after-save-hook 'rails-routes-invalidate-cache nil t)))

(add-hook 'ruby-mode-hook #'rails-routes--set-routes-hook)

(add-hook 'savehist-mode-hook #'rails-routes--add-alist)
(add-to-list 'savehist-additional-variables 'rails-routes-cache)
(add-to-list 'savehist-additional-variables 'rails-routes-cache-validations)

(provide 'rails-routes)
;;; rails-routes.el ends here
