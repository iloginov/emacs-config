;;
;; Это один из init-файлов, которые смотрит Emacs: [[http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html]]
;;

(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

(let ((my-packages (list
                    'ac-nrepl
                    'auto-complete
                    'cider
                    'clj-refactor
                    'clojure-mode
                    'clojure-mode-extra-font-locking
                    'clojure-snippets
                    'clojurescript-mode
                    'company
                    'company-cider
                    'csharp-mode
                    'elein
                    'ergoemacs-mode
                    'espresso-theme
                    'flymake
                    'graphviz-dot-mode
                    'monokai-theme
                    'magit
                    'omnisharp
                    'org
                    'paredit
                    'paredit-menu
                    'popup
                    'rainbow-delimiters
                    'rainbow-mode
                    'smex
                    'yasnippet)))

        (unless package-archive-contents
          (package-refresh-contents))
        (dolist (package my-packages)
          (unless (package-installed-p package)
            (package-install package))))


;; Инициализация ErgoEmacs.
(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)

;; Настраиваем внешний вид.
(menu-bar-mode -1) ; Выключаем меню.
(tool-bar-mode -1) ; Выключаем тулбар.
(scroll-bar-mode -1) ; Выключаем скроллбар.
(setq-default indicate-empty-lines t) ; Отображение чёрточек у пустых линий.
(load-theme 'espresso t)
(add-to-list 'default-frame-alist '(font . "PT\ Mono-13"))
(set-default-font "PT\ Mono-13")

;; Отключаем всевозможные бекапы.
(setq make-backup-files        nil) ; Don't want any backup files
(setq auto-save-list-file-name nil) ; Don't want any .saves files
(setq auto-save-default        nil) ; Don't want any autosaving

(add-to-list 'load-path "~/.emacs.d/")

;;
;; Отображение номеров строк.
;;
;; Если сделать просто (setq linum-format "%d"), как пишут во
;; многих местах в интернете, то происходит некорректная работа
;; с цветовыми схемами.
(require 'linum+)
(setq linum-format 'dynamic)
(setq linum+-dynamic-format "%%%dd")
(global-linum-mode 1)

;; Инициализация ido.
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Инициализация Smex (M-x + IDO).
(require 'smex)
(smex-initialize)

;; Автодополнение
(require 'company)
;(add-hook 'after-init-hook 'global-company-mode)

;; Подсветка лисповых выражений.
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (setq show-paren-style 'expression)
	     (show-paren-mode 2)
	     (company-mode t)))

;; TODO: Yasnippet

;;
;; Настройка фолдинга с помощью hideshow.
;;
(defvar hs-special-modes-alist
  (mapcar 'purecopy
	  '((c-mode "{" "}" "/[*/]" nil nil)
	    (c++-mode "{" "}" "/[*/]" nil nil)
	    (csharp-mode "{" "}" "/[*/]" nil nil)
	    (java-mode "{" "}" "/[*/]" nil nil)
	    (emacs-lisp-mode "(" ")" nil))))
(autoload 'hs-minor-mode "hideshow" "HideShow Minor Mode" t)

;;
;; Настройка C#-mode.
;;
(add-hook 'csharp-mode-hook
	  '(lambda ()
	     (hs-minor-mode t)
	     (ergoemacs-local-set-key (kbd "<f9>") 'hs-toggle-hiding)
	     (ergoemacs-local-set-key (kbd "C-<f9>") 'hs-hide-all)
	     (ergoemacs-local-set-key (kbd "C-S-<f9>") 'hs-show-all)))

;;
;; Настройка org-mode.
;;
;; Проверяем, что org-mode > версии 8.0.0. Он не обновляется вместе
;; с остальными пакетами при установке, потому что входит в комплект.
(when (< (string-to-int (first (split-string (org-version) "[.]"))) 8)
  (package-install 'org)
  (org-reload))

;; List of additional LaTeX packages
(add-to-list 'org-latex-packages-alist '("" "cmap" t))
(add-to-list 'org-latex-packages-alist '("english,russian" "babel" t))
;; Sources in org-mode
(org-babel-do-load-languages 'org-babel-load-languages
			     '((dot . t)
			       (ditaa . t)))
;; Set ditaa path
(setq org-ditaa-jar-path (expand-file-name "/usr/share/ditaa/ditaa.jar"))
;; Подсветка кода
(setq org-src-fontify-natively t)
;; Выключаем конвертацию _ в индекс.
(setq org-with-sub-superscripts nil)
(add-hook 'org-mode-hook
	  '(lambda ()
	     (visual-line-mode t))) ; не хочу, чтобы текст убегал за края
;; Agenda files path
(setq org-agenda-files '("~/Dropbox/YaTrade/SocialService"))

;;
;; Настройки для Clojure.
;;
(require 'clojure-mode)
(require 'yasnippet)
;; Добавляем Yasnippet к Clojure-mode.
(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (yas/minor-mode-on)))
;; Настраиваем intendation: https://github.com/weavejester/compojure/wiki/Emacs-indentation
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))
;; Включаем rainbow braces.
(add-hook 'prog-mode-hook  'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;; Настраиваем paredit.
(require 'paredit)
(defun turn-on-paredit () (paredit-mode t))
(add-hook 'emacs-lisp-mode-hook       'turn-on-paredit)
(add-hook 'lisp-mode-hook             'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook           'turn-on-paredit)
(add-hook 'clojure-mode-hook          'turn-on-paredit)
(add-hook 'cider-repl-mode-hook       'turn-on-paredit)
(add-hook 'sibiliant-mode-hook        'turn-on-paredit)
;; Elastic Returns.
(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\" return.")
(defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
          (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))
(add-hook 'paredit-mode-hook
        (lambda ()
          (local-set-key (kbd "RET") 'electrify-return-if-match)))
;; Auto Completion
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)
;; CIDER-specific configuration for auto-completion.
(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'cider-mode)
(add-to-list 'ac-modes 'cider-repl-mode)
;; Hot-key for doc.
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
;; Enable ElDoc.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; CIDER config.
(setq cider-repl-use-clojure-font-lock t)
(setq nrepl-hide-special-buffers t)
(setq cider-popup-stacktraces nil)
;; Enable CIDER in org-mode also.
(require 'ob-clojure)

(setq org-babel-clojure-backend 'cider)
(require 'cider)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
