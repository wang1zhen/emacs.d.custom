(require 'cl-lib)

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)            ; Disable the menu bar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; 左右边框

;; Column number in the modeline
(column-number-mode t)

;; Line numbers
(global-display-line-numbers-mode t)
;; Disable for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Visual bell
(setq visible-bell t)

;; Fonts
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (display-graphic-p)
  (cond
   ((equal (system-name) "Pro14-wang1zhen") (setq font-size-factor 3))
   ((equal (system-name) "Arch-X230") (setq font-size-factor 3))
   ((equal (system-name) "PC-SH") (setq font-size-factor 3))
   (t (setq font-size-factor 2)))
  
  ;; Set default font
  (cl-loop for font in '("CaskaydiaCove Nerd Font" "Cascadia Code"
			 "FiraCode Nerd Font" "Fira Code"
			 "Hack" "Source Code Pro" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (* font-size-factor 60)))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("Sarasa Mono SC" "WenQuanYi Micro Hei Mono" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))
;; 字体检查 言 ♪

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use the system clipboard
(setq x-select-enable-clipboard t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("elpa" . "https://elpa.emacs-china.org/gnu/")
			 ("melpa" . "https://elpa.emacs-china.org/melpa/")
                         ("org" . "https://elpa.emacs-china.org/org/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package command-log-mode
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :diminish doom-modeline-mode
  :init (doom-modeline-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (ivy-mode 1)
  :custom
  (ivy-use-selectable-prompt 1)
  (ivy-initial-inputs-alist nil))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init (counsel-mode 1))

(use-package amx
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :diminish counsel-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.1))

(use-package ivy-rich
  :ensure t
  :diminish ivy-rich-mode
  :init (ivy-rich-mode 1))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package undo-fu
  :ensure t
  :bind
  ("C-/" . undo-fu-only-undo)
  ("C-r" . undo-fu-only-redo)
  ("C-x r" . undo-fu-only-redo))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-org-config))

(use-package general
  :ensure t
  :config
  (general-create-definer w1/leader-key1
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-c")

  (w1/leader-key1
    "o" '(:ignore t :which-key "custom entry")
    "ot" '(counsel-load-theme :which-key "choose theme")
    "oy" '(youdao-dictionary-search-at-point :which-key "Youdao Dict"))

  (general-define-key :keymaps 'evil-insert-state-map
		      (general-chord "jk") 'evil-normal-state))

(use-package evil
  :ensure t
  :init
  (setq
   evil-want-integration t
   evil-want-keybinding nil
   evil-want-C-u-scroll t
   evil-want-C-i-jump nil
   evil-disable-insert-state-bindings t)
;;  :hook
  :config
  (evil-mode 1)
  (evil-global-set-key 'insert (kbd "C-g") 'evil-normal-state)

  ;; Visual line motions
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Emacs flavour moving in normal and visual mode
  (evil-global-set-key 'normal (kbd "C-f") 'forward-char)
  (evil-global-set-key 'normal (kbd "C-b") 'backward-char)
  (evil-global-set-key 'normal (kbd "C-n") 'next-line)
  (evil-global-set-key 'normal (kbd "C-p") 'previous-line)
  (evil-global-set-key 'normal (kbd "C-a") 'move-beginning-of-line)
  (evil-global-set-key 'normal (kbd "C-e") 'move-end-of-line)

  (evil-global-set-key 'visual (kbd "C-f") 'forward-char)
  (evil-global-set-key 'visual (kbd "C-b") 'backward-char)
  (evil-global-set-key 'visual (kbd "C-n") 'next-line)
  (evil-global-set-key 'visual (kbd "C-p") 'previous-line)
  (evil-global-set-key 'visual (kbd "C-a") 'move-beginning-of-line)
  (evil-global-set-key 'visual (kbd "C-e") 'move-end-of-line)
  )

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
  (w1/leader-key1
   "os" '(hydra-text-scale/body :which-key "scale text")))

(use-package avy
  :ensure t)

(use-package youdao-dictionary
  :ensure t
  :config
  (general-define-key
   :states 'normal
   :keymaps 'youdao-dictionary-mode-map
   "q" 'quit-window))

(use-package key-chord
  :ensure t
  :diminish key-chord-mode
  :init (key-chord-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(youdao-dictionary key-chord keychord hydra avy evil-collection evil general amx doom-themes undo-fu helpful ivy-rich which-key rainbow-delimiters ranbow-delimiters ranbow-delimeters counsel swiper ivy command-log-mode use-package doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
