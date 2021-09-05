(require 'cl-lib)

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)            ; Disable the menu bar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; 左右边框

;; Scratch buffer settings
(setq initial-scratch-message ";; Happy Emacsing\n\n")

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

;; Always focus the help window
(setq help-window-select t)

;; Initialize package sources
(require 'package)

;; (setq package-archives '(("elpa" . "https://elpa.emacs-china.org/gnu/")
;; 			 ("melpa" . "https://elpa.emacs-china.org/melpa/")
;;                          ("org" . "https://elpa.emacs-china.org/org/")))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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
  :init
  (setq which-key-idle-delay 0.1)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode))

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
  (load-theme 'doom-gruvbox t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package general
  :ensure t
  :after evil
  :config
  (general-create-definer w1/leader-key1
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix (general-chord ",,"))

  (general-def help-map
  ;; new keybinds
  "'"    #'describe-char

  ;; Unbind `help-for-help'. Conflicts with which-key's help command for the
  ;; <leader> h prefix. It's already on ? and F1 anyway.
  "C-h"  nil

  ;; replacement keybinds
  ;; replaces `info-emacs-manual' b/c it's on C-m now
  "r"    nil

  "b"   #'describe-bindings

  ;; replaces `apropos-command'
  "a"    #'apropos
  "A"    #'apropos-documentation
  ;; replaces `describe-copying' b/c not useful
  "C-c"  #'describe-coding-system
  ;; replaces `Info-got-emacs-command-node' b/c redundant w/ `Info-goto-node'
  "F"    #'describe-face
  ;; replaces `view-hello-file' b/c annoying
  "h"    nil
  ;; replaces `view-emacs-news' b/c it's on C-n too
  "n"    #'doom/help-news
  ;; replaces `help-with-tutorial', b/c it's less useful than `load-theme'
  "t"    #'counsel-load-theme
  ;; replaces `finder-by-keyword' b/c not useful
  "p"    nil
  )

  (general-def evil-window-map
    "c" nil
    "d" #'evil-window-delete)
  
  (w1/leader-key1
    ;; maps
    "w" '(evil-window-map :which-key "Window")
    "h" '(help-command :which-key "Help")
    "p" '(projectile-command-map :which-key "Projectile")

    ;; keys
    "SPC" '(counsel-M-x :which-key "Execute")
    "M-x" 'eval-expression

    "a" '(avy-goto-char-2 :which-key "Avy")
    "g" '(magit-status :which-key "Magit")
    "u" 'universal-argument
    
    ;; buffer
    "b" '(:ignore t :which-key "Buffer")
    "bp" '(previous-buffer :which-key "Previous Buffer")
    "bn" '(next-buffer :which-key "Next Buffer")
    "bb" '(switch-to-buffer :which-key "Switch Buffer")
    "bc" '(clone-indirect-buffer :which-key "Clone Buffer")
    "bd" '(kill-current-buffer :which-key "Kill Buffer")
    "bi" 'ibuffer
    "bm" '(bookmark-set :which-key "Set Bookmark")
    "bM" '(bookmark-delete :which-key "Delete Bookmark")
;; investigate bookmarks
    "bN" '(evil-buffer-new :which-key "New Empty Buffer")
    "br" '(revert-buffer :which-key "Revert Buffer")
    "bs" '(basic-save-buffer :which-key "Save Buffer")
    "bS" '(evil-write-all :which-key "Save All Buffers")
    
    ;; file
    "f" '(:ignore t :which-key "File")
    "fd" '(dired :which-key "Dired")
    "ff" '(find-file :which-key "Find File")
    "fs" '(save-buffer :which-key "Save File")
    "fS" '(write-file :which-key "Save File As")
    
    ;; quit
    "q" '(:ignore t :which-key "Quit")
    "qf" '(delete-frame :which-key "Delete Frame")
    "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
    
    ;; custom
    "o" '(:ignore t :which-key "Custom Entry")
    "ot" '(counsel-load-theme :which-key "Choose Theme")
    "oy" '(youdao-dictionary-search-at-point :which-key "Youdao Dict"))

  (general-define-key :keymaps 'evil-insert-state-map
                      (general-chord "jk") 'evil-normal-state)
  (general-define-key :keymaps 'evil-visual-state-map
                      (general-chord "jk") 'evil-normal-state)
  (general-define-key
   (general-chord ";;") 'evilnc-comment-or-uncomment-lines))

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

  (evil-global-set-key 'insert (kbd "C-u") 'undo)
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
    "Scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("q" nil "quit" :exit t))
  (w1/leader-key1
    "os" '(hydra-text-scale/body :which-key "Scale text"))

  (defhydra hydra-window-resize (:timeout 4)
    "Resize window"
    ("j" evil-window-increase-height "Increase height")
    ("k" evil-window-decrease-height "Decrease height")
    ("h" evil-window-decrease-width "Decrease width")
    ("l" evil-window-increase-width "Increase width")
    ("SPC" balance-windows "Balance windows")
    ("q" nil "quit" :exit t))
  (w1/leader-key1
    "wr" '(hydra-window-resize/body :which-key "Window resize"))
  )

(use-package avy
  :ensure t
  :custom
  (avy-all-windows nil))

(use-package youdao-dictionary
  :ensure t
  :custom
  (url-automatic-caching t)
  (youdao-dictionary-search-history-file (concat user-emacs-directory ".youdao"))
  :config
  (general-define-key
   :states 'normal
   :keymaps 'youdao-dictionary-mode-map
   "q" 'quit-window))

(use-package key-chord
  :ensure t
  :diminish key-chord-mode
  :init (key-chord-mode 1))

(use-package evil-nerd-commenter
  :ensure t)

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval 14)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode)
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :diminish counsel-peojectile-mode
  :after projectile
  :init (counsel-projectile-mode))

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :hook (git-commit-mode . evil-insert-state))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode))

(defun w1/org-mode-setup ()
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :ensure t
  ;; :hook (org-mode . (lambda () (electric-indent-local-mode -1)))
  :bind
  (:map org-mode-map ("C-j" . org-meta-return)) ;; This is for Ctrl+Enter in terminal mode
  :hook (org-mode . w1/org-mode-setup)
  :custom
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers nil) ;; Show bold and italic verbosely
  (org-link-descriptive nil) ;; Show links verbosely
  (org-hide-leading-stars t)
  )

(use-package org-superstar
  :ensure t
  :diminish org-superstar-mode
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :custom
  (org-superstar-headline-bullets-list '("■" "◆" "▲" "▶"))
  (org-superstar-cycle-headline-bullets nil)
  (org-superstar-prettify-item-bullets nil)
  )

(use-package evil-terminal-cursor-changer
  :ensure t
  :init
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate) ;; or (etcc-on)
    ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-superstar evil-terminal-cursor-changer evil-magit git-gutter magit counsel-projectile projectile auto-package-update evil-nerd-commenter youdao-dictionary key-chord keychord hydra avy evil-collection evil general amx doom-themes undo-fu helpful ivy-rich which-key rainbow-delimiters ranbow-delimiters ranbow-delimeters counsel swiper ivy command-log-mode use-package doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
