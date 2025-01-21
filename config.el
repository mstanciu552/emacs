(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))


(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

(use-package evil :ensure t :demand t :init (setq evil-want-keybinding nil) (evil-mode))
(use-package evil-collection :after evil :ensure t :demand t :config (setq evil-collection-mode-list '(dashboard dired ibuffer)) (evil-collection-init))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'dtmacs t)

(use-package all-the-icons :ensure t :if (display-graphic-p))
(use-package all-the-icons-dired :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(set-face-attribute 'default nil
  :font "JetBrainsMono NF SemiBold"
  :height 110)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono NF SemiBold"
  :height 110)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF SemiBold-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)

(use-package general
        :ensure t
        :config
        (general-evil-setup)

        (general-create-definer leader-keys
          :states '(normal insert visual emacs)
          :keymaps 'override
          :prefix "SPC"
          :global-prefix "M-SPC")

        (leader-keys
          "b" '(:ignore t :wk "Buffer")
          "bb" '(switch-to-buffer :wk "Switch buffer")
          "bi" '(ibuffer :wk "IBuffer")
          "bk" '(kill-this-buffer :wk "Kill this buffer")
          "bn" '(next-buffer :wk "Next buffer")
          "bp" '(previous-buffer :wk "Previous buffer")
          "br" '(revert-buffer :wk "Reload buffer"))


        (leader-keys
          "f" '(:ignore t :wk "File")
          "f f" '(find-file :wk "Find File")
          "f l" '(load-file :wk "Load File")
          "f r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload Config File")
          "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Open Config File")
          )


        (leader-keys "n" '(neotree-toggle :wk "Toggle neotree file viewer"))

    (leader-keys 
    "d" '(:ignore t :wk "Dired")
      "d n" '((lambda () (interactive) (dired "~/Documents/Notes")) :wk "Open Notes Folder in Dired")
    )

        (leader-keys
          "w" '(:ignore t :wk "Window")
          "w c" '(evil-window-delete :wk "Close window")
          "w n" '(evil-window-new :wk "New Window")
          "w s" '(evil-window-new :wk "Horizontal Split Window")
          "w v" '(evil-window-new :wk "Vertical Split Window")
          )

(leader-keys
   "g" '(:ignore t :wk "Git")    
   "g /" '(magit-displatch :wk "Magit dispatch")
   "g ." '(magit-file-displatch :wk "Magit file dispatch")
   "g b" '(magit-branch-checkout :wk "Switch branch")
   "g c" '(:ignore t :wk "Create") 
   "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
   "g c c" '(magit-commit-create :wk "Create commit")
   "g c f" '(magit-commit-fixup :wk "Create fixup commit")
   "g C" '(magit-clone :wk "Clone repo")
   "g f" '(:ignore t :wk "Find") 
   "g f c" '(magit-show-commit :wk "Show commit")
   "g f f" '(magit-find-file :wk "Magit find file")
   "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
   "g F" '(magit-fetch :wk "Git fetch")
   "g g" '(magit-status :wk "Magit status")
   "g i" '(magit-init :wk "Initialize git repo")
   "g l" '(magit-log-buffer-file :wk "Magit buffer log")
   "g r" '(vc-revert :wk "Git revert file")
   "g s" '(magit-stage-file :wk "Git stage file")
   "g t" '(git-timemachine :wk "Git time machine")
   "g u" '(magit-stage-file :wk "Git unstage file"))

 (leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s m" '(man :wk "Man pages")
    "s o" '(pdf-occur :wk "Pdf search lines matching STRING")
    "s t" '(tldr :wk "Lookup TLDR docs for a command")
    "s w" '(woman :wk "Similar to man but doesn't require man"))


  (leader-keys
      "w" '(:ignore t :wk "Windows/Words")
      ;; Window splits
      "w c" '(evil-window-delete :wk "Close window")
      "w n" '(evil-window-new :wk "New window")
      "w s" '(evil-window-split :wk "Horizontal split window")
      "w v" '(evil-window-vsplit :wk "Vertical split window")
      ;; Window motions
      "w h" '(evil-window-left :wk "Window left")
      "w j" '(evil-window-down :wk "Window down")
      "w k" '(evil-window-up :wk "Window up")
      "w l" '(evil-window-right :wk "Window right")
      "w w" '(evil-window-next :wk "Goto next window")
      ;; Move Windows
      "w H" '(buf-move-left :wk "Buffer move left")
      "w J" '(buf-move-down :wk "Buffer move down")
      "w K" '(buf-move-up :wk "Buffer move up")
      "w L" '(buf-move-right :wk "Buffer move right")
      ;; Words
      "w d" '(downcase-word :wk "Downcase word")
      "w u" '(upcase-word :wk "Upcase word")
      "w =" '(count-words :wk "Count words/lines for buffer"))

        )

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package diminish)

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns 20
	which-key-min-display-lines 15
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator " => " ))

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(elpaca transient)
(use-package magit :after transient)

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets :ensure t :demand t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require 'org-tempo)

(setq org-startup-with-inline-images t)

(use-package org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 20      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package neotree
:config
(setq neo-smart-open t
      neo-show-hidden-files t
      neo-window-width 55
      neo-window-fixed-size nil
      inhibit-compacting-font-caches t
      projectile-switch-project-action 'neotree-projectile-action) 
      ;; truncate long file names in neotree
      (add-hook 'neo-after-create-hook
         #'(lambda (_)
             (with-current-buffer (get-buffer neo-buffer-name)
               (setq truncate-lines t)
               (setq word-wrap nil)
               (make-local-variable 'auto-hscroll-mode)
               (setq auto-hscroll-mode nil)))))

(use-package ivy
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package counsel :after ivy :config (counsel-mode))

(use-package all-the-icons-ivy-rich :ensure t :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))
