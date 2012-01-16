(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq el-get-sources
      '((:name color-theme-zenburn
	       :depends "color-theme"
	       :type git
	       :url "https://github.com/bbatsov/zenburn-emacs.git"
	       :features "color-theme-zenburn"
	       :post-init (lambda () (color-theme-zenburn)))
	(:name bbdb
	       :build (list "autoconf"
			    (concat "./configure --with-emacs=" el-get-emacs)
			    "make all")
	       :after (lambda ()
			(require 'bbdb-gnus)
			(require 'bbdb-message)
			(require 'bbdb-migrate)
			(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
			(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
			(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
			(bbdb-initialize 'gnus 'message)))
	(:name org-mode
	       :after (lambda ()
			(require 'org-exp-bibtex)
			(setq vince-org-directory (expand-file-name "~/org"))
			(setq org-capture-templates
			      '(("t" "Enter a Todo Item")
				("tp" "Plain todo item" entry
				 (file+headline (concat vince-org-directory "/gtd.org") "Tasks")
				 "* TODO %?\n  + created :: %U")
				("tl" "Todo item with a link to the current buffer" entry
				 (file+headline (concat vince-org-directory "/gtd.org") "Tasks")
				 "* TODO %?\n  + created :: %U\n  + link :: %A")
				("n" "Just a simple note" entry
				 (file+headline (concat vince-org-directory "/notes.org") "Tasks")
				 "* %?\n  + created :: %U")
				("d" "Todo item for domatix." entry
				 (file+headline (expand-file-name "~/gits/domatix/gtd.org") "Tasks")
				 "* TODO %?\n  + created :: %U\n")
				("w" "" entry ;; 'w' for 'org-protocol'
				 (file+headline (concat vince-org-directory "www.org") "Notes")
				 "* %^{Title}\n\n  Source: %u, %c\n\n  %i")))
			(define-key global-map "\C-cc" 'org-capture)
			(global-set-key "\C-ca" 'org-agenda)
			(setq org-agenda-start-on-weekday nil)
			(setq org-agenda-files 
			      (list (concat vince-org-directory "/gtd.org")
				    (expand-file-name "~/gits/domatix/gtd.org")))
			(setq org-agenda-custom-commands
			      '(("u" "Unscheduled TODO entries." 
				 alltodo "" 
				 ((org-agenda-skip-function '(org-agenda-skip-entry-if
							      'scheduled
							      'deadline))))))
			(define-key global-map (kbd "C-c l") 'org-store-link)))
	(:name ropemacs
	       :depends "pymacs"
	       :features "pymacs"
	       :after (lambda ()
			(defun vince-load-ropemacs ()
			  "Load pymacs and ropemacs"
			  (interactive)
			  (pymacs-load "ropemacs" "rope-")
			  ;; Automatically save project python buffers 
			  ;; before refactorings
			  (setq ropemacs-confirm-saving 'nil))
			(global-set-key "\C-xpl" 'vince-load-ropemacs)))
	(:name cedet
	       :load "common/cedet.el"
	       :info "common"
	       :after (lambda ()
			(srecode-minor-mode 1)
			(semantic-load-enable-guady-code-helpers)
			(global-cedet-m3-minor-mode 1)
			(global-ede-mode 1)
			(add-hook 'speedbar-load-hook
				  (lambda () (require 'semantic-sb)))
			(defun vince-speedbar-vc-git (directory)
			  (file-exists-p (concat directory ".git")))
			(add-hook 'speedbar-vc-directory-enable-hook
				  'vince-speedbar-vc-git)))
	(:name emms
	       :after (lambda ()
			(emms-standard)
			(emms-default-players)
			(global-set-key (kbd "C-c m l") 'emms-playlist-mode-go)
			(global-set-key (kbd "C-c m s") 'emms-pause)
			(global-set-key (kbd "C-c m n") 'emms-next)
			(global-set-key (kbd "C-c m p") 'emms-previous)
			(define-key dired-mode-map
			  (kbd "C-c m a") 'emms-add-dired)))))

(el-get)

(defun vince-set-look-and-feel ()
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (ido-mode 1)
  (setq vc-follow-symlinks 't)
  (require 'uniquify)
  (setq uniquify-buffer-name-style
	'post-forward-angle-brackets)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (vince-set-parentheses-behaviour)
  (vince-init-dired))

(defun vince-init-dired ()
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target 't))

(defun vince-set-parentheses-behaviour ()
  (show-paren-mode 't)
  (setq show-paren-style 'mixed))

(vince-set-look-and-feel)


(setq erc-nick "fuzzy_id")
(setq erc-autojoin-channels-alist '(("freenode.net" "#pyramid")))
