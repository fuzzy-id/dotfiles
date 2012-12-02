(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(if (file-exists-p "/usr/bin/conkeror")
    (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "/usr/bin/conkeror"))

;; to avoid
;; Symbol's value as variable is void: custom-theme-load-path
(setq custom-theme-load-path '())
;; to avoid anoying popups when decrypting gpg
(setenv "GPG_AGENT_INFO" nil)

(setq el-get-sources
      '((:name autopair
	       :post-init (progn
			    (autopair-global-mode t)))
	(:name auctex
	       :post-init (progn
			    (setq-default TeX-master nil)))
	(:name color-theme-zenburn
	       :after (load-theme 'zenburn))
	(:name org-mode
	       :after (progn
			(require 'org-exp-bibtex)
			(setq org-latex-to-pdf-process
			      '("cd %o && latexmk -pdf %f"))
			(require 'org-latex)
			(add-to-list 'org-export-latex-classes
				     (list "scrartcl" 
					   (concat "\\documentclass[11pt]{scrartcl}\n")
					   '("\\section{%s}" . "\\section*{%s}")
					   '("\\subsection{%s}" . "\\subsection*{%s}")
					   '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					   '("\\paragraph{%s}" . "\\paragraph*{%s}")
					   '("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
			(add-to-list 'org-export-latex-classes
				     (list "acmtog"
					   (concat "\\documentclass{acmtog}\n"
						   "\\usepackage{amsmath}\n")
					   '("\\section{%s}" . "\\section*{%s}")
					   '("\\subsection{%s}" . "\\subsection*{%s}")
					   '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					   '("\\paragraph{%s}" . "\\paragraph*{%s}")
					   '("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
			(add-to-list 'org-export-latex-classes
				     (list "scrreprt"
					   (concat "\\documentclass{scrreprt}\n"
						   "\\usepackage{amsmath}\n")
					   '("\\chapter{%s}" . "\\chapter*{%s}")
					   '("\\section{%s}" . "\\section*{%s}")
					   '("\\subsection{%s}" . "\\subsection*{%s}")
					   '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					   '("\\paragraph{%s}" . "\\paragraph*{%s}")
					   '("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
			(setq org-export-latex-default-class "scrartcl")
			(add-to-list 'org-export-latex-packages-alist '("" "centernot" t))
			(setq org-export-latex-listings 'minted)
			(setq org-export-latex-minted-options
			      '(("frame" "none")
				("fontsize" "\\footnotesize")
				("linenos" "false")))
			(setq org-export-pdf-remove-logfiles 'nil)
			(add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))

			(setq vince-org-directory (expand-file-name "~/crypt/org"))
			(setq org-todo-keywords
			      '((sequence "TODO(t!)" "WAIT(w@)" 
					  "|" "DONE(d!)" "CANCELED(c@)")))
			(setq org-capture-templates
			      '(("t" "Enter a Todo Item")
				("tp" "Plain todo item" entry
				 (file+headline (concat vince-org-directory 
							"/gtd.org") "Tasks")
				 "* TODO %?\n  + created :: %U")
				("tl" "Todo item with a link to the current buffer" entry
				 (file+headline (concat vince-org-directory 
							"/gtd.org") "Tasks")
				 "* TODO %?\n  + created :: %U\n  + link :: %A")
				("n" "Just a simple note")
				("np" "Plain note." entry
				 (file+headline (concat vince-org-directory 
							"/notes.org") "New")
				 "* %?\n  + created :: %U")
				("nl" "A note with a link." entry
				 (file+headline (concat vince-org-directory
							"/notes.org") "New")
				 "* %?\n  + created :: %U\n  + link :: %A")
				("w" "" entry ;; 'w' for 'org-protocol'
				 (file+headline (concat vince-org-directory 
							"/www.org") "Notes")
				 "* %^{Title}\n\n  Source: %u, %c\n\n  %i")))
			(define-key global-map (kbd "C-c c") 'org-capture)
			(global-set-key (kbd "C-c a") 'org-agenda)
			(setq org-agenda-start-on-weekday nil)
			(setq org-agenda-custom-commands
			      '(("u" "Unscheduled TODO entries." 
				 alltodo "" 
				 ((org-agenda-skip-function '(org-agenda-skip-entry-if
							      'scheduled
							      'deadline))))))
			(define-key global-map (kbd "C-c l") 'org-store-link)
			(require 'org-drill)))
	(:name org2blog
	       :depends "xml-rpc-el"
	       :after (progn
			(setq org2blog/wp-blog-alist
			      '(("vinceblog"
				 :url "http://vincebox.webfactional.com/xmlrpc.php"
				 :username "admin")))))
	(:name cedet
	       :after (progn 
			;; Add further minor-modes to be enabled by semantic-mode.
			;; See doc-string of `semantic-default-submodes' for other things
			;; you can use here.
			(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
			(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
			(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

			;; Enable Semantic
			(semantic-mode 1)

			;; Enable EDE (Project Management) features
			(global-ede-mode 1)))
	(:name emms
	       :after (progn
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
  (column-number-mode 1)
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

(setq tramp-default-method "ssh")

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("emacs-config"
	  (or
	   (filename . ".emacs")
	   (filename . ".emacs.d/"))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(add-to-list 'auto-mode-alist '("\\.pt\\'" . html-mode))
(add-to-list 'auto-mode-alist '("/tmp/mutt-.*" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

(defun vince-p-imap-process (process)
  "Returns `t' if `process' is an imap-process."
  (and (processp process)
       (string-match-p "^imap.*$" (process-name process))))

(defun vince-search-imap-processes-in-list (processes)
  "Searches for imap processes in the given list."
  (if processes
      (let ((this-process (pop processes)))
	(if (vince-p-imap-process this-process)
	    (cons this-process
		  (vince-search-imap-processes-in-list processes))
	  (vince-search-imap-processes-in-list processes)))))

(defun vince-kill-imap-processes ()
  "Kills all running imap processes."
  (interactive)
  (mapcar 'kill-process 
	  (vince-search-imap-processes-in-list (process-list))))

(defun vince-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun vince-count-paragraph-starts-region (beginning end)  
  "Returns the number of starting paragraphs in the region."
  (save-excursion
    (let ((count 0))
      (goto-char beginning)
      (while (and (< (point) end)
		  (re-search-forward "^\\s *\n.*?\\w" end t))
        (setq count (1+ count)))
      (symbol-value 'count))))

(defun vince-org-cite-paragraph ()
  (interactive)
  (let* ((heading (vince-org-get-previous-heading))
	 (paragraph (vince-org-get-this-paragraph-count))
	 (cite (format "%s, §%d" heading paragraph)))
    (kill-new cite)
    (message (format "Pushed '%s' to kill ring." cite))))

(defun vince-org-get-previous-heading ()
  (save-excursion
    (outline-previous-heading)
    (let* ((re-todo (mapconcat 'identity org-todo-keywords-1 "\\|"))
	   (start (re-search-forward (format "\\*+\\s +\\(%s\\)*\\s *" re-todo)))
	   (end (re-search-forward "\\s *\\($\\|–\\)"))
	   (header (buffer-substring start end)))
      (replace-regexp-in-string "~–$" ""
				(replace-regexp-in-string "\\s " "~" header)))))

(defun vince-org-get-this-paragraph-count ()
  (save-excursion
    (let ((point-in-paragraph (point)))
      (outline-previous-heading)
      (vince-count-paragraph-starts-region (point) point-in-paragraph))))

(defun vince-paragraph-start-counter (beginning end)
  (interactive "r")
  (let ((paragraph-starts (vince-count-paragraph-starts-region beginning end)))
    (cond ((zerop paragraph-starts)
	   (message "There are no starts."))
	  ((= 1 paragraph-starts)
	   (message "There is 1 start."))
	  (t
	   (message "There are %d starts." paragraph-starts)))))
