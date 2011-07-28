;; loading personal init-file

(when (load (expand-file-name "~/.emacs.d/vince-init"))
  (vince-init-all))
