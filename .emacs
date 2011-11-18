;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(when (load (expand-file-name "~/.emacs.d/vince-init.el"))
  (vince-init-all))

(add-to-list 'semanticdb-project-roots 
	     "/home/vince/src/pyramid_install/wp_frontend/wp_frontend")
(semantic-add-system-include 
 "/home/vince/src/pyramid_install/lib/python2.7" 'python-mode)

(setq erc-nick "fuzzy_id")
