(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq user-mail-address "bachth@uni-mainz.de")
(setq user-full-name "Thomas Bach")

(setq gnus-select-method
      '(nntp "news.informatik.uni-mainz.de"))

(setq gnus-secondary-select-methods
      '((nnimap "uni-mainz.de"
		(nnimap-address "mail.uni-mainz.de")
		(nnimap-server-port 993)
		(nnimap-stream ssl)
		(nnimap-authinfo-file "~/.authinfo.gpg"))
	(nnimap "students.uni-mainz.de"
		(nnimap-address "mail.students.uni-mainz.de")
		(nnimap-server-port 993)
		(nnimap-stream ssl)
		(nnimap-authinfo-file "~/.authinfo.gpg"))))

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "mail.uni-mainz.de")
(setq smtpmail-auth-credentials "~/.authinfo.gpg")
(setq smtpmail-starttls-credentials '(("mail.uni-mainz.de" 25 nil nil)))
(setq gnus-message-archive-group "nnimap+uni-mainz.de:Gesendete Objekte")
