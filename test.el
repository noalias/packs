(pcase "~/.local/share/emacs-lisp"
  ((and (pred file-exists-p)
	dir)
   (delete-directory dir t)))

(require 'autoload)
(load "~/projects/packs/init-packs.el")

