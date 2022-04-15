(pcase "~/.local/share/emacs-lisp"
  ((and (pred file-exists-p)
	dir)
   (delete-directory dir t)))

(load "~/projects/packs/init-packs.el")

