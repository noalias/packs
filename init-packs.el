;;; -*- lexical-binding: t -*-
(defvar noalias-packs-root "emacs-lisp"
  "")

(defvar noalias-packs--root
  (expand-file-name noalias-packs-root
                    (or (getenv "XDG_DATA_HOME") "~/.local/share")))

(defvar noalias-packs-cache "cache"
  "the directory resource files locate")

(defvar noalias-packs--cache
  (expand-file-name noalias-packs-cache noalias-packs--root))

(defconst noalias-packs-re (rx (+? (not (or ?. ?/)))
                               ?/
                               (group (* (not ?.)) (? ?. (* nonl)))
                               eos)
  "Package 必须符合 \"user/repo\" 的形式")

(cl-defstruct (noalias-packs--pkg
               (:constructor noalias-packs--make))
  path repo pu-repo)

(defun noalias-packs-make (pkg)
  (if (string-match-p noalias-packs-re pkg)
      (noalias-packs--make pkg)
    (user-error   "Package 必须符合 \"user/repo\" 的形式")))

(defvar noalias-packs--loaded-files nil)
(defun noalias-packs--use (pkg)
  (let* ((pkg (noalias-packs--make pkg))
         (autoload-file (format "%s.elc" (noalias-packs--pkg-pu-repo pkg)))
         (autoload-files noalias-packs--loaded-files)))
  (unless autoload-files
    ))

(defun noalias-packs--install (s-pkg)
  (let ((default-directory noalias-packs--cache))
    (unless (file-exists-p  ".git")
      (shell-command "git init"))
    (if (fboundp 'magit-submodule-add)
        (magit-submodule-add pkg)
      (shell-command
       (format "git submodule add --depth 1 git@github.com:%s.git"
               (noalias-packs--pkg-path s-pkg))))))

(defvar noalias-packs--source-dir-maybe "lisp")
(defun noalias-packs--build (s-pkg &optional source-dir doc-dir)
  (let* ((target-dir (noalias-packs--pkg-repo s-pkg))
         (result-dir (noalias-packs--pkg-pu-repo s-pkg))
         (build (lambda (target result)
                  (let ((dir (expand-file-name
                              (or source-dir noalias-packs--source-dir-maybe) target)))
                    (and (file-exists-p dir)
                         (setq target dir)))
                  ;; the target directory's created when pkg's installed,
                  ;; now the result directory need to be created.
                  (or (file-exists-p result)
                      (make-directory result t))
                  ;; update load-path
                  (push result load-path)
                  ;; build autoload file for elisp files
                  (let* ((default-directory result)
                         (inhibit-message t)
                         (find-file-hook nil)
                         (write-file-functions nil)
                         (backup-inhibited t)
                         (generated-autoload-file (expand-file-name "autoloads.el"))
                         ;; 检查文件是否更新
                         (newer nil))
                    ;; make soft link to source files
                    (dolist (file (directory-files target t "\\.el$"))
                      (let* ((base (file-name-nondirectory file))
                             (byte-file (concat (file-name-sans-extension base ".elc"))))
                        (unless (string-prefix-p "." base)
                          (make-symbolic-link file base t))
                        (when (file-newer-than-file-p file byte-file)
                          (setq newer t)
                          (byte-compile-file byte-file))))
                    ;; 创建 autoloadfile
                    (unless (file-exists-p generated-autoload-file)
                      (with-current-buffer (find-file-noselect generated-autoload-file)
                        (insert ";; -*- lexical-binding: t -*-\n")
                        (save-buffer)))
                    ;; 更新 autoloadfile
                    (when newer
                      (update-directory-autoloads "")
                      (byte-compile-file generated-autoload-file))
		            ;; 加载 autoload file
                    (load "autoloads" nil 'nomessage)))))
    ;; building
    (funcall build
             (expand-file-name target-dir noalias-packs--cache)
             (expand-file-name result-dir noalias-packs--root))))

(defun noalias-packs--init ()
  (or (executable-find name)
      (user-error "Cannot find required executable: %s" name))
  (unless (file-exists-p noalias-packs--cache)
    (make-directory noalias-packs--cache t)
    ;; get required packages
    (let ((pkgs '("magnars/dash.el" "magit/with-editor" "magit/magit")))
      (when (version< emacs-version "28.0")
        (push "magit/translation" pkgs))
      (dolist (pkg pkgs)
        (noalias-packs--install pkg)
        (noalias-packs--build pkg)))))

(defun noalias-packs--use (pkg)
  (let ((pkg (noalias-packs--make-pkg pkg)))
    (unless ())))






;;; init
(noalias-packs--init)
(require 'magit-git)

(defvar noalias-packs--list nil
  "The list of installed packages")

(let ((default-directory noalias-packs--cache))
  (magit-list-module-names))

(provide 'init-packs)
