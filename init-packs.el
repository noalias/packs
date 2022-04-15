;;; -*- lexical-binding: t -*-
(require 'rx)
(require 'cl-lib)

(defvar noalias-packs-root "emacs-lisp")

(defvar noalias-packs--root
  (expand-file-name noalias-packs-root
                    (or (getenv "XDG_DATA_HOME") "~/.local/share")))

(defvar noalias-packs-cache "cache"
  "the directory resource files locate")

(defvar noalias-packs--cache
  (expand-file-name noalias-packs-cache noalias-packs--root))

(defun noalias-packs--make (pkg)
  (or
   (pcase pkg
     ((rx (+? (not (or ?. ?/)))
          ?/
          (let repo
            (let name (* (not ?.))) (? ?. (* nonl)))
          eos)
      (list pkg repo name)))
   (user-error "Package 必须符合 \"user/repo\" 的形式")))

(defvar noalias-packs--autoloadfile-format "%s-autoloads")
(defvar noalias-packs--loaded-files '())
(defun noalias-packs--use (pkg source-dir)
  "Install build & load package."
  (pcase-let ((`(,path ,repo ,name) (noalias-packs--make pkg)))
    (let ((autoload-file (format noalias-packs--autoloadfile-format name))
          (autoload-files noalias-packs--loaded-files)
          file-loaded)
      ;; 获取 load-files 信息
      (or autoload-files
          (pcase-dolist (`(,file . ,_) load-history)
            (push file autoload-files)))
      ;; 判断 autoload-file 是否加载    
      (dolist (file autoload-files)
	(and (string= autoload-file (file-name-base file))
             (setq file-loaded t)))
      ;; 如果未加载
      (unless file-loaded
          ;; 如果 pkg 不存在则安装
          (let ((default-directory noalias-packs--cache))
            ;; 判断是否初始化，即 magit 是否已经加载
            (if (not noalias-packs--inited)
                (progn
                  (or (file-exists-p ".git")
                      (shell-command "git init"))
                  (or (file-exists-p repo)
                      (shell-command (format "git submodule add --depth 1 git@github.com:%s.git" path))))
              (require 'magit-git)
              (require 'magit-clone)
              (unless (member repo (magit-list-module-names))
                (let ((url (magit-clone--name-to-url path)))
                  (magit-submodule-add url)))))
          ;; 如果 pkg 未构建则构建
          (let ((origin (expand-file-name repo noalias-packs--cache))
                (target (expand-file-name name noalias-packs--root)))
            (unless (member name (directory-files noalias-packs--root))
              (noalias-packs--build origin target source-dir))
            (let ((autoload-file (expand-file-name (concat autoload-file ".elc") target)))
              ;; 加载 autoload-file
              (load autoload-file)
              ;; 更新 loaded-files
              (push autoload-file autoload-files))))
      (or (eql (length noalias-packs--loaded-files)
              (length autoload-files))
          (setq noalias-packs--loaded-files autoload-files)))))

(defun noalias-packs--build (origin target  &optional source-dir)
  (pcase origin
    ((and (app (expand-file-name source-dir) dir)
          (guard (file-exists-p dir)))
     (setq origin dir)))
  ;; the target directory's created when pkg's installed,
  ;; now the result directory need to be created.
  (or (file-exists-p target)
      (make-directory target t))
  ;; 创建 build 环境
  (push target load-path)
  ;; build autoload file for elisp files
  (let* ((default-directory target)
         (inhibit-message t)
         (find-file-hook nil)
         (write-file-functions nil)
         (backup-inhibited t)
         (generated-autoload-file
          (concat (format noalias-packs--autoloadfile-format
                          (file-name-base (directory-file-name target)))
                  ".el"))
         ;; 检查文件是否更新
         (newer nil))
    ;; make soft link to source files
    (dolist (file (directory-files origin t "\\.el$"))
      (let* ((base (file-name-nondirectory file))
             (byte-file (concat (file-name-sans-extension base) ".elc")))
        (unless (string-prefix-p "." base)
          (make-symbolic-link file base t)
          (when (file-newer-than-file-p file byte-file)
            (setq newer t)))))
    ;; 创建 autoloadfile
    (unless (file-exists-p generated-autoload-file)
      (with-current-buffer (find-file-noselect generated-autoload-file)
	(insert ";; -*- lexical-binding: t -*-\n")
	(save-buffer)))
    ;; 更新 autoloadfile
    (when newer
      (make-directory-autoloads "" generated-autoload-file)
      (byte-recompile-directory default-directory 0 nil t))))

(cl-defun noalias-packs-use (pkg &key (source-dir "lisp"))
  (interactive "sInput the repo: ")
  (noalias-packs--use pkg source-dir))

(defvar noalias-packs--inited nil)
(defun noalias-packs--init ()
  (or (executable-find "git")
      (user-error "Cannot find required executable: %s" "git"))
  (unless (file-exists-p noalias-packs--cache)
    (make-directory noalias-packs--cache t))
    ;; get required packages
  (let ((pkgs '("magnars/dash.el" "magit/with-editor" "magit/magit")))
    (when (version< emacs-version "28.0")
      (push "magit/translation" pkgs))
    (dolist (pkg pkgs) (noalias-packs-use pkg)))
  (setq noalias-packs--inited t))

;;; init
(noalias-packs--init)

(provide 'init-packs)
