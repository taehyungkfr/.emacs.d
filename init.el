;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
(set-language-environment "UTF-8")
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024))

(let ((default-directory user-emacs-directory)
      (file-name-handler-alist nil))

  ;; 기본 설정파일 로딩
  (let* ((.org "my.org")
         (.el "my.el")
         (mtime (file-attribute-modification-time (file-attributes .org))))
    (require 'org-compat)
    (require 'org-macs)
    (unless (org-file-newer-than-p .el mtime)
      (require 'ob-tangle)
      ;; (message "Rewrite my config")
      (org-babel-tangle-file .org .el "emacs-lisp"))
    (load-file .el))

  ;; 개인 설정
  (when (file-exists-p "pers.el")
    (load-file "pers.el"))

  ;; 기본 디렉토리 변경
  (cd "~"))
