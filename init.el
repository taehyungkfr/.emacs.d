;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
(set-language-environment "UTF-8")
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024))

(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	      (url-retrieve-synchronously
	       "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-check-for-modifications nil)
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;;(setq use-package-verbose 'debug)
;;(setq use-package-minimum-reported-time 0)

(use-package org :straight (:type built-in))

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
      (org-babel-tangle-file .org .el "emacs-lisp"))
    (load-file .el))

  ;; 개인 설정
  (when (file-exists-p "pers.el")
    (load-file "pers.el")))

;; 기본 디렉토리 변경
(cd "~")
