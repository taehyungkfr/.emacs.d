(setq package-enable-at-startup nil)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil
          native-comp-deferred-compilation-deny-list '("my.el" "pers.el")
          comp-deferred-compilation t
          package-native-compile t)
    (add-to-list 'native-comp-eln-load-path
                 (expand-file-name "eln-cache" user-emacs-directory))))
