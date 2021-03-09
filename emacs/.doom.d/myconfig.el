;; tab size
(setq-default evil-shift-width 2 ;; I normally use 2wide for my projects.
              tab-width 2)

;; tab bar
(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-height 18
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")

;; neotree
(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   ;; (flycheck-mode +1)
;;   ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   ;; (eldoc-mode +1)
  ;; (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency.
;;   (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq company-idle-delay  .3)
(setq company-minimum-prefix-length 2)
(setq company-show-numbers t)
(setq company-tooltip-limit 20)
(setq tide-completion-show-source t)
(setq tide-completion-detailed t)
(setq tide-always-show-documentation t)
(setq company-tooltip-maximum-width 90)
(setq company-box-tooltip-maximum-width 90)
(setq company-box-backends-colors nil)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; (defun my-backends ()
;;     (set (make-local-variable 'company-backends)
;;         '((company-capf ;; I think this must come first?
;;             :with
;;             company-yasnippet
;;             company-files
;;             company-dabbrev-code))))

;; (setq lsp-completion-provider :none)

  ;; company-backends '((company-files :with company-tide company-yasnippet)))
;; (add-hook! 'typescript-mode-hook
;;            (message "TYYYYYPED")
;;   company-backends '((company-yasnippet company-files)))
(after! typescript-mode
  (add-hook!  'before-save-hook #'lsp-eslint-apply-all-fixes)
    (defun ~+company-typescript-init-h ()
      (set-company-backend! 'typescript-mode nil)
      ;; (set-company-backend! 'typescript-mode '( company-tide :separated company-yasnippet company-files company-abbrev)))

    (setq lsp-completion-provider :none)

      (set-company-backend! 'typescript-mode '(company-yasnippet company-abbrev)))
    (add-hook 'typescript-mode-hook '~+company-typescript-init-h))


(use-package! company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))




(use-package! dired
  :config
  ;; (evil-collection-define-key 'normal 'dired-mode-map
    ;; "h"  'dired-up-directory
    ;; "l" 'dired-find-file)
  )



                (setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                                         ; Set width for tabs
 uniquify-buffer-name-style 'forward      ; Uniquify buffer names
 window-combination-resize t                    ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                                           ; Stretch cursor to the glyph width

(setq undo-limit 80000000                          ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                             ; By default while in insert all changes are one big blob. Be more granular
      inhibit-compacting-font-caches t      ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

;; (delete-selection-mode 1)                             ; Replace selection when inserting text
(display-time-mode 1)                                   ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words
(setq line-spacing 0.3)



                (setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

;;for some reason doom disables theese
(setq auto-save-default t
      make-backup-files t)

;;Disable exit confirmation.
(setq confirm-kill-emacs nil)

(setq x-select-enable-clipboard nil)

(defun paste-from-clipboard()
  (interactive)
  (setq x-select-enable-clipboard t)
  (yank)
  (setq x-select-enable-clipboard nil))

(defun copy-to-clipboard()
  (interactive)
  (setq x-select-enable-clipboard t)
  (kill-ring-save (region-beginning) (region-end))
  (set x-select-enable-clipboard nil))

(global-set-key (kbd "M-v") 'paste-from-clipboard)
(global-set-key (kbd "M-c") 'copy-to-clipboard)

  ;; nil, t, 'relative
  (setq display-line-numbers-type 'relative)

  ;; Disable line numbers in some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  eshel-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq doom-font (font-spec :family "Monaco" :size 30))
(setq doom-variable-pitch-font (font-spec :family "Cantarell" :size 30))
(setq doom-big-font (font-spec :family "Monaco" :size 40))

;; Good overall theme
(setq doom-theme 'doom-material)

;; similar to gruvbox, window separation very clear
;; (setq doom-theme 'doom-manegram)

;; banner
(defun my-banner-fn ()
  (let* ((banner
          '("                                   ____"
        "                                 /   () \\"
        "                          .--.  |   //   |  .--."
        "                         : (\\ \". \\ ____ / .\" /) :"
        "                          \".    `   ||     `  .\""
        "                           /    _        _    \\"
        "                          /     0}      {0     \\"
        "                         |       /      \\       |"
        "                         |      /        \\     |"
        "                          \\    |.  .==.  .|   /"
        "                           \"._ \\.  \\__/  ./ _.\""
        "                           /  ``\"._-\"\"-_.\"``  \\"
        "=========================================================================="
        "  _____     ____                                          ____     _____  "
        " /      \\  |  o |   See the TURTLE of  Enormous Girth    | o  |  /      \\ "
        "|        |/ ___\\|    On his shell he holds the Earth     |/___ \\|        |"
        "|_________/        His thought is slow, but always kind        \\_________|"
        "|_|_| |_|_|          He holds us all within his mind           |_|_| |_|_|"
        "                                                                          "
        "=========================================================================="
            ))
         (longest-line (apply #'max (mapcar #'length banner))))
        (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-functions
      '(my-banner-fn
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded))

    (setq doom-modeline-modal-icon nil)
    (setq doom-modeline-indent-info nil)
    ;; (set-face-attribute 'mode-line-inactive nil :height 95)
    ;; (set-face-attribute 'mode-line nil :height 95)
(custom-set-faces
  '(mode-line ((t (:family "Noto Sans" :height 0.75))))
  '(mode-line-inactive ((t (:family "Noto Sans" :height 0.75)))))

    (setq doom-modeline-height 22)

(defun bumbler/org-font-setup ()
  ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-block nil :font "Cantarell" :weight 'regular)
  ;; (set-face-attribute 'org-code nil :font "Cantarell" :weight 'regular)
  ;; (set-face-attribute 'org-table nil :font "Cantarell" :weight 'regular)
  ;; (set-face-attribute 'org-verbatim nil :font "Cantarell" :weight 'regular)
  ;; (set-face-attribute 'org-special-keyword nil :font "Cantarell" :weight 'regular)
  ;; (set-face-attribute 'org-meta-line nil :font "Cantarell" :weight 'regular)
  ;; (set-face-attribute 'org-checkbox nil :font "Cantarell" :weight 'regular))
  )

(setq org-directory "~/Documents/OrgFiles/")
(setq org-startup-align-all-tables t)


(defun bumbler/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package! org
  :hook (org-mode . bumbler/org-mode-setup)
  :config
  (setq org-ellipsis " ▾▾▾")
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-startup-folded t)


  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("todo.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-todo-keywords
    '((sequence "TODO(t)" "DOING(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;;========================================================= set the todo state of the parent
(defun org-todo-if-needed (state)
  "Change header state to STATE unless the current item is in STATE already."
  (unless (string-equal (org-get-todo-state) state)
    (org-todo state)))

(defun ct/org-summary-todo-cookie (n-done n-not-done)
  "Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to DOING otherwise"
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo-if-needed (cond ((= n-done 0)
                               "TODO")
                              ((= n-not-done 0)
                               "DONE")
                              (t
                               "DOING")))))
(add-hook! 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

(defun ct/org-summary-checkbox-cookie ()
  "Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to DOING otherwise"
  (let (beg end)
    (unless (not (org-get-todo-state))
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        ;; Regex group 1: %-based cookie
        ;; Regex group 2 and 3: x/y cookie
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                ;; [xx%] cookie support
                (cond ((equal (match-string 1) "100%")
                       (org-todo-if-needed "DONE"))
                      ((equal (match-string 1) "0%")
                       (org-todo-if-needed "TODO"))
                      (t
                       (org-todo-if-needed "DOING")))
              ;; [x/y] cookie support
              (if (> (match-end 2) (match-beginning 2)) ; = if not empty
                  (cond ((equal (match-string 2) (match-string 3))
                         (org-todo-if-needed "DONE"))
                        ((or (equal (string-trim (match-string 2)) "")
                             (equal (match-string 2) "0"))
                         (org-todo-if-needed "TODO"))
                        (t
                         (org-todo-if-needed "DOING")))
                (org-todo-if-needed "DOING"))))))))
(add-hook! 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)
;;=============================================================
 (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("public" . ?h)
       ("private" . ?H)
       ("errand" . ?E)
       ("home" . ?H)
       ("work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))


;;   (setq org-agenda-start-with-log-mode t)
   ;; (setq org-agenda-files
   ;;       '("~/Documents/OrgFiles/todo.org"
   ;;         "~/Documents/OrgFiles/habits.org"))
;;   (require 'org-habit)
;;   (add-to-list 'org-modules 'org-habit)
;;   (setq org-habit-graph-column 60)

;;   ;; Configure custom agenda views
;;   (setq org-agenda-custom-commands
;;    '(("d" "Dashboard"
;;      ((agenda "" ((org-deadline-warning-days 7)))
;;       (todo "NEXT"
;;         ((org-agenda-overriding-header "Next Tasks")))
;;       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

;;     ("n" "Next Tasks"
;;      ((todo "NEXT"
;;         ((org-agenda-overriding-header "Next Tasks")))))

;;     ("W" "Work Tasks" tags-todo "+work-email")

;;     ;; Low-effort next actions
;;     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
;;      ((org-agenda-overriding-header "Low Effort Tasks")
;;       (org-agenda-max-todos 20)
;;       (org-agenda-files org-agenda-files)))

;;     ("w" "Workflow Status"
;;      ((todo "WAIT"
;;             ((org-agenda-overriding-header "Waiting on External")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "REVIEW"
;;             ((org-agenda-overriding-header "In Review")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "PLAN"
;;             ((org-agenda-overriding-header "In Planning")
;;              (org-agenda-todo-list-sublevels nil)
;;              (org-agenda-files org-agenda-files)))
;;       (todo "BACKLOG"
;;             ((org-agenda-overriding-header "Project Backlog")
;;              (org-agenda-todo-list-sublevels nil)
;;              (org-agenda-files org-agenda-files)))
;;       (todo "READY"
;;             ((org-agenda-overriding-header "Ready for Work")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "ACTIVE"
;;             ((org-agenda-overriding-header "Active Projects")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "COMPLETED"
;;             ((org-agenda-overriding-header "Completed Projects")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "CANC"
;;             ((org-agenda-overriding-header "Cancelled Projects")
;;              (org-agenda-files org-agenda-files)))))))

 (bumbler/org-font-setup)
)

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)
(setq org-agenda-files
      '("~/Documents/OrgFiles/todo.org"
      "~/Documents/OrgFiles/habits.org"
      "~/Documents/OrgFiles/projects.org"))

(let ((org-agenda-span 'day)
      (org-super-agenda-groups
       '((:name "Time grid items in all-uppercase with RosyBrown1 foreground"
                :time-grid t
                :transformer (--> it
                                  (upcase it)
                                  (propertize it 'face '(:foreground "RosyBrown1"))))
         (:name "Priority >= C items underlined, on black background"
                :face (:background "black" :underline t)
                :not (:priority>= "C")
                :order 100))))
  (org-agenda nil "a"))

(setq org-agenda-custom-commands
      '(("z" "Super zaen view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Assignments"
                                 :tag "Assignment"
                                 :order 10)
                          (:name "Issues"
                                 :tag "Issue"
                                 :order 12)
                          (:name "Projects"
                                 :tag "Project"
                                 :order 14)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 13)
                          (:name "Research"
                                 :tag "Research"
                                 :order 15)
                          (:name "To read"
                                 :tag "Read"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

    ;; (use-package! org-projectile
    ;; :config
    ;;     (setq org-projectile-projects-file
    ;;       "~/Documents/OrgFiles/projects.org")
    ;;     ;; (org-projectile-per-project)
    ;;     ;; (setq org-projectile-per-project-filepath "todo.org")
    ;;     ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))

    ;;     )

(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  (defun bumbler/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package! visual-fill-column
    :hook (org-mode . bumbler/org-mode-visual-fill))

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

  ;; Automatically tangle our Emacs.org config file when we save it
  (defun bumbler/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/.dotfiles/emacs/.doom.d/Configs.org"))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))


(add-hook! org-mode
            (add-hook! 'after-save-hook  #'bumbler/org-babel-tangle-config))
  ;; (setq-hook! 'org-mode-hook (lambda () (setq-hook! 'after-save-hook #'bumbler/org-babel-tangle-config)))



(evil-define-key 'normal 'global (kbd "J") (kbd "5j"))
(evil-define-key 'normal 'global (kbd "K") (kbd "5k"))
;; (evil-define-key 'normal 'global (kbd "gbn") 'projectile-next-project-buffer)
;; (evil-define-key 'normal 'global (kbd "gbb") 'projectile-previous-project-buffer)
;; (evil-define-key 'normal 'global (kbd "bd") 'kill-this-buffer)

;; i keep using :W to save because dont release shift in time so
(evil-ex-define-cmd "W" 'evil-write)

(map! :leader
      :desc "File tree"
      "e" #'neotree-toggle)

(setq user-mail-address "eduloni3@gmail.com"
      user-full-name  "Eduardo Lonighi "
      ;; points to my mbsyncrc config
      mu4e-get-mail-command "mbsync -c ~/.config/mu4e-dt/mbsyncrc -a"
      mu4e-update-interval  300
      ;; mu4e-main-buffer-hide-personal-addresses t
      ;; message-send-mail-function 'smtpmail-send-it
      ;; starttls-use-gnutls t
      ;; smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))

  ;;NEEDED FOR MBSYNC
      mu4e-change-filenames-when-moving t
      mu4e-sent-folder "/eduloni3-gmail/Sent Mail"
      mu4e-drafts-folder "/eduloni3-gmail/Drafts"
      mu4e-trash-folder "/eduloni3-gmail/Bin"
      mu4e-refile-folder "/eduloni3-gmail/All Mail"

      ;; Try to show images
      mu4e-view-show-images t
     mu4e-show-images t
     mu4e-view-image-max-width 800

      mu4e-compose-signature "Eduardo Lonighi "
      ;; mu4e-maildir-shortcuts
      ;; '(("/eduloni3-gmail/Inbox"      . ?i)
      ;;   ("/eduloni3-gmail/Sent Items" . ?s)
      ;;   ("/eduloni3-gmail/Drafts"     . ?d)
      ;;   ("/eduloni3-gmail/Trash"      . ?t)))
)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs)))
  )
