(setq-default evil-shift-width 2 ;; I normally use 2wide for my projects.
              tab-width 2)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Eduardo Lonighi"
      user-mail-address "eduloni3@gmail.com")

;;for some reason doom disables theese
(setq auto-save-default t
      make-backup-files t)

;;Disable exit confirmation.
(setq confirm-kill-emacs nil)

(setq select-enable-clipboard nil)

(defun paste-from-clipboard()
  (interactive)
  (setq select-enable-clipboard t)
  (yank)
  (setq select-enable-clipboard nil))

(defun copy-to-clipboard()
  (interactive)
  (setq select-enable-clipboard t)
  (kill-ring-save (region-beginning) (region-end))
  (set select-enable-clipboard nil))

(global-set-key (kbd "M-v") 'paste-from-clipboard)
(global-set-key (kbd "M-c") 'copy-to-clipboard)

  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-height 10)

  ;; nil, t, 'relative
  (setq display-line-numbers-type 'relative)

  ;; Disable line numbers in some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  eshel-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq doom-font (font-spec :family "Monaco" :size 35))
(setq doom-variable-pitch-font (font-spec :family "Cantarell" :size 38))

(setq doom-theme 'doom-material)

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

(setq org-directory "~/Documents/OrgFiles/")

(defun bumbler/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package! org
  :hook (org-mode . bumbler/org-mode-setup)
  :config
  (setq org-ellipsis " ▾▾▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

   (setq org-agenda-files
         '("~/Documents/OrgFiles/todo.org"
           ;; "~/Documents/OrgFiles/Tasks.org"
           "~/Documents/OrgFiles/Habits.org"))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

(setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

 (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

)

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
                        (expand-file-name "~/.doom.d/Configs.org"))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'bumbler/org-babel-tangle-config)))

(evil-define-key 'normal 'global (kbd "J") (kbd "5j"))
(evil-define-key 'normal 'global (kbd "K") (kbd "5k"))

(map! :leader
      :desc "File tree"
      "e" #'neotree-toggle)
