;;; saaxy.el --- saaxy

;; Author: Konrad Scorciapino <konr@konr.mobi>
;; Keywords: Bot, productivity, personal assistant, SaaS

;;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;;                    Version 2, December 2004
;;; 
;;; Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
;;; 
;;; Everyone is permitted to copy and distribute verbatim or modified
;;; copies of this license document, and changing it is allowed as long
;;; as the name is changed.
;;; 
;;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;; 
;;;  0. You just DO WHAT THE FUCK YOU WANT TO.

;;; Code

(require 'saaxy-commands)
(require 'pp)


;; Buffers and silly doctor
(defvar saaxy-buffer-name "Saaxy!")
(defvar saaxy-doctor-buffer-name "Saaxy! - Doctor")
(defvar saaxy-noncommand-handler #'saaxy-aux-doctor)


(defvar saaxy-prompt (propertize "➤ " 'font-lock-face `(:foreground "#f00")))
(defvar saaxy-history nil)
(defvar saaxy-history-cycle nil)
(defvar saaxy-welcome-message
  (format "Welcome to %s! Fortune and glory await.\n" (propertize "Saaxy" 'font-lock-face '(:foreground "#944"))))
(defvar saaxy-input nil)
(defvar saaxy-output nil)
(defvar saaxy-context nil)
(defvar saaxy-commands
  '((name monica class adult function (lambda (a al) (saaxy-async-command (format "http://monicastube.com/search/?q=%s" (url-hexify-string a)) #'saaxy-aux-monica)))
    (name freebase class web function (lambda (a al) (saaxy-async-command (format "https://www.googleapis.com/freebase/v1/search?indent=true&query=%s" (url-hexify-string a))
                                                                     #'saaxy-aux-freebase)))
    (name reddit class web function (lambda (a al) (saaxy-async-command (format "http://www.reddit.com/r/%s/.json" (url-hexify-string a)) #'saaxy-aux-reddit)))
    (name ddg class web function (lambda (a al) (saaxy-async-command (format "http://duckduckgo.com/lite/?kp=-1&kl=br-pt&k1=-1&q=%s" (url-hexify-string a)) #'saaxy-aux-ddg)))
    (name help class sys function saaxy-aux-help)
    (name au class portuguese function  (lambda (a al) (saaxy-async-command (format "http://www.dicionariodoaurelio.com/%s.html" (url-hexify-string a)) #'saaxy-aux-au)))
    (name mic class portuguese function (lambda (a al) (saaxy-async-command (format "http://michaelis.uol.com.br/moderno/portugues/index.php?lingua=portugues-portugues&palavra=%s"
                                                                               (url-hexify-string a)) #'saaxy-aux-mic)))
    (name por class portuguese function (lambda (a al) (saaxy-async-command (format "http://www.infopedia.pt/pesquisa-global/%s" (url-hexify-string a)) #'saaxy-aux-por)))
    (name wa class web function (lambda (a al) (saaxy-async-command (format "http://m.wolframalpha.com/input/?i=%s&asynchronous=false" (url-hexify-string a)) #'saaxy-aux-wa)))
    (name sh class sys function (lambda (a al) (shell-command-to-string a)))
    (name eval class sys function (lambda (a al) (format "%s\n" (eval (read a)))))
    (name bye class sys function (lambda (a al) (kill-buffer (current-buffer))))
    (name pp  class sys function (lambda (a al) (format "%s\n" (pp-to-string (eval (read a))))))
    (name tinycc class url function 'saaxy-aux-tinycc)
    (name tt class productivity function (lambda (a al) (saaxy-aux-tomatinho 'tubes)))
    (name th class productivity function (lambda (a al) (saaxy-aux-tomatinho 'history)))
    (name now class productivity function (lambda (a al) (saaxy-aux-now a al)))
    (name clean class sys function (lambda (a al) (setq saaxy-history nil) "History cleaned!"))
    (name say class sys function (lambda (a al) (saaxy-aux-say a al)))
    (name dict class english function saaxy-aux-wordnet-call)
    (name trans class web function (lambda (a al) (saaxy-aux-trans a al)))
    (name latin class latin function (lambda (a al) (saaxy-async-command (format "http://www.archives.nd.edu/cgi-bin/wordz.pl?keyword=%s" (url-hexify-string a)) #'saaxy-aux-latin)))
    (name tolat class latin function (lambda (a al) (saaxy-async-command (format "http://www.archives.nd.edu/cgi-bin/wordz.pl?english=%s" (url-hexify-string a)) #'saaxy-aux-latin)))
    (name vadio class portuguese function (lambda (a al) (saaxy-async-command (format "http://poetavadio.com/rhymedic_search.php?word=%s&terml=%s&nres=10000000"
                                                                                 (url-hexify-string (nth 0 al)) (url-hexify-string (or (nth 1 al) "2")))
                                                                         #'saaxy-aux-vadio)))
    (name tiny class url function (lambda (a al) (saaxy-async-command (format "http://tinyurl.com/api-create.php?url=%s" (url-hexify-string a)) #'saaxy-aux-tinyurl)))))

(defvar saaxy-error-message "Error occurred.")
(defvar saaxy-context-reset-message "Context reset.")
(defvar saaxy-function-not-defined-message "Function not defined.")
(defvar saaxy-goodies nil)
(defvar saaxy-buffers nil)
(defvar saaxy-e404 (propertize "Not found :'(" 'font-lock-face '(:foreground "#FF0000")))


(defun saaxy-defined-command (command)
  (car (remove-if-not (lambda (x) (equal (plist-get x 'name) (intern command))) saaxy-commands)))

(defvar saaxy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'saaxy-eval-buffer)
    (define-key map (kbd "M-n") (lambda () (interactive) (saaxy-complete-history t)))
    (define-key map (kbd "M-p") 'saaxy-complete-history)
    map))

(defun saaxy-mode ()
  (use-local-map saaxy-map)
  (font-lock-mode 1)

  ;; Initial text
  (insert (saaxy-stickify saaxy-welcome-message))
  (insert (saaxy-stickify (make-string (1- (length saaxy-welcome-message)) ?-)))
  (insert (saaxy-stickify "\n")) (saaxy-add-line)

  ;; Hook to deal with on-the-fly completion
  (add-hook 'after-change-functions (lambda (beg end &rest dunno) (saaxy-handle-typed beg end)) nil t))

(defun saaxy-add-line ()
  (with-current-buffer saaxy-buffer-name
    (goto-char (point-max))
    (setq-local previous-line (cons (point) (1+ (point))))
    (insert (saaxy-stickify (format "\n%s%s" (or saaxy-context "") saaxy-prompt)))))

(defun saaxy-stickify (string)
  (propertize string 'read-only t 'front-sticky t 'rear-nonsticky t))

(defun saaxy-eval-buffer ()
  (interactive)
  (let* ((data (saaxy-current-line-get))
         (history (setq saaxy-history (cons `(type command data ,data) saaxy-history)))
         (result (saaxy-eval data)))
    (saaxy-insert-text (format "%s" result))
    (saaxy-add-line)))

(defun saaxy-insert-text (text)
  (with-current-buffer saaxy-buffer-name
    (insert (saaxy-stickify (format "\n%s" text)))
    (setq saaxy-history-cycle nil)))

;;;;;;;;;;;;;;;;;;;;
;;; Current line ;;;
;;;;;;;;;;;;;;;;;;;;

(defun saaxy-current-line-bounds ()
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (header-len (+ (length saaxy-context) (length saaxy-prompt)))
         (beg (+ beg header-len))
         (len (- end beg)))
    (list beg end len header-len)))

(defun saaxy-current-line-set (string)
  "FIXME multiple lines"
  (let* ((bounds (saaxy-current-line-bounds))
         (beg (nth 0 bounds))
         (end (nth 1 bounds)))
    (delete-region beg end)
    (goto-char beg)
    (insert string)))

(defun saaxy-current-line-get ()
  (let* ((bounds (saaxy-current-line-bounds))
         (beg (nth 0 bounds))
         (end (nth 1 bounds)))
    (buffer-substring beg end)))


(defun saaxy-complete-history (&optional back?)
  (interactive)
  (when (not saaxy-history-cycle)
    (setq saaxy-history-cycle
          (loop for x in saaxy-history when (equal (plist-get x 'type) 'command) collect (plist-get x 'data)
                into stuff and finally return (cons (saaxy-current-line-get) stuff))))
  (let ((new `(,(saaxy-current-line-get) ,@(cdr saaxy-history-cycle))))
    (if back? (setq saaxy-history-cycle (append (last new) (butlast new)))
      (setq saaxy-history-cycle `(,@(cdr new) ,(car new)))))
  (saaxy-current-line-set (car saaxy-history-cycle))
  (message "%s" `((,(car saaxy-history-cycle)) ,@(cdr saaxy-history-cycle))))

(defun saaxy ()
  "The information manager we all love"
  (interactive)
  (let ((exists (get-buffer saaxy-buffer-name))
        (buffer (get-buffer-create saaxy-buffer-name)))
    (with-current-buffer buffer
      (unless exists (saaxy-mode))
      (switch-to-buffer saaxy-buffer-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exciting stuff starts here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun saaxy-from-history (position)
  (plist-get (nth position (remove-if-not (lambda (x) (equal (plist-get x 'type) 'valid-command)) saaxy-history)) 'data))

(defun saaxy-eval (string)
  (if (string= "" string) ""
    (let* ((string (concat string " ")) (chosen (string-to-char string))
           (context-reset (and (> (length string) 1) (string= (substring string 0 2) "@@"))))
      (case chosen
        (?! (let* ((args (split-string string)) (command (car (remove-if-not (lambda (x) (equal (plist-get x 'name) (intern (substring (car args) 1)))) saaxy-commands)))
                   (function (plist-get command 'function)) (arglist (cdr args)) (args (apply #'concat (mapcar (lambda (x) (format "%s " x)) arglist)))
                   (arglist (append arglist '(()))) ;; FIXME: Stupid (apply #'null nil) bug.
                   (args (if (> (length args) 0) (substring args 0 -1) args))
                   (result (if (functionp function) (progn (apply function args (list arglist))) saaxy-function-not-defined-message)))
              (if result (progn (setq saaxy-history (cons `(type valid-command data ,command) saaxy-history)) result) saaxy-error-message)))
        (?# (let* ((s (substring string 1 -1)) (n (string-to-int s)) (goodie (plist-get saaxy-goodies n)))
              (if (not (string= s "")) (eval (plist-get goodie 'call))
                (loop for head = saaxy-goodies then (cddr head) while head collecting
                      (format "%s. %s\n" (car head) (plist-get (cadr head) 'name)) into commands
                      and finally return (apply #'concat commands)))))
        (?@ (if context-reset (progn (setq saaxy-context nil) saaxy-context-reset-message)
              (if (saaxy-defined-command (substring string 1 -1)) (setq saaxy-context (substring string 1 -1)) saaxy-function-not-defined-message)))
        (t (funcall (if saaxy-context (plist-get (car (remove-if-not (lambda (x) (equal (plist-get x 'name) (intern saaxy-context))) saaxy-commands)) 'function) saaxy-noncommand-handler)
                    (substring string 0 -1) (split-string string)))))))

(defun saaxy-eval-process-history (position)
  (let* ((plist (plist-get saaxy-history position))
         (type (plist-get plist 'type))
         (data (plist-get plist 'data)))
    (if plist
        (progn (case type
                 ('text (find-file data))
                 (t (shell-command (run-associated-program data)))) "\n\n")
      "Not on history\n")))

(defun saaxy-handle-typed (beg end)
  (with-current-buffer saaxy-buffer-name
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line)
      (previous-line))))

(provide 'saaxy)
;;; saaxy.el ends here
