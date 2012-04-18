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
(require 'cl)
(require 'doctor)

(defgroup saaxy nil "Sexy services" :tag "saaxy" :group 'web)

(defcustom saaxy-noncommand-handler 
  (lambda (a b) (format "You said: \"%s\". Try !help" a))
  "Function to handle commandless text."
  :group 'saaxy
  :type 'sexp)
(defcustom saaxy-custom-commands '()
  "List of commands you'd like to use on Saaxy. See saaxy-commands on saaxy.el"
  :group 'saaxy
  :type '(repeat sexp))


(defvar saaxy-buffer-name "Saaxy!")
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
    (name ddg class web function (lambda (a al) (saaxy-async-command (format "http://duckduckgo.com/lite/?kp=-1&k1=-1&q=%s" (url-hexify-string a)) #'saaxy-aux-ddg)))
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

(defvar saaxy-context-reset-message "Context reset.")
(defvar saaxy-function-not-defined-message "Function not defined.")
(defvar saaxy-goodies nil)
(defvar saaxy-buffers nil)
(defvar saaxy-error (propertize "Error occurred." 'font-lock-face '(:foreground "#FF0000")))


;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;

(defmacro setq-local (key val)
  "Sets a local function. It's a macro to avoid using quotes too much"
  `(progn (make-local-variable ',key) (set ',key ,val)))

(defun color-for (string)
  "Deterministic way of selecting a color for a string"
  (let* ((colors '("#F08F90" "#F47983" "#DB5A6B" "#C93756" "#FCC9B9" "#FFB3A7"
                   "#F2666C" "#F58F84" "#AC8181" "#B95754" "#C91F37" "#9D2933"
                   "#7B3B3A" "#F7665A" "#B56C60" "#97645A" "#A24F46" "#C3272B"
                   "#8F1D21" "#672422" "#BC2D29" "#5E2824" "#8B352D" "#FA7B62"
                   "#F8674F" "#DC3023" "#AB4C3D" "#934337" "#9D2B22" "#913228"
                   "#6F3028" "#351E1C" "#F35336" "#D34E36" "#CF3A24" "#A13D2D"
                   "#913225" "#752E23" "#F9906F" "#FF7952" "#F07F5E" "#E68364"
                   "#FF4E20" "#E35C38" "#CB6649" "#B35C44" "#B14A30" "#9B533F"
                   "#8C4736" "#60281E" "#542D24" "#4C221B" "#9F7462" "#B64925"
                   "#592B1F" "#351F19" "#F57F4F" "#EC8254" "#9F5233" "#EC956C"
                   "#985538" "#824B35" "#FFA26B" "#FCA474" "#FF8936" "#FA9258"
                   "#FB8136" "#8F583C" "#2E211B" "#AB6134" "#CA6924" "#FFA565"
                   "#D57835" "#C66B27" "#C66B27" "#985629" "#8C5939" "#6A432D"
                   "#593A27" "#C48E69" "#BE7F51" "#7D4E2D" "#B7702D" "#6B4423"
                   "#F7BB7D" "#FFA400" "#FFA631" "#E08A1E" "#CB7E1F" "#C57F2E"
                   "#785E49" "#FFB95A" "#FAA945" "#CE9F6F" "#BB8141" "#FFB61E"
                   "#FFB94E" "#E2BE9F" "#E69B3A" "#E29C45" "#B0927A" "#826B58"
                   "#7F6B5D" "#7F5D3B" "#665343" "#4C3D30" "#A17917" "#896C39"
                   "#5C4827" "#E3B130" "#E2B13C" "#F3C13A" "#D3B17D" "#AA8736"
                   "#957B38" "#D9B611" "#645530" "#BDA928" "#BBA46D" "#9C8A4D"
                   "#534A32" "#473F2D" "#8B7D3A" "#524B2A" "#3B3429" "#857C55"
                   "#5E5545" "#7A942E" "#4D4B3A" "#BCB58C" "#8DB255" "#8C9E5E"
                   "#5B8930" "#52593B" "#454D32" "#8C9C76" "#6B9362" "#817B69"
                   "#5E644F" "#374231" "#2A603B" "#A5BA93" "#898A74" "#407A52"
                   "#3D5D42" "#3D4035" "#006442" "#656255" "#224634" "#2D4436"
                   "#2E372E" "#5A6457" "#749F8D" "#819C8B" "#3A6960" "#3A403B"
                   "#2B3733" "#354E4B" "#203838" "#757D75" "#4F4944" "#2B3736"
                   "#86ABA5" "#6A7F7A" "#C6C2B6" "#48929B" "#006C7F" "#455859"
                   "#5C544E" "#264348" "#364141" "#1D697C" "#317589" "#4D646C"
                   "#044F67" "#344D56" "#3D4C51" "#4D8FAC" "#252321" "#5D8CAE"
                   "#192236" "#181B26" "#1F4788" "#003171" "#1B294B" "#78779B"
                   "#191F45" "#766980" "#5A4F74" "#89729E" "#614E6E" "#875F9A"
                   "#5D3F6A" "#976E9A" "#3F313A" "#2B2028" "#3A243B" "#A87CA0"
                   "#8D608C" "#5B3256" "#4F284B" "#23191E" "#763568" "#BB7796"
                   "#491E3C" "#755D5B" "#63424B" "#6D2B50" "#4D3B3C" "#A4345D"
                   "#8F4155" "#43242A" "#512C31" "#7E2639" "#59292C" "#44312E"
                   "#FFDDCA" "#B9A193" "#97867C" "#6E5F57" "#4B3C39" "#393432"
                   "#352925" "#27221F" "#171412" "#ebf6f7"))
         (len (length colors))
         (hash (string-to-number (substring (md5 (if (stringp string) string "")) 0 6) 16)))
    (nth (mod hash len) colors)))

;;;;;;;;;;;;;;;
;; Structure ;;
;;;;;;;;;;;;;;;

(defun saaxy-defined-command (command)
  (car (remove-if-not (lambda (x) (equal (plist-get x 'name) (intern command))) (append saaxy-commands saaxy-custom-commands))))

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
        (?! (let* ((args (split-string string)) 
		   (command (car (remove-if-not (lambda (x) (equal (plist-get x 'name) (intern (substring (car args) 1)))) (append saaxy-commands saaxy-custom-commands))))
                   (function (plist-get command 'function)) (arglist (cdr args)) (args (apply #'concat (mapcar (lambda (x) (format "%s " x)) arglist)))
                   (arglist (append arglist '(()))) ;; FIXME: Stupid (apply #'null nil) bug.
                   (args (if (> (length args) 0) (substring args 0 -1) args))
                   (result (if (functionp function) (progn (apply function args (list arglist))) saaxy-function-not-defined-message)))
              (if result (progn (setq saaxy-history (cons `(type valid-command data ,command) saaxy-history)) result) saaxy-error)))
        (?# (let* ((s (substring string 1 -1)) (n (string-to-int s)) (goodie (plist-get saaxy-goodies n)))
              (if (not (string= s "")) (eval (plist-get goodie 'call))
                (loop for head = saaxy-goodies then (cddr head) while head collecting
                      (format "%s. %s\n" (car head) (plist-get (cadr head) 'name)) into commands
                      and finally return (apply #'concat commands)))))
        (?@ (if context-reset (progn (setq saaxy-context nil) saaxy-context-reset-message)
              (if (saaxy-defined-command (substring string 1 -1)) (setq saaxy-context (substring string 1 -1)) saaxy-function-not-defined-message)))
        (t (funcall (if saaxy-context (plist-get (car (remove-if-not (lambda (x) (equal (plist-get x 'name) (intern saaxy-context))) 
								     (append saaxy-commands saaxy-custom-commands))) 'function) saaxy-noncommand-handler)
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
