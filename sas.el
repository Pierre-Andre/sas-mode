;;; sas.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 1997-2021 Free Software Foundation, Inc.
;;
;; Author: Pierre-André Cornillon <https://github.com/pac>
;; Author: Fabián E. Gallina <fgallina@gnu.org>
;; Author: A.J. Rossini
;; Author: Rodney A. Sparapani
;; Author: Richard M. Heiberger <rmh@temple.edu>
;; Maintainer: Pierre-André Cornillon <pierre-andre.cornillon@univ-rennes2.fr>
;; Created: april 28, 2021
;; Modified: 2022-03-29
;; Version: 0.6.0
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This is based upon Version 1.4 of SAS mode:
;;;    sas-mode:  indent, run etc, SAS programs.
;;;  Author:   Tom Cook
;;;            Dept. of Biostatistics
;;;            University of Wisconsin - Madison
;;;            Madison, WI 53706
;;;            cook@biostat.wisc.edu
;;;
;;;  Acknowledgements for ESS SAS-mode
;;;  Menu code for XEmacs/Lucid emacs and startup mods
;;;  contributed by arossini@biostats.hmc.psu.edu
;;;
;;; Last change: 2/1/95
;;; Last change: 01/15/02
;;
;;  Description
;;
;;; Code:
(require 'comint)
(require 'tramp-sh)

(defcustom sas-shell-interpreter "sas"
  "Default Sas interpreter for shell."
  :type 'string
  :group 'sas)
(defcustom sas-log-separated 't
  "If nil SAS buffer will contains LOG and Output."
  :type 'string
  :group 'sas)
(defcustom sas-shell-interpreter-args "-nodms -nonews -stdio -nofullstimer -nodate -nocenter -terminal -pagesize max -nosyntaxcheck -cleanup"
  "Default arguments for the Sas interpreter to make a real session using comint.
\"-nodms -stdio\" is the important part."
  :type 'string
  :group 'sas)
(defcustom sas-shell-command-interpreter-args "-nodms -nonews -nofullstimer -nodate -nocenter -terminal -pagesize max -nosyntaxcheck"
  "Default arguments for the Sas interpreter."
  :type 'string
  :group 'sas)
(defcustom sas-shell-command-interpreter-args-windows "-NOTERMINAL NOSPLASH -NOSTATUSWIN -NOICON "
  "Default arguments for the Sas interpreter."
  :type 'string
  :group 'sas)

(defcustom sas-shell-buffer-name "Sas"
  "Default buffer name for Sas interpreter."
  :type 'string
  :group 'sas
  :safe 'stringp)

;; The next two are ``the inside of [...] in a regexp'' to be used in
;; (skip-chars-(for|back)ward SAS-..-chars)
(defcustom sas-white-chars " \t\n\f"
  "This does NOT escape blanks (RMH, 2000/03/20)."
  :group 'sas
  :type  'string)

(defcustom sas-delay-kill-buffer 800
  "Delay in millisecs between endsas; and killing windows"
  :group 'sas
  :type  'integer)
(defcustom sas-verbose nil
  "If not nil message in minibuffer"
  :group 'sas
  :type  'boolean)

(defcustom sas-user-library nil
  "Name of SAS user library, if none no user library is created, if nil a temp dir is created, else an existing dir is used"
  :group 'sas
  :type  'string)
(defcustom sas-realsession t
  "If nil a fake session is used, else a comint buffer is created"
  :group 'sas
  :type  'string)
(defcustom sas-sas-windows nil
  "If nil in a fake session windows shell syntax is used to execute sas program, else unix/linux syntax is used"
  :group 'sas
  :type  'string)
(defcustom sas-view-maxnumber-of-rows 3
  "Maximum number of rows displayed in `sas-view-table'"
  :group 'sas
  :type  'integer)

(defun make-comint-in-buffer-std (name buffer program &optional startcommand stderr &rest switches)
"Make a Comint process NAME in BUFFER, running PROGRAM.
If BUFFER is nil, it defaults to NAME surrounded by `*'s.
If there is a running process in BUFFER, it is not restarted.

PROGRAM should be one of the following:
- a string, denoting an executable program to create via
  `start-file-process'
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to be opened via `open-network-stream'
- nil, denoting a newly-allocated pty.

Optional fourth arg STARTCOMMAND is string whose
contents are sent to the process as its initial input.
Optional fifth arg STDERR is a buffer for standard error.
SWITCHES are PROGRAM switches.

If PROGRAM is a string, any more args are arguments to PROGRAM.

Return the (possibly newly created) process buffer."
  (or (fboundp 'start-file-process)
      (error "Multi-processing is not supported for this system"))
  (setq buffer (get-buffer-create (or buffer (concat "*" name "*"))))
  ;; If no process, or nuked process, crank up a new one and put buffer in
  ;; comint mode.  Otherwise, leave buffer and existing process alone.
  (unless (comint-check-proc buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'comint-mode)
        (comint-mode))) ; Install local vars, mode, keymap, ...
    (comint-exec-std buffer name program startcommand stderr switches))
  buffer)

(defun comint-exec-std (buffer name command startcommand stderr switches)
"Start up a process named NAME in buffer BUFFER for Comint modes.
Run the given COMMAND with SWITCHES, initial input
from STARTCOMMAND and standard error from STDERR.

COMMAND should be one of the following:
- a string, denoting an executable program to create via
  `start-file-process'
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to be opened via `open-network-stream'
- nil, denoting a newly-allocated pty.

This function blasts any old process running in the buffer, and
does not set the buffer mode.  You can use this to cheaply run a
series of processes in the same Comint buffer.  The hook
`comint-exec-hook' is run after each exec."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc
           (if (consp command)
               (open-network-stream name buffer (car command) (cdr command))
             (comint-exec-1-std name buffer command stderr switches))))
      (set-process-filter proc 'comint-output-filter)
      (setq-local comint-ptyp process-connection-type) ; t if pty, nil if pipe.
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (cond (startcommand
        (sleep-for 1)
	     (goto-char (point-max))
          (comint-send-string proc startcommand)))
      (run-hooks 'comint-exec-hook)
      buffer)))

(defun comint-exec-1-std (name buffer command stderr switches)
"Same function as `comint-exec-1' but with STDERR argument.
  STDERR is a buffer that will be used as standard error of process (see `make-process')"
  (let ((process-environment
         (nconc
          (comint-term-environment)
          (list (format "INSIDE_EMACS=%s,comint" emacs-version))
          process-environment))
        (default-directory
          (if (file-accessible-directory-p default-directory)
              default-directory
            "/"))
        proc decoding encoding changed)
    (let ((exec-path (if (and command (file-name-directory command))
                         ;; If the command has slashes, make sure we
                         ;; first look relative to the current directory.
                         (cons default-directory exec-path) exec-path)))
      (setq proc (apply 'start-file-process-std name buffer command stderr switches)))
    ;; Some file name handler cannot start a process, fe ange-ftp.
    (unless (processp proc) (error "No process started"))
    (let ((coding-systems (process-coding-system proc)))
      (setq decoding (car coding-systems)
            encoding (cdr coding-systems)))
    ;; Even if start-file-process left the coding system for encoding data
    ;; sent from the process undecided, we had better use the same one
    ;; as what we use for decoding.  But, we should suppress EOL
    ;; conversion.
    (if (and decoding (not encoding))
        (setq encoding (coding-system-change-eol-conversion decoding 'unix)
              changed t))
    (if changed
        (set-process-coding-system proc decoding encoding))
    proc))

(defun start-file-process-std (name buffer program stderr &rest program-args)
"Start a program in a subprocess.  Return the process object for it.

Similar to `start-process', but may invoke a file name handler based on
`default-directory'.  See Info node `(elisp)Magic File Names'.

This handler ought to run PROGRAM, perhaps on the local host,
perhaps on a remote host that corresponds to `default-directory'.
In the latter case, the local part of `default-directory', the one
produced from it by `file-local-name', becomes the working directory
of the process on the remote host.

PROGRAM and PROGRAM-ARGS might be file names.  They are not
objects of file name handler invocation, so they need to be obtained
by calling `file-local-name', in case they are remote file names.

STDERR is a buffer which will be used as standard error of process (see `make-process')

File name handlers might not support pty association, if PROGRAM is nil."
  (let ((fh (find-file-name-handler default-directory 'start-file-process-std)))
    (if fh (apply fh 'start-file-process-std name buffer program stderr program-args)
      (apply 'start-process-std name buffer program stderr program-args))))

(defun start-process-std (name buffer program stderr &rest program-args)
"Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.

Process output (both standard output and standard error streams)
goes at end of BUFFER, unless you specify a filter function to
handle the output.  BUFFER may also be nil, meaning that this
process is not associated with any buffer.

PROGRAM is the program file name.  It is searched for in `exec-path'
\(which see).  If nil, just associate a pty with the buffer.  Remaining
arguments PROGRAM-ARGS are either strings to give program as arguments or
a plist (:stderr \"*buffer name of stderr*\" :switches (\"-l\" \"-a\"))

STDERR is a buffer for separate standard error from standard output:
if nil standard error is in BUFFER ; if it is a buffer this will receive standard error

The process runs in `default-directory' if that is local (as
determined by `unhandled-file-name-directory'), or \"~\"
otherwise.  If you want to run a process in a remote directory
use `start-file-process'."
  (unless (fboundp 'make-process)
    (error "Emacs was compiled without subprocess support"))
  (apply #'make-process
         (append (list :name name :buffer buffer)
                 (if program
                     (if stderr
                         (list :command (cons program program-args)
                               :stderr stderr)
                       (list :command (cons program program-args)))
                   )))  )

(defun sas--clean-eol (string)
  "Function to remove page-break."
  (replace-regexp-in-string "\014" "\n" string))

(defun run-sas (&optional cmd dedicated show)
"Run an inferior Sas process.

Argument CMD defaults to `sas-shell-calculate-command' return
value.  When called interactively with `prefix-arg', it allows
the user to edit such value and choose whether the interpreter
should be DEDICATED for the current buffer.  When numeric prefix
arg is other than 0 or 4 do not SHOW.

For a given buffer and same values of DEDICATED, if a process is
already running for it, it will do nothing.  This means that if
the current buffer is using a global process, the user is still
able to switch it to use a dedicated one.

Runs the hook `inferior-sas-mode-hook' after
`comint-mode-hook' is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run Sas: " (sas-shell-calculate-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (sas-shell-calculate-command) nil t)))
  (if sas-realsession
  (let ((buffer
         (sas-shell-make-comint
          (or cmd (sas-shell-calculate-command))
          (sas-shell-get-process-name dedicated) dedicated show)))
    (pop-to-buffer buffer)
    (get-buffer-process buffer))
  (sas-make-fakesession 't sas-user-library)))

(defun sas-shell-calculate-command ()
"Calculate the string used to execute the inferior Sas process."
  (format "%s %s"
          ;; `sas-shell-make-comint' expects to be able to
          ;; `split-string-and-unquote' the result of this function.
          (combine-and-quote-strings (list sas-shell-interpreter))
          sas-shell-interpreter-args))

(defmacro sas-shell-with-environment (&rest body)
"Modify shell environment during execution of BODY.
Temporarily sets `process-environment' and `exec-path' during
execution of body.  If `default-directory' points to a remote
machine then modifies `tramp-remote-process-environment' and
`sas-shell-remote-exec-path' instead."
  (declare (indent 0) (debug (body)))
  (let ((vec (make-symbol "vec")))
    `(progn
       (let* ((,vec
               (when (file-remote-p default-directory)
                 (ignore-errors
                   (tramp-dissect-file-name default-directory 'noexpand))))
              (process-environment
               (if ,vec
                   process-environment
                 (sas-shell-calculate-process-environment)))
              (exec-path
               (if ,vec
                   exec-path
                 (sas-shell-calculate-exec-path)))
              (tramp-remote-process-environment
               (if ,vec
                   (sas-shell-calculate-process-environment)
                 tramp-remote-process-environment)))
         (when (tramp-get-connection-process ,vec)
           ;; For already existing connections, the new exec path must
           ;; be re-set, otherwise it won't take effect.  One example
           ;; of such case is when remote dir-locals are read and
           ;; *then* subprocesses are triggered within the same
           ;; connection.
           (sas-shell-tramp-refresh-remote-path
            ,vec (sas-shell-calculate-exec-path))
           ;; The `tramp-remote-process-environment' variable is only
           ;; effective when the started process is an interactive
           ;; shell, otherwise (like in the case of processes started
           ;; with `process-file') the environment is not changed.
           ;; This makes environment modifications effective
           ;; unconditionally.
           (sas-shell-tramp-refresh-process-environment
            ,vec tramp-remote-process-environment))
         ,(macroexp-progn body)))))
(defmacro sas-shell--add-to-path-with-priority (pathvar paths)
"Modify PATHVAR and ensure PATHS are added only once at beginning."
  `(dolist (path (reverse ,paths))
     (cl-delete path ,pathvar :test #'string=)
     (cl-pushnew path ,pathvar :test #'string=)))

(defun sas-shell-get-process-name (dedicated)
"Calculate the appropriate process name for inferior Sas process.
If DEDICATED is t returns a string with the form
`sas-shell-buffer-name'[`buffer-name'] else returns the value
of `sas-shell-buffer-name'."
  (if dedicated
      (format "%s[%s]" sas-shell-buffer-name (buffer-name))
    sas-shell-buffer-name))
(defun sas-shell-get-errorbuffer-name (dedicated)
"Calculate the appropriate  name for error bufffer .
If DEDICATED is t returns a string with the form
Log`sas-shell-buffer-name'[`buffer-name'] else returns the value
of `sas-shell-buffer-name'."
  (if dedicated
      (format "Log-%s[%s]" sas-shell-buffer-name (buffer-name))
   (format "Log-%s"  sas-shell-buffer-name)))

(defun sas-shell-command-get-process-name (dedicated)
"Calculate the appropriate process name for inferior Sas process.
If DEDICATED is t returns a string with the form
`sas-shell-buffer-name_buffer-name' else returns the value
of `sas-shell-buffer-name'."
  (if dedicated
      (format "%s_%s" sas-shell-buffer-name (buffer-name))
    sas-shell-buffer-name))
(defun sas-shell-command-get-errorbuffer-name (dedicated)
"Calculate the appropriate  name for error bufffer .
If DEDICATED is t returns a string with the form
Log`sas-shell-buffer-name_buffer-name' else returns the value
of `sas-shell-buffer-name'."
  (if dedicated
      (format "Log-%s_%s" sas-shell-buffer-name (buffer-name))
   (format "Log-%s"  sas-shell-buffer-name)))

(defun sas-shell-make-comint (cmd proc-name &optional dedicated  show internal)
"Create a Sas shell comint buffer.
CMD is the Sas command to be executed and PROC-NAME is the
process name the comint buffer will get.  After the comint buffer
is created the `inferior-sas-mode' is activated. When
optional argument DEDICATED is non-nil it controls if the
 stderr buffer is dedicated. When
optional argument SHOW is non-nil the buffer is shown.  When
optional argument INTERNAL is non-nil this process is run on a
buffer with a name that starts with a space, following the Emacs
convention for temporary/internal buffers, and also makes sure
the user is not queried for confirmation when the process is
killed."
  (save-excursion
    (sas-shell-with-environment
     (let* ((proc-buffer-name
             (format (if (not internal) "*%s*" " *%s*") proc-name)))
       (when (not (comint-check-proc proc-buffer-name))
         (let* ((cmdlist (split-string-and-unquote cmd))
                (interpreter (car cmdlist))
                (args (cdr cmdlist))
                (bufstderr (if sas-log-separated
                             (get-buffer-create (sas-shell-get-errorbuffer-name dedicated))))
                (buffer (if sas-log-separated
                            (apply #'make-comint-in-buffer-std
                                   proc-name proc-buffer-name
                                   interpreter nil bufstderr args)
                          (apply #'make-comint-in-buffer
                                   proc-name proc-buffer-name
                                   interpreter nil args)))
                (sas-shell--parent-buffer (current-buffer))
                (process (get-buffer-process buffer))
                ;; Users can override the interpreter and args
                ;; interactively when calling `run-sas', let-binding
                ;; these allows having the new right values in all
                ;; setup code that is done in `inferior-sas-mode',
                ;; which is important, especially for prompt detection.
                (sas-shell--interpreter interpreter)
                (sas-shell--interpreter-args
                 (mapconcat #'identity args " ")))
           (if sas-log-separated (with-current-buffer bufstderr
             (inferior-sas-mode)))
           (with-current-buffer buffer
             (inferior-sas-mode))
            (when show (display-buffer buffer))
           (and internal (set-process-query-on-exit-flag process nil))))
       proc-buffer-name))))

(defun sas-shell-calculate-process-environment ()
"Calculate `process-environment' or `tramp-remote-process-environment'.
  If `default-directory' points to a remote host, the returned value is intended for `tramp-remote-process-environment'."
  (let* ((remote-p (file-remote-p default-directory))
         (process-environment (if remote-p
                                  tramp-remote-process-environment
                                process-environment)))
    process-environment))

(defun sas-shell-calculate-exec-path ()
"Calculate `exec-path'.
Prepends `sas-shell-exec-path'.  If `default-directory' points
to a remote host, the returned value appends
`sas-shell-remote-exec-path' instead of `exec-path'."
  (let ((new-path (copy-sequence
                   (if (file-remote-p default-directory)
                       sas-shell-remote-exec-path
                     exec-path))))
    (sas-shell--add-to-path-with-priority
     new-path sas-shell-exec-path)
    new-path))

(defcustom sas-shell-remote-exec-path nil
"List of paths to be ensured remotely for searching executables.
When this variable is non-nil, values are exported into remote
hosts PATH before starting processes.  Values defined in
`sas-shell-exec-path' will take precedence to paths defined
here.  Normally you wont use this variable directly unless you
plan to ensure a particular set of paths to all Sas shell
executed through tramp connections."
  :version "25.1"
  :type '(repeat string)
  :group 'sas)
(defcustom sas-shell-exec-path nil
"List of paths for searching executables.
When this variable is non-nil, values added at the beginning of
the PATH before starting processes.  Any values present here that
already exists in PATH are moved to the beginning of the list so
that they are prioritized when looking for executables."
  :type '(repeat string)
  :group 'sas)

(defun sas-make-fakesession  (&optional dedicated sas-user-library)
  "Create results and log buffer and if needed create user library"
;  (make-local-variable 'sas-file-progsas)
  (make-local-variable 'sas-buffer-user-library)
  (setq sas-buffer-user-library
        (if sas-user-library
            (if (not (string= sas-user-library "none"))
                (if (file-directory-p sas-user-library)
                    sas-user-library
                  (user-error "directory %s does not exist"
                              sas-user-library))
              sas-user-library)
          (make-temp-file "saslib" t)))
  (let ((buffer-sas (current-buffer)))
    ;(setq sas-file-progsas (buffer-name (create-file-buffer ".sas_temp_progsas.sas")))
    (get-buffer-create (sas-shell-command-get-errorbuffer-name dedicated))
    (get-buffer-create (sas-shell-command-get-process-name dedicated))
    (if (not (string= (buffer-name) (buffer-name buffer-sas)))
        (switch-to-buffer buffer-sas))))

(defvar sas-prompt-regexp "^"
"Prompt for `run-sas'.")
(defun sas--initialize ()
  "Helper function to initialize Sas"
  (setq comint-process-echoes t)
  (add-hook 'comint-preoutput-filter-functions 'sas--clean-eol 0 'make-it-local)
  (setq comint-use-prompt-regexp t))

(define-derived-mode inferior-sas-mode comint-mode "Inferior sas"
 "Major mode for sas inferior process`run-sas'."
  nil "sas"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp sas-prompt-regexp)
  (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(sas-mode-font-lock-defaults nil t)))
(add-hook 'inferior-sas-mode-hook 'sas--initialize)

(defvar sas-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r"   #'sas-shell-send-region)
    (define-key map "\C-c\C-b"   #'sas-shell-send-buffer)
    (define-key map "\C-c\C-j"   #'sas-shell-send-line)
    (define-key map [(control return)] #'sas-shell-send-dwim)
    (define-key map "\C-c\C-v"   #'sas-view-table)
    (define-key map "\C-c\C-q"   #'sas-exit)
   map)
  "Keymap for `sas-mode'.")

(defun sas-string-finish-with-ret (s)
  "Add to S \n if needed."
  (let ((start-pos (- (length s) 1)))
    (and (>= start-pos 0)
         (if (eq t (compare-strings "\n" nil nil
                                s start-pos nil t))
             s
           (concat s "\n")))))
(defun sas-shell-send-string (string &optional process msg)
"Send STRING to inferior Sas PROCESS or via shell-command.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Sas command: ") nil t))
  (if sas-realsession
  (let ((process (or process (sas-shell-get-process-or-error msg))))
      (comint-send-string process (sas-string-finish-with-ret string))
      (when (not (string-match ".*\n[:blank:]*" string))
        (comint-send-string process "\n")))
  (sas-send-string-with-shell-command string sas-buffer-user-library)))

(defun sas-shell-send-region (start end &optional  msg)
"Send the region delimited by START and END to inferior Sas process.
When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive
   (list (region-beginning) (region-end) t))
  (if sas-realsession
  (let* ((string (buffer-substring-no-properties start end))
         (process (sas-shell-get-process-or-error msg))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (when sas-verbose (message "Sent: %s..." (match-string 1 string)))
    ;; Recalculate positions to avoid landing on the wrong line if
    ;; lines have been removed/added.
    ;; (with-current-buffer (process-buffer process)
    ;;  (compilation-forget-errors))
    (sas-shell-send-string string process)
    (deactivate-mark))
  (let ((string (buffer-substring-no-properties start end)))
     (when sas-verbose (message "Sent: %s..." (match-string 1 string)))
      (sas-send-string-with-shell-command string sas-buffer-user-library))))

(defun sas-shell-send-line (&optional  msg)
"Send the current line to the inferior SAS process.
 When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
 (interactive (list t))
  (if sas-realsession
  (let* ((start (point-at-bol))
         (end (point-at-eol))
         (string (buffer-substring-no-properties start end))
         (process (sas-shell-get-process-or-error msg))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (when sas-verbose (message "Sent: %s..." (match-string 1 string)))
    ;; Recalculate positions to avoid landing on the wrong line if
    ;; lines have been removed/added.
    ;; (with-current-buffer (process-buffer process)
    ;;  (compilation-forget-errors))
    (sas-shell-send-string string process)
    (deactivate-mark))
  (let* ((start (point-at-bol))
         (end (point-at-eol))
         (string (buffer-substring-no-properties start end)))
    (when sas-verbose (message "Sent: %s..." (match-string 1 string)))
      (sas-send-string-with-shell-command string sas-buffer-user-library))))

(defun sas-shell-send-buffer (&optional msg)
"Send the entire buffer to inferior Sas process.
When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive (list t))
  (save-restriction
    (widen)
    (sas-shell-send-region (point-min) (point-max)  msg)))

(defun sas-shell-send-file (file-name &optional process msg)
"Send FILE-NAME to inferior Sas PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running;
defaults to t when called interactively."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    t))                                 ; msg
  (if sas-realsession
  (let* ((process (or process (sas-shell-get-process-or-error msg)))
         (file-name (file-local-name (expand-file-name file-name)))
         (string (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string))))
    (sas-shell-send-string string process t))
  (let* ((file-name (file-local-name (expand-file-name file-name)))
         (string (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string))))
    (sas-send-string-with-shell-command string sas-buffer-user-library))))

(defun sas-shell-send-exit (&optional process)
"Send \"endsas;\" to the Sas PROCESS."
  (interactive (list nil))
  (if sas-realsession
   (let* ((process (or process (sas-shell-get-process-or-error))))
    (sas-shell-send-string "endsas;\n" process))))

(defun sas-exit ()
"Send exit to Sas PROCESS, and close buffer."
  (interactive)
  (if sas-realsession
  (let* ((process (sas-shell-get-process-or-error))
         (name-buffer-sas (buffer-name (process-buffer process)))
         (name-buffer-saslog (concat "Log-" (substring name-buffer-sas 1 -1))))
    (sas-shell-send-exit process)
    ;; sits for a clean exit of Sas process
    (sleep-for 0 sas-delay-kill-buffer)
    ;; kill buffer
    (if sas-log-separated
        (kill-buffer name-buffer-saslog))
    (kill-buffer name-buffer-sas))
  (progn
    (kill-buffer sas-file-progsas)
    (kill-buffer (sas-shell-command-get-errorbuffer-name 't))
    (kill-buffer  (sas-shell-command-get-process-name 't))
    (delete-file sas-file-progsas)
    (delete-file (sas-shell-command-get-errorbuffer-name 't))
    (delete-file (sas-shell-command-get-process-name 't)))))

(defun sas-shell-send-dwim ()
"Send the region if selected if not try to send the block
proc/run or data/run."
  (interactive)
  (if (use-region-p)
      (sas-shell-send-region (region-beginning) (region-end) t)
    (let (begpos endpos nameproc)
      (save-excursion
        (setq nameproc (sas-beginning-of-sas-proc))
        (setq begpos (point))
        (when sas-verbose (message "begpos %s" begpos)))
      (if (and nameproc (string-equal (downcase nameproc) "iml"))
          (sas-shell-send-line t)
          (progn
            (save-excursion
              (sas-end-of-sas-proc t nil)
              (setq endpos (point))
              (when sas-verbose (message "endpos %s" endpos)))
            (sas-shell-send-region begpos endpos t))))))

(defun sas-shell-get-process-or-error (&optional interactivep)
"Return inferior Sas process for current buffer or signal error.
When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
  (or (sas-shell-get-process)
      (if interactivep
          (run-sas)
          ;; (user-error
          ;;  "Start a Sas process first with `M-x run-sas' or `%s'."
          ;;  ;; Get the binding.
          ;;  (key-description
          ;;   (where-is-internal
          ;;    #'run-sas overriding-local-map t)))
        (error
         "No inferior Sas process running."))))
(defun sas-shell-get-process ()
 "Return inferior Sas process for current buffer."
  (get-buffer-process (sas-shell-get-buffer)))

(defun sas-shell-get-buffer ()
"Return inferior Sas buffer for current buffer.
If current buffer is in `inferior-sas-mode', return it."
  (if (derived-mode-p 'inferior-sas-mode)
      (current-buffer)
    (let* ((dedicated-proc-name (sas-shell-get-process-name t))
           (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
           (global-proc-name  (sas-shell-get-process-name nil))
           (global-proc-buffer-name (format "*%s*" global-proc-name))
           (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
           (global-running (comint-check-proc global-proc-buffer-name)))
      ;; Always prefer dedicated
      (or (and dedicated-running dedicated-proc-buffer-name)
          (and global-running global-proc-buffer-name)))))

(eval-and-compile
  (defun sas-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (''comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (''string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (''paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))
(defun sas-syntax-context (type &optional syntax-ppss)
"Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro sas-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      ('comment (and (nth 4 ppss) (nth 8 ppss)))
      ('string (and (nth 3 ppss) (nth 8 ppss)))
      ('paren (nth 1 ppss))
      (_ nil))))

(defun sas-beginning-of-sas-statement ()
"Move point to beginning of current sas statement."
  (interactive)
  (if (re-search-backward ";[ \n\t]*" (point-min) t)
      (if (sas-syntax-context 'comment)
          (sas-beginning-of-sas-statement)
        (progn
          (if (looking-at ";\n")
              (forward-char 2)
            (forward-char 1))
          (skip-chars-forward sas-white-chars)))
    (goto-char (point-min))))

(defun sas-end-of-sas-statement ()
"Move point to beginning of current sas statement."
  (interactive)
  (if (search-forward ";" nil t)
      (if (sas-syntax-context 'comment)
          (sas-end-of-sas-statement))
    (goto-char (point-max))))

(defun sas-beginning-of-sas-proc (&optional redo)
"Move point to the beginning of sas proc, macro or data step.
Optional argument REDO (when non-nil) allows
to skip the first displacement to the end of statement."
  (interactive)
  (if (not redo)
      (sas-end-of-sas-statement))
  (let (nameproc (case-fold-search t))
(if (re-search-backward "[ \t\n]+proc[ \t\n]\\|[ \t\n]+data[ \t\n]+\\|[ \t\n]+%macro[ \t\n]*" (point-min) t)
    (if (sas-syntax-context 'comment)
        (sas-beginning-of-sas-proc t))
  (goto-char (point-min)))
(if (looking-at "[ \t\n]+proc[ \t\n]+\\([A-Za-z]+\\)")
        (setq nameproc (match-string 1)))
      (skip-chars-forward sas-white-chars)
    (concat nameproc "")))

(defun sas-end-of-sas-proc (&optional plusone redo)
"Move point to end of sas proc, macro or data step.
If PLUSONE is non-nil point is moved forward of one char.
Optional argument REDO (when non-nil) allows
to skip the first displacement to the end of statement."
  (interactive (list t nil))
  (if (not redo)
      (progn
        (sas-beginning-of-sas-statement)
        (forward-char -1)))
  (let ((case-fold-search t))
    (if (re-search-forward "[ \t\n]+run[ \t\n]*;\\|%mend[ \t\n]+[a-z_0-9]+[ \t\n]*;\\|%mend[ \t\n]*;" (point-max) t)
        (if (sas-syntax-context 'comment)
            (sas-end-of-sas-proc nil t)
          (if plusone
              (forward-char 1)))
      (goto-char (point-max)))))

(defun sas-next-sas-proc (arg)
"Move point to beginning of next sas proc, macro or data step.
The optional argument ARG is a number that indicates the
  search direction and the number of occurrences to search for.  If it
  is positive, search forward for COUNT successive occurrences; if it
  is negative, search backward, instead of forward, for -COUNT
  occurrences.  A value of nil means the same as 1."
  (interactive "P")
  (let ((case-fold-search t))
    (forward-char 1)
    (if (re-search-forward
         "^[ \t]*\\(data[ ;]\\|proc[ ;]\\|endsas[ ;]\\|g?options[ ;]\\|%macro[ ;]\\)"
         nil t arg)
      (if (sas-syntax-context 'comment)  (sas-next-sas-proc))
        (sas-beginning-of-sas-statement 1)
      (forward-char -1))))

(defun sas-external-shell-command (session file-progsas file-result file-log)
  "return string: the sas command to be run.
   IF SESSION is not 'none' a personnal sas library is used"
    (if sas-sas-windows
        (if (string= session "none")
            (format "%s -SYSIN %s %s -PRINT %s -LOG %s"
                    sas-shell-interpreter
                    file-progsas
                    sas-shell-command-interpreter-args-windows
                    file-result
                    file-log)
          (format "%s -USER %s -SYSIN %s %s -PRINT %s -LOG %s"
                    sas-shell-interpreter
                    sas-buffer-user-library
                    file-progsas
                    sas-shell-command-interpreter-args-windows
                    file-result
                    file-log))
      (if (string= session "none")
          (format "%s %s -log %s -print %s %s"
                    sas-shell-interpreter
                    sas-shell-command-interpreter-args
                    file-log
                    file-result
                    file-progsas)
        (format "%s -user %s %s -log \"%s\" -print \"%s\" %s"
                    sas-shell-interpreter
                    sas-buffer-user-library
                    sas-shell-command-interpreter-args
                    file-log
                    file-result
                    file-progsas))))
(defun sas-send-string-with-shell-command (string session)
  (let* ((name-buffer-sas (buffer-name (current-buffer)))
        (file-progsas (make-temp-file "SAS"))
        (buffer-progsas  (find-file-noselect file-progsas))
        (file-result (sas-shell-command-get-process-name 't))
        (file-log (sas-shell-command-get-errorbuffer-name 't)))
    (with-current-buffer file-result
      (set-visited-file-name file-result)
      (erase-buffer)
      (save-buffer 0))
    (with-current-buffer file-log
      (set-visited-file-name file-log)
      (erase-buffer)
      (save-buffer 0))
    (with-current-buffer buffer-progsas
      (set-visited-file-name file-progsas)
      (erase-buffer)
      (insert string)
      (save-buffer 0))
    (shell-command
     (sas-external-shell-command session file-progsas file-result file-log)
     nil nil)
    (if (buffer-live-p buffer-progsas) (kill-buffer buffer-progsas))
    (if (file-exists-p file-progsas) (delete-file file-progsas))
    (if (not (string= (buffer-name) name-buffer-sas))
        (switch-to-buffer name-buffer-sas))))

(defun sas--get-point-symbol ()
  "Get symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (version< emacs-version "24.4")
        (thing-at-point 'symbol)
      (thing-at-point 'symbol t))))

(defun sas-fsview-table  (&optional table edit)
  "Open a proc fsview on TABLE or region or point.
If EDIT is not nil fsview in edit mode else browseonly "
   (interactive
   (list (if current-prefix-arg
        (read-from-minibuffer
         "SAS Table: ") nil)
         nil))
   (let* ((lookfor-table (or table (sas--get-point-symbol)))
          (sas-command (concat "proc fsview"
                               (unless edit
                                 " BROWSEONLY")
                               " data=" lookfor-table "; run;")))
    (if sas-realsession
        (let ((process (sas-shell-get-process-or-error nil)))
          (when sas-verbose (message "Sent: %s" sas-command))
          (sas-shell-send-string sas-command process))
      (progn
        (when sas-verbose (message "Sent: %s" sas-command))
        (sas-send-string-with-shell-command sas-command sas-buffer-user-library)))))

(defun sas-fsview-edit-table  (&optional table)
  "Open a proc fsview on TABLE or region or point."
  (interactive
   (list
    (if current-prefix-arg
        (read-from-minibuffer "SAS Table: ")
      nil)))
  (let ((lookfor-table (or table (sas--get-point-symbol))))
    (sas-fsview-table lookfor-table 't)))

(defun sas-view-table  (&optional table)
  "launch a proc print on TABLE or region or point."
   (interactive
   (list (if current-prefix-arg
        (read-from-minibuffer
         "SAS Table: ")
        nil)))
   (let* ((lookfor-table (or table (sas--get-point-symbol)))
          (sas-command
           (concat "proc print data="
                   lookfor-table
                   "(obs="
                   (number-to-string sas-view-maxnumber-of-rows)
                   "); run;")))
    (if sas-realsession
        (let ((process (sas-shell-get-process-or-error nil)))
          (when sas-verbose (message "Sent: %s" sas-command))
          (sas-shell-send-string sas-command process))
      (progn
        (when sas-verbose (message "Sent: %s" sas-command))
        (sas-send-string-with-shell-command sas-command sas-buffer-user-library)))))

(defcustom ess-sas-tab-stop-list
  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
  "List of tab stop positions used by `tab-to-tab-stop' in sas-mode."
  :type '(repeat integer)
  :group 'sas-mode)

(defvar sas-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?\\ "."  tab)  ;; backslash is punctuation
    (modify-syntax-entry ?+  "."  tab)
    (modify-syntax-entry ?-  "."  tab)
    (modify-syntax-entry ?=  "."  tab)
    (modify-syntax-entry ?%  "w"  tab)
    (modify-syntax-entry ?<  "."  tab)
    (modify-syntax-entry ?>  "."  tab)
    (modify-syntax-entry ?&  "w"  tab)
    (modify-syntax-entry ?|  "."  tab)
    (modify-syntax-entry ?\' "\"" tab)
    (modify-syntax-entry ?*  ". 23"  tab) ; comment character
    (modify-syntax-entry ?\; "."  tab)
    (modify-syntax-entry ?_  "w"  tab)
    (modify-syntax-entry ?<  "."  tab)
    (modify-syntax-entry ?>  "."  tab)
    (modify-syntax-entry ?/  ". 14"  tab) ; comment character
    (modify-syntax-entry ?.  "w"  tab)
    tab)
  "Syntax table for `sas-mode'.")

(defvar sas-mode-font-lock-comment01
  (list
  ;; .log NOTE: messages
       (cons "^NOTE [0-9]+-[0-9]+: Line generated by the invoked macro"
             font-lock-comment-face)
       (cons "^NOTE: .*$"                          font-lock-comment-face)
       (cons "^      [^ @].*[.]$"                   font-lock-comment-face)
       (cons "^      [a-z].*[a-z][ ]?$"            font-lock-comment-face)
       (cons "^      Engine:[ ]+V.+$"              font-lock-comment-face)
       (cons "^      Physical Name:[ ]+.+$"        font-lock-comment-face)
       (cons "^      \\(cpu\\|real\\) time[ ]+[0-9].*$"
             font-lock-comment-face)
       (cons "^      decimal may be shifted by the"
             font-lock-comment-face)
       (cons "^NOTE: The infile "                  font-lock-comment-face)
       (cons "^NOTE: 1 record was read from the infile "
             font-lock-comment-face)
       (cons "^NOTE: [1-9][0-9]* records were read from the infile "
             font-lock-comment-face)
       (cons "^      Filename=.*,$"                font-lock-comment-face)
       (cons "^      File Name=.*,$"               font-lock-comment-face)
       (cons "^      File $"                       font-lock-comment-face)
       (cons "^      Name=.*,$"                    font-lock-comment-face)
       (cons "^      File List=("                  font-lock-comment-face)
       (cons "^      List=("                       font-lock-comment-face)
       (cons "^      Owner Name=.*,$"              font-lock-comment-face)
       (cons "^      Access Permission=.*,$"       font-lock-comment-face)
       (cons "^      Last Modified=.*,?$"          font-lock-comment-face)
       (cons "^      File Size (bytes)=[0-9]+$"    font-lock-comment-face)
       (cons "^      Pipe command="                font-lock-comment-face)
       (cons "^NOTE: The file "                    font-lock-comment-face)
       (cons "^NOTE: 1 record was written to the file "
             font-lock-comment-face)
       (cons "^NOTE: [1-9][0-9]* records were written to the file "
             font-lock-comment-face)
       (cons "^NOTE: PROC LOGISTIC is modeling the probability that"
             font-lock-comment-face)
       (cons "^NOTE: PROC GENMOD is modeling the probability that"
             font-lock-comment-face)
       ;; Sas system message
       (cons "^1[ ]+The SAS System.*$"             font-lock-comment-face)
       (cons "^1[ ]+Le Système SAS.*$"             font-lock-comment-face)
       (cons "^[ ]+SAS/ETS[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       ;; Sas module
       (cons "^[ ]+SAS/IML[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       (cons "^[ ]+SAS/OR[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       (cons "^[ ]+SAS/QC[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       (cons "^[ ]+SAS/STAT[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       ;; uname
        (cons "^[ ]+Linux LIN X64.*$" font-lock-comment-face)
        (cons "^\014.*$"                            font-lock-comment-face)
       (cons "[*][*][*] ANNOTATE macros are now available [*][*][*]"
             font-lock-comment-face)
       (cons "For further information on ANNOTATE macros, enter,"
             font-lock-comment-face)
       (cons "\\(or \\)?%HELPANO.*$"
             font-lock-comment-face)
       (cons "^Local Variables:$"                  font-lock-comment-face)
       (cons "^End:$"                              font-lock-comment-face)
       (cons "^MPRINT([_A-Z0-9]+)"                 font-lock-comment-face)
       ))

(defvar sas-mode-font-lock-errors02
  (list
       ;; .log ERROR: messages
                                        ;     (cons "^ERROR\\( [0-9]+-[1-9][0-9][0-9]\\)?: .*$"
       (cons "^ERROR\\( [0-9]+-[0-9]+\\)?: .*$"
             font-lock-keyword-face)
                                        ;       ERROR:
       (cons "^       [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-keyword-face)
                                        ;       ERROR #-###:
       (cons "^             [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-keyword-face)
                                        ;       ERROR ##-###:
       (cons "^              [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-keyword-face)
                                        ;       ERROR ###-###:
       (cons "^               [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-keyword-face)
       (cons "^              a format name."       font-lock-keyword-face)
       (cons "^       where a numeric operand is required. The condition was: "
             font-lock-keyword-face)
       (cons "[ ][_]+$"                            font-lock-keyword-face)))

(defvar sas-mode-font-lock-warnings03
  (list
   ;; .log WARNING: messages
                                        ;(cons "^WARNING\\( [0-9]+-[1-9][0-9][0-9]\\)?: .*$"
       (cons "^WARNING\\( [0-9]+-[0-9]+\\)?: .*$"
             font-lock-function-name-face)
                                        ;       WARNING:
       (cons "^         [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-function-name-face)
                                        ;       WARNING #-###:
       (cons "^               [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-function-name-face)
                                        ;       WARNING ##-###:
       (cons "^                [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-function-name-face)
                                        ;       WARNING ###-###:
       (cons "^                 [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-function-name-face)

       ;; SAS comments
       ;; /* */ style handled by grammar above
       (cons "\\(^[0-9]*\\|[:;!]\\)[ \t]*%?\\*[^;/][^;]*;"
             font-lock-comment-face)))

(defvar sas-mode-font-lock-override04
  (list
                                            ; these over-rides need to come before the more general declarations
       (cons "\\<and("      font-lock-function-name-face)
       (cons "\\<data="     font-lock-keyword-face)
       (cons "\\<in:("      font-lock-function-name-face)
       (cons "\\<index("    font-lock-function-name-face)
       (cons "\\<input("    font-lock-function-name-face)
       (cons "\\<libname("  font-lock-function-name-face)
       (cons "\\<not("      font-lock-function-name-face)
       (cons "\\<or("       font-lock-function-name-face)
       (cons "\\<put("      font-lock-function-name-face)
       (cons "\\<sum("      font-lock-function-name-face)

                                        ; other idiosyncratic keywords
                                        ;(cons "key="      font-lock-keyword-face)
                                        ;(cons "/unique"   font-lock-keyword-face)
))

(defvar sas-mode-font-lock-execblocks05
  (list
  ;; SAS execution blocks: DATA, %MACRO/%MEND, %DO/%END, etc.
       (cons (regexp-opt '(
                           "data" "start" "return" ;"proc"
                           "%macro" "%mend"
                           "%do" "%to" "%by" "%end"
                           "%goto" "%go to"
                           "%if" "%then" "%else"
                           "%global" "%inc" "%include" "%input" "%local" "%let" "%put" "%sysexec"
                           ) 'words) font-lock-constant-face)
 ;; SAS execution blocks that must be followed by a semi-colon
       (cons (concat "\\<"
                     (regexp-opt
                      '(
                        "run;" "quit;" "endsas;" "finish;"
                        "cards;" "cards4;" "datalines;" "datalines4;" "lines;" "lines4;"
                        )))
             font-lock-constant-face)))

(defvar sas-mode-font-lock-statements06
  (list
       ;; SAS statements that must be followed by a semi-colon
       (cons (concat "\\<"
                     (regexp-opt
                      '(
                        "end;" "list;" "lostcard;" "page;" "stop;" ;"return;"
                        )))
             font-lock-keyword-face)

       ;; SAS statements that must be followed by an equal sign
       (cons (concat "\\<"
                     (regexp-opt
                      '(
                        "compress=" "in=" "out=" "sortedby="
                        )))
             font-lock-keyword-face)
   ))

(defvar sas-mode-font-lock-procname07
  (list
    ;; SAS procedure names
       (cons (concat "\\<proc[ ]+"
                     (regexp-opt '(
"access" "aceclus" "adaptivereg" "anom" "anova"
"append" "arima" "assess" "authlib" "autoreg"
"bchoice" "binning" "boolrule" "boxplot" "build"
"calendar" "calis" "cancorr" "candisc" "capability"
"cardinality" "carima" "catalog" "catmod" "causalmed"
"causaltrt" "ccopula" "cddm" "cdisc odm" "cdisc sdtm"
"cesm" "chart" "cimport" "cluster" "cntselect"
"compare" "computab" "contents" "copula" "copy"
"corr" "correlation" "corresp" "countreg" "cpanel"
"cport" "cqlim" "cspatialreg" "cusum" "datasets"
"datasource" "datekeys" "dbload" "delete" "discrim"
"display" "distance" "download" "ds2" "dstods2"
"ecm" "entropy" "esm" "expand" "export"
"factex" "factor" "fastclus" "fcmp" "fedsql"
"fmm" "fmtc2itm" "fontreg" "forecast" "format"
"freq" "freqtab" "fsbrowse" "fsedit" "fsletter "
"fslist" "fsview" "g3d" "g3grid" "gam"
"gammod" "gampl" "ganno" "gareabar" "gbarline"
"gchart" "gcontour" "gdevice" "gee" "genmod"
"genselect" "geocode" "gfont" "ginside" "gkpi"
"glimmix" "glm" "glmmod" "glmpower" "glmselect"
"gmap" "goptions" "gplot" "gproject" "gradar"
"greduce" "gremove" "greplay" "groovy" "gslide"
"gtile" "hadoop" "hdmd" "hmm" "hpbin"
"hpcandisc" "hpcdm" "hpcopula" "hpcorr" "hpcountreg"
"hpdmdb" "hpds2" "hpfmm" "hpgenselect" "hpimpute"
"hplmixed" "hplogistic" "hpmixed" "hpnlmod" "hppanel"
"hppls" "hpprincomp" "hpqlim" "hpquantselect" "hpreg"
"hpsample" "hpseverity" "hpsplit" "hpsummary" "http"
"ica" "iclifetest" "icphreg" "import" "inbreed"
"irt" "ishikawa" "javainfo" "json" "kclus"
"kde" "krige2d" "lattice" "lifereg" "lifetest"
"lmixed" "loan" "loess" "logistic" "logselect"
"lua" "macontrol" "mapimport" "mbc" "mcmc"
"mdc" "mds" "means" "mi" "mianalyze"
"migrate" "mixed" "modeclus" "model" "modelmatrix"
"multtest" "mvpdiagnose" "mvpmodel" "mvpmonitor" "nested"
"nlin" "nlmixed" "nlmod" "npar1way" "optex"
"options" "optload" "optlp" "optmilp" "optmodel"
"optqp" "optsave" "orthoreg" "panel" "pareto"
"partition" "pca" "pdlreg" "phreg" "phselect"
"plan" "plm" "plot" "pls" "plsmod"
"pmenu" "power" "presenv" "princomp" "prinqual"
"print" "printto" "probit" "product_status" "proto"
"prtdef" "prtexp" "psmatch" "pwencode" "qdevice"
"qlim" "qtrselect" "quantlife" "quantreg" "quantselect"
"rank" "rareevents" "reg" "registry" "regselect"
"reliability" "report" "robustreg" "rsreg" "s3"
"scaproc" "score" "scoreaccel" "seqdesign" "seqtest"
"severity" "sevselect" "sgdesign" "sgmap" "sgpanel"
"sgplot" "sgrender" "sgscatter" "shewart" "sim2d"
"similarity" "simlin" "simnormal" "smcalib" "smproject"
"smscore" "smspec" "soap" "sort" "spatialreg"
"spc" "spectra" "spp" "sql" "sqoop"
"ssm" "standard" "statespace" "stdize" "stdrate"
"stepdisc" "stream" "summary" "surveyfreq" "surveyimpute"
"surveylogistic" "surveymeans" "surveyphreg" "surveyreg" "surveyselect"
"syslin" "tabulate" "textmine" "timedata" "timeid"
"timeplot" "timeseries" "tmodel" "tmscore" "tpspline"
"transpose" "transreg" "tree" "treesplit" "tscsreg"
"tsinfo" "tsmodel" "tsreconcil" "ttest" "ucm"
"univariate" "upload" "varclus" "varcomp" "varimpute"
"variogram" "varmax" "varreduc" "x11" "x12"
"x13" "xsl"
                                   ) 'words)) font-lock-constant-face)

                                        ;       (cons (concat
                                        ;             "\\<"
                                        ;             "do[ \t]*" (regexp-opt '("over" "until" "while") t) "?"
                                        ;             "\\>")
                                        ;            font-lock-keyword-face)
                                        ;
   ))

(defvar sas-mode-font-lock-basegraphstatements08
  (list
       ;; SAS base and SAS/Graph statements
       (cons (concat ;"\\<"
              (regexp-opt
               '(
                 "do" "to" "by" "goto" ; "go"
                 "abort" "and" "array" "assess" "attrib"
                 "baseline" "bayes" "between" "bivar" "block" "bubble" "bubble2"
                 "change" "choro" "class" "contains" "contrast"
                 "delete" "display" "dm" "donut" "drop"
                 "else" "error" "exchange" "exclude"
                 "fcs" "file" "filename" "format" "freq"
                 "footnote" "footnote1" "footnote2" "footnote3" "footnote4" "footnote5"
                 "footnote6" "footnote7" "footnote8" "footnote9" "footnote10"
                 "goptions" "grid" ; "ge" "gt"
                 "hazardratio" "hbar" "hbar3d"
                 "id" "if" "index" "infile" "informat" "input" ; "is" rarely used, but common false pos.
                 "keep"
                 "label" "length" "libname" "like" "link" "lsmeans" ; "le" "lt"
                 "manova" "means" "merge" "missing" "model" "modify"
                 "not" "null" ; "ne" "note"
                 "ods" "options" "output" "otherwise" ; "or"
                 "pageby" "parms" "pie" "pie3d" "plot" "plot2" "prism" "put"
                 "random" "rename" "repeated" "retain"
                 "same" "save" "scatter" "select" "set" "skip" "star" "strata" "sum" "sumby" "surface"
                 "table" "tables" "test" "then" "time"
                 "title" "title1" "title2" "title3" "title4" "title5"
                 "title6" "title7" "title8" "title9" "title10"
                 "univar" "update"
                 "value" "var" "vbar" "vbar3d"
                 "weight" "where" "window" "with"
                                        ; "x"
                 ) 'words)) ;"\\>")
             font-lock-keyword-face)

       ;; SAS/GRAPH statements not handled above
       (cons (concat "\\<"
                     (regexp-opt
                      '("axis" "legend" "pattern" "symbol")) "\\([1-9][0-9]?\\)?"
                      "\\>")
             font-lock-keyword-face)
   ))

(defvar sas-mode-font-lock-macrosfunctions09
  (list
       ;; SAS functions and SAS macro functions
       (cons "%[a-z_][a-z_0-9]*[(;]"                  font-lock-function-name-face)
                                        ;(cons "\\<call[ \t]+[a-z]+("                   font-lock-function-name-face)
   ))

(defvar sas-mode-font-lock-functions10
  (list
       (cons (concat ;"\\<"
              (regexp-opt
               '(
"abs" "addr" "addrlong" "airy" "all"
"allcomb" "allperm" "any" "anyalnum" "anyalpha"
"anycntrl" "anydigit" "anyfirst" "anygraph" "anylower"
"anyname" "anyprint" "anypunct" "anyspace" "anyupper"
"anyxdigit" "apply" "arcos" "arcosh" "armasim"
"arsin" "arsinh" "artanh" "atan" "atan2"
"attrc" "attrn" "band" "beta" "betainv"
"bin" "bitfnbor" "blackclprc" "blackptprc" "blankstr"
"blkshclprc" "blkshptprc" "block" "blshift" "bnot"
"bor" "branks" "brshift" "bspline" "btran"
"bxor" "byte" "call allcomb" "call allcombi" "call allperm"
"call appcort" "call armacov" "call armalik" "call bar" "call box"
"call cats" "call catt" "call catx" "call change" "call compcost"
"call comport" "call delete" "call eigen" "call execute" "call executefile"
"call exportdatasettor" "call exportmatrixtor" "call exporttabletor" "call farmacov" "call farmafit"
"call farmalik" "call farmasim" "call fdif" "call gaend" "call gagetmem"
"call gagetval" "call gainit" "call gareeval" "call garegen" "call gasetcro"
"call gasetmut" "call gasetobj" "call gasetsel" "call geneig" "call graycode"
"call gscale" "call gsorth" "call heatmapcont" "call heatmapdisc" "call histogram"
"call importdatasetfromr" "call importmatrixfromr" "call ipf" "call is8601_convert" "call itsolver"
"call kalcvf" "call kalcvs" "call kaldff" "call kaldfs" "call label"
"call lav" "call lcp" "call lexcomb" "call lexcombi" "call lexperk"
"call lexperm" "call listadditem" "call listdeleteitem" "call listdeletename" "call listinsertitem"
"call listsetitem" "call listsetname" "call listsetsubitem" "call lms" "call logistic"
"call lp" "call lpsolve" "call lts" "call lupdt" "call marg"
"call maxqform" "call mcd" "call milpsolve" "call missing" "call module"
"call modulei" "call mve" "call nlpcg" "call nlpdd" "call nlpfdd"
"call nlpfea" "call nlphqn" "call nlplm" "call nlpnms" "call nlpnra"
"call nlpnrr" "call nlpqn" "call nlpqua" "call nlptr" "call ode"
"call odsgraph" "call ortvec" "call poke" "call pokelong" "call prxchange"
"call prxdebug" "call prxfree" "call prxnext" "call prxposn" "call prxsubstr"
"call push" "call qntl" "call qr" "call quad" "call queue"
"call ranbin" "call rancau" "call rancomb" "call randgen" "call randseed"
"call ranexp" "call rangam" "call rannor" "call ranperk" "call ranperm"
"call ranpoi" "call rantbl" "call rantri" "call ranuni" "call rename"
"call rupdt" "call rzlind" "call scan" "call scatter" "call seqscale"
"call seqshift" "call series" "call set" "call sleep" "call softmax"
"call solvelin" "call sort" "call sortc" "call sortn" "call sortndx"
"call sound" "call stdize" "call stream" "call streaminit" "call streamrewind"
"call svd" "call symput" "call symputx" "call system" "call tableaddvar"
"call tableprint" "call tablerenamevar" "call tablesetvarformat" "call tablesetvarinformat" "call tablesetvarlabel"
"call tablewritetodataset" "call tabulate" "call tanh" "call tpspline" "call tpsplnev"
"call tsbaysea" "call tsdecomp" "call tsmlocar" "call tsmlomar" "call tsmulmar"
"call tso" "call tspears" "call tspred" "call tsroot" "call tstvcar"
"call tsunimar" "call valset" "call varmacov" "call varmalik" "call varmasim"
"call vname" "call vnext" "call vnormal" "call vtsroot" "call wavft"
"call wavget" "call wavift" "call wavprint" "call wavthrsh" "call widetolong"
"cat" "catq" "cats" "catt" "catx"
"cdf" "cdf bernoulli distribution" "cdf beta distribution" "cdf binomial distribution" "cdf cauchy distribution"
"cdf chi square distribution" "cdf conway maxwell poisson distribution" "cdf exponential distribution" "cdf f distribution" "ceil"
"ceilz" "cexist" "char" "choose" "choosec"
"choosen" "cinv" "close" "cmiss" "cnonct"
"coalesce" "coalescec" "col" "collate" "colvec"
"comb" "compare" "compbl" "compfuzz" "compged"
"complev" "compound" "compress" "concat" "constant"
"contents" "convexit" "convx" "convxp" "corr"
"corr2cov" "cos" "cosh" "cot" "count"
"countc" "countmiss" "countn" "countunique" "countw"
"cov" "cov2corr" "covlag" "csc" "cshape"
"css" "cumipmt" "cumprinc" "cuprod" "curobs"
"cusum" "cv" "cvexhull" "daccdb" "daccdbsl"
"daccsl" "daccsyd" "dacctab" "dairy" "datasets"
"datdif" "date" "datejul" "datepart" "datetime"
"day" "dclose" "dcreate" "depdb" "depdbsl"
"depsl" "depsyd" "deptab" "dequote" "design"
"designf" "det" "deviance" "dhms" "diag"
"dif" "digamma" "dim" "dimension" "dinfo"
"distance" "divide" "dnum" "do" "dopen"
"doptname" "doptnum" "dosubl" "dread" "dropnote"
"dsname" "dsncatlgd" "dur" "duration" "durp"
"echelon" "effrate" "eigval" "eigvec" "element"
"envlen" "erf" "erfc" "euclid" "exist"
"exp" "expandgrid" "expmatrix" "fact" "fappend"
"fclose" "fcol" "fcopy" "fdelete" "fetch"
"fetchobs" "fexist" "fft" "fftc" "fget"
"fileexist" "filename" "fileref" "finance" "find"
"findc" "findw" "finfo" "finv" "fipname"
"fipnamel" "fipstate" "first" "floor" "floorz"
"fmtinfo" "fnonct" "fnote" "fopen" "foptname"
"foptnum" "forward" "fpoint" "fpos" "fput"
"fread" "frewind" "frlen" "froot" "fsep"
"full" "fuzz" "fwrite" "gaminv" "gamma"
"garkhclprc" "garkhptprc" "gasetup" "gcd" "geodist"
"geomean" "geomeanz" "getoption" "getvarc" "getvarn"
"ginv" "graycode" "hadamard" "half" "hankel"
"harmean" "harmeanz" "hashing" "hashing_file" "hashing_hmac"
"hashing_hmac_file" "hashing_hmac_init" "hashing_init" "hashing_part" "hashing_term"
"hbound" "hdir" "hermite" "hms" "holiday"
"holidayck" "holidaycount" "holidayname" "holidaynx" "holidayny"
"holidaytest" "homogen" "hour" "htmldecode" "htmlencode"
"i" "ibessel" "ifc" "ifft" "ifftc function"
"ifn" "importtablefromr" "index" "indexc" "indexw"
"input" "inputc" "inputn" "insert" "int"
"intcindex" "intck" "intcycle" "intfit" "intfmt"
"intget" "intindex" "intnx" "intrr" "intseas"
"intshift" "inttest" "intz" "inv" "invupdt"
"iorcmsg" "ipmt" "iqr" "irr" "isempty"
"isskipped" "j" "jbessel" "jroot" "juldate"
"juldate7" "kurtosis" "lag" "lambertw" "largest"
"lbound" "lcm" "lcomb" "left" "length"
"lengthc" "lengthm" "lengthn" "lexcomb" "lexcombi"
"lexperk" "lexperm" "lfact" "lgamma" "libname"
"libref" "listcreate" "listgetallnames" "listgetitem" "listgetname"
"listgetsubitem" "listindex" "listlen" "loc" "log"
"log10" "log1px" "log2" "logabsdet" "logbeta"
"logcdf" "logistic" "logpdf" "logsdf" "lowcase"
"lperm" "lpnorm" "mad" "magic" "mahalanobis"
"margrclprc" "margrptprc" "max" "md5" "mdy"
"mean" "median" "min" "minute" "missing"
"mod" "modexist" "module" "modulec" "moduleic"
"modulein" "modulen" "modz" "month" "mopen"
"mort" "msplint" "mvalid" "n" "name"
"ncol" "ndx2sub" "netpv" "nleng" "nliteral"
"nmiss" "nomrate" "norm" "normal" "notalnum"
"notalpha" "notcntrl" "notdigit" "note" "notfirst"
"notgraph" "notlower" "notname" "notprint" "notpunct"
"notspace" "notupper" "notxdigit" "npv" "nrow"
"num" "nvalid" "nwkdom" "open" "opscal"
"ordinal" "orpol" "palette" "parentname" "pathname"
"pctl" "pdf" "peek" "peekc" "peekclong"
"peeklong" "perm" "pmt" "point" "poisson"
"polyroot" "ppmt" "probbeta" "probbnml" "probbnrm"
"probchi" "probf" "probgam" "probhypr" "probit"
"probmc" "probnegb" "probnorm" "probt" "prod"
"product" "propcase" "prxchange" "prxmatch" "prxparen"
"prxparse" "prxposn" "ptrlongadd" "put" "putc"
"putn" "pv" "pvp" "qtr" "quantile"
"quartile" "quote" "ranbin" "rancau" "rancomb"
"rand" "randdirichlet" "randfun" "randmultinomial" "randmvt"
"randnormal" "randwishart" "ranexp" "rangam" "range"
"rank" "ranktie" "rannor" "ranperk" "ranperm"
"ranpoi" "rantbl" "rantri" "ranuni" "rates"
"ratio" "rdodt and rupdt" "remove" "rename" "repeat"
"resolve" "reverse" "rewind" "right" "rms"
"root" "round" "rounde" "roundz" "row"
"rowcat" "rowcatc" "rowvec" "rsubstr" "sample"
"saving" "savings" "scan" "sdf" "sec"
"second" "seq, seqscale, and seqshift" "setdif" "sha256" "sha256hex"
"sha256hmachex" "shape" "shapecol" "sign" "sin"
"sinh" "skewness" "sleep" "smallest" "soapweb"
"soapwebmeta" "soapwipservice" "soapwipsrs" "soapws" "soapwsmeta"
"solve" "sort" "soundex" "sparse" "spedis"
"spline and splinec" "splinev" "spot" "sqrsym" "sqrt"
"sqrvech" "squantile" "ssq" "standard" "std"
"stderr" "stfips" "stname" "stnamel" "storage"
"strip" "sub2ndx" "subpad" "substr" "substrn" "sum" "sumabs" "sweep"
"symexist" "symget" "symglobl" "symlocal" "symsqr"
"sysexist" "sysget" "sysmsg" "sysparm" "sysprocessid"
"sysprocessname" "sysprod" "sysrc" "system" "t"
"tablecreate" "tablecreatefromdataset" "tablegetvardata" "tablegetvarformat" "tablegetvarindex"
"tablegetvarinformat" "tablegetvarlabel" "tablegetvarname" "tablegetvartype" "tableisexistingvar"
"tableisvarnumeric" "tan" "tanh" "tfhilbert" "tfpwv"
"tfstft" "tfwindow" "time" "timepart" "timevalue"
"tinv" "tnonct" "today" "toeplitz" "trace"
"translate" "transtrn" "tranwrd" "trigamma" "trim"
"trimn" "trisolv" "trunc" "type" "typeof"
"tzoneid" "tzonename" "tzoneoff" "tzones2u" "tzoneu2s"
"uniform" "union" "unique" "uniqueby" "upcase"
"urldecode" "urlencode" "uss" "uuidgen" "value"
"var" "varfmt" "varinfmt" "varlabel" "varlen"
"varname" "varnum" "varray" "varrayx" "vartype"
"vecdiag" "vech" "verify" "vformat" "vformatd"
"vformatdx" "vformatn" "vformatnx" "vformatw" "vformatwx"
"vformatx" "vinarray" "vinarrayx" "vinformat" "vinformatd"
"vinformatdx" "vinformatn" "vinformatnx" "vinformatw" "vinformatwx"
"vinformatx" "vlabel" "vlabelx" "vlength" "vlengthx"
"vname" "vnamex" "vtype" "vtypex" "vvalue"
"vvaluex" "week" "weekday" "whichc" "whichn"
"xmult" "xsect" "year" "yield" "yieldp"
"yrdif" "yyq" "zipcity" "zipcitydistance" "zipfips"
"zipname" "zipnamel" "zipstate"
                 ) 'words) ;"\\>"
              "("); "[ \t]*(")
             font-lock-function-name-face)
   ))

(defvar sas-mode-font-lock-defaults
  (append sas-mode-font-lock-comment01
sas-mode-font-lock-errors02
sas-mode-font-lock-warnings03
sas-mode-font-lock-override04
sas-mode-font-lock-execblocks05
sas-mode-font-lock-statements06
sas-mode-font-lock-procname07
sas-mode-font-lock-basegraphstatements08
sas-mode-font-lock-macrosfunctions09
sas-mode-font-lock-functions10))

(define-derived-mode sas-mode prog-mode "sas"
  "Major mode for editing SAS source. "
  :group 'sas-mode
  (setq-local sentence-end ";[\t\n */]*")
  (setq-local paragraph-start "^[ \t]*$")
  (setq-local paragraph-separate "^[ \t]*$")
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local adaptive-fill-mode nil)
  ;(setq-local indent-line-function #'sas-indent-line)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/[*]")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[*]/")
  (setq-local comment-column 40)
;  (setq-local tab-stop-list ess-sas-tab-stop-list)
  (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(sas-mode-font-lock-defaults nil t))
  (set-syntax-table sas-mode-syntax-table))
(add-hook 'sas-mode-hook 'sas--initialize)


(add-to-list 'auto-mode-alist '("\\.[Ss][Aa][Ss]\\'" . sas-mode))

(provide 'sas)
;;; sas.el ends here
