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
(defcustom sas-shell-interpreter-args "-nodms -nonews -stdio -nofullstimer -nonumber -nodate -nocenter -terminal -pagesize max -nosyntaxcheck -cleanup"
  "Default arguments for the Sas interpreter to make a real session using comint.
\"-nodms -stdio\" is the important part."
  :type 'string
  :group 'sas)
(defcustom sas-shell-command-interpreter-args "-nodms -nonews -nofullstimer -nodate -nocenter -terminal -pagesize max -nosyntaxcheck"
  "Default arguments for the Sas (Unix/Linux) interpreter."
  :type 'string
  :group 'sas)
(defcustom sas-shell-command-interpreter-args-windows "-NOTERMINAL NOSPLASH -NOSTATUSWIN -NOICON "
  "Default arguments for the Sas (Windows) interpreter."
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
 :syntax-table nil
    (modify-syntax-entry ?\' ".")
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp sas-prompt-regexp)
  (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(sas-inferior-mode-font-lock-defaults nil t)))
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
        (comint-send-string process "\n;")))
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

(defun sas-shell-dedicated ()
  "is it a dedicated process or not"
  (let* ((global-proc-name  (sas-shell-get-process-name nil))
           (global-proc-buffer-name (format "*%s*" global-proc-name))
           (current-proc-buffer-name (sas-shell-get-buffer)))
    (if (string= global-proc-buffer-name current-proc-buffer-name)
        nil t)))

(defun sas-clear-log-buffer ()
    "send comint-clear-buffer to log file."
  (interactive)
  (if sas-realsession
  (let ((file-log (sas-shell-command-get-errorbuffer-name (sas-shell-dedicated))))
    (with-current-buffer file-log
      (comint-clear-buffer)))))

(defun sas-clear-output-buffer ()
    "send comint-clear-buffer to log file."
  (interactive)
    (with-current-buffer (format "*%s*" (sas-shell-get-process-name nil))
      (comint-clear-buffer)))

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

(defvar sas-doc-proc-list
'(("carima" . ("casecon" "casecon_carima_syntax.htm"))
 ("ccopula" . ("casecon" "casecon_ccopula_syntax02.htm"))
 ("cesm" . ("casecon" "casecon_cesm_syntax.htm"))
 ("cesm" . ("casecon" "casecon_cesm_syntax.htm"))
 ("cntselect" . ("casecon" "casecon_cntselect_syntax.htm"))
 ("cpanel" . ("casecon" "casecon_cpanel_syntax.htm"))
 ("cqlim" . ("casecon" "casecon_cqlim_syntax.htm"))
 ("cspatialreg" . ("casecon" "casecon_cspatialreg_syntax.htm"))
 ("ecm" . ("casecon" "casecon_ecm_syntax.htm"))
 ("hmm" . ("casecon" "casecon_hmm_syntax.htm"))
 ("sevselect" . ("casecon" "casecon_sevselect_syntax.htm"))
 ("tsmodel" . ("casecon" "casecon_tsmodel_syntax02.htm"))
 ("optlp" . ("casmopt" "casmopt_optlp_syntax10.htm"))
 ("optmilp" . ("casmopt" "casmopt_optmilp_syntax09.htm"))
 ("optqp" . ("casmopt" "casmopt_optqp_syntax.htm"))
 ("anom" . ("qcug" "qcug_anom_sect007.htm"))
 ("capability" . ("qcug" "qcug_capability_sect008.htm"))
 ("cusum" . ("qcug" "qcug_cusum_sect005.htm"))
 ("factex" . ("qcug" "qcug_factex_syntax.htm"))
 ("ishikawa" . ("qcug" "qcug_ishikawa_syntax.htm"))
 ("macontrol" . ("qcug" "qcug_macontrol_sect005.htm"))
 ("mvpdiagnose" . ("qcug" "qcug_mvpdiagnose_gettingstarted.htm"))
 ("mvpmodel" . ("qcug" "qcug_mvpmodel_gettingstarted02.htm"))
 ("mvpmonitor" . ("qcug" "qcug_mvpmonitor_gettingstarted01.htm"))
 ("optex" . ("qcug" "qcug_optex_syntax.htm"))
 ("pareto" . ("qcug" "qcug_pareto_syntax01.htm"))
 ("rareevents" . ("qcug" "qcug_rareevents_gettingstarted.htm"))
 ("reliability" . ("qcug" "qcug_reliability_syntax.htm"))
 ("aceclus" . ("statug" "statug_aceclus_syntax.htm"))
 ("anova" . ("statug" "statug_anova_syntax.htm"))
 ("bchoice" . ("statug" "statug_bchoice_syntax.htm"))
 ("boxplot" . ("statug" "statug_boxplot_syntax.htm"))
 ("calis" . ("statug" "statug_calis_syntax.htm"))
 ("cancorr" . ("statug" "statug_cancorr_syntax.htm"))
 ("candisc" . ("statug" "statug_candisc_syntax.htm"))
 ("catmod" . ("statug" "statug_catmod_syntax.htm"))
 ("causalmed" . ("statug" "statug_causalmed_syntax.htm"))
 ("causaltrt" . ("statug" "statug_causaltrt_syntax.htm"))
 ("cluster" . ("statug" "statug_cluster_syntax.htm"))
 ("corresp" . ("statug" "statug_corresp_syntax.htm"))
 ("discrim" . ("statug" "statug_discrim_syntax.htm"))
 ("factor" . ("statug" "statug_factor_syntax.htm"))
 ("fastclus" . ("statug" "statug_fastclus_syntax.htm"))
 ("fmm" . ("statug" "statug_fmm_syntax01.htm"))
 ("freq" . ("procstat" "procstat_freq_syntax.htm"))
 ("gam" . ("statug" "statug_gam_syntax.htm"))
 ("gampl" . ("statug" "statug_hpgam_syntax01.htm"))
 ("gee" . ("statug" "statug_gee_syntax.htm"))
 ("genmod" . ("statug" "statug_genmod_syntax.htm"))
 ("glimmix" . ("statug" "statug_glimmix_syntax01.htm"))
 ("glm" . ("statug" "statug_glm_syntax.htm"))
 ("glmmod" . ("statug" "statug_glmmod_syntax.htm"))
 ("glmpower" . ("statug" "statug_glmpower_syntax.htm"))
 ("glmselect" . ("statug" "statug_glmselect_syntax.htm"))
 ("hpcandisc" . ("statug" "statug_hpcandisc_syntax01.htm"))
 ("hpfmm" . ("statug" "statug_hpfmm_syntax01.htm"))
 ("hpgenselect" . ("statug" "statug_hpgenselect_syntax01.htm"))
 ("hplmixed" . ("statug" "statug_hplmixed_syntax01.htm"))
 ("hplogistic" . ("statug" "statug_hplogistic_syntax01.htm"))
 ("hpmixed" . ("statug" "statug_hpmixed_syntax01.htm"))
 ("hpnlmod" . ("statug" "statug_hpnlin_syntax01.htm"))
 ("hppls" . ("statug" "statug_hppls_syntax.htm"))
 ("hpprincomp" . ("statug" "statug_hpprincomp_syntax01.htm"))
 ("hpquantselect" . ("statug" "statug_hpqtr_syntax01.htm"))
 ("hpreg" . ("statug" "statug_hpreg_syntax01.htm"))
 ("hpsplit" . ("statug" "statug_hpsplit_syntax01.htm"))
 ("iclifetest" . ("statug" "statug_iclifetest_syntax.htm"))
 ("icphreg" . ("statug" "statug_icphreg_syntax01.htm"))
 ("inbreed" . ("statug" "statug_inbreed_syntax.htm"))
 ("irt" . ("statug" "statug_irt_syntax.htm"))
 ("kde" . ("statug" "statug_kde_syntax.htm"))
 ("krige2d" . ("statug" "statug_krige2d_syntax.htm"))
 ("lattice" . ("statug" "statug_lattice_syntax.htm"))
 ("lifereg" . ("statug" "statug_lifereg_syntax.htm"))
 ("lifetest" . ("statug" "statug_lifetest_syntax01.htm"))
 ("logistic" . ("statug" "statug_logistic_syntax.htm"))
 ("mcmc" . ("statug" "statug_mcmc_syntax.htm"))
 ("mds" . ("statug" "statug_mds_syntax.htm"))
 ("mixed" . ("statug" "statug_mixed_syntax.htm"))
 ("modeclus" . ("statug" "statug_modeclus_syntax.htm"))
 ("multtest" . ("statug" "statug_multtest_syntax.htm"))
 ("nested" . ("statug" "statug_nested_syntax.htm"))
 ("nlin" . ("statug" "statug_nlin_syntax.htm"))
 ("nlmixed" . ("statug" "statug_nlmixed_syntax.htm"))
 ("npar1way" . ("statug" "statug_npar1way_syntax.htm"))
 ("orthoreg" . ("statug" "statug_orthoreg_syntax.htm"))
 ("phreg" . ("statug" "statug_phreg_syntax01.htm"))
 ("plan" . ("statug" "statug_plan_syntax.htm"))
 ("plm" . ("statug" "statug_plm_syntax01.htm"))
 ("pls" . ("statug" "statug_pls_syntax.htm"))
 ("power" . ("statug" "statug_power_syntax.htm"))
 ("princomp" . ("statug" "statug_princomp_syntax.htm"))
 ("prinqual" . ("statug" "statug_prinqual_syntax.htm"))
 ("probit" . ("statug" "statug_probit_syntax.htm"))
 ("psmatch" . ("statug" "statug_psmatch_syntax08.htm"))
 ("quantlife" . ("statug" "statug_quantlife_syntax.htm"))
 ("quantreg" . ("statug" "statug_qreg_syntax04.htm"))
 ("quantselect" . ("statug" "statug_qrsel_syntax.htm"))
 ("reg" . ("statug" "statug_reg_syntax.htm"))
 ("robustreg" . ("statug" "statug_rreg_syntax.htm"))
 ("rsreg" . ("statug" "statug_rsreg_syntax.htm"))
 ("score" . ("statug" "statug_score_syntax.htm"))
 ("sim2d" . ("statug" "statug_sim2d_syntax.htm"))
 ("simnormal" . ("statug" "statug_simnorm_syntax.htm"))
 ("spp" . ("statug" "statug_spp_syntax.htm"))
 ("stdize" . ("statug" "statug_stdize_syntax.htm"))
 ("stepdisc" . ("statug" "statug_stepdisc_syntax.htm"))
 ("surveyfreq" . ("statug" "statug_surveyfreq_syntax.htm"))
 ("surveylogistic" . ("statug" "statug_surveylogistic_syntax.htm"))
 ("surveymeans" . ("statug" "statug_surveymeans_syntax.htm"))
 ("surveyphreg" . ("statug" "statug_surveyphreg_syntax11.htm"))
 ("surveyreg" . ("statug" "statug_surveyreg_syntax.htm"))
 ("surveyselect" . ("statug" "statug_surveyselect_syntax.htm"))
 ("tpspline" . ("statug" "statug_tpspline_syntax.htm"))
 ("transreg" . ("statug" "statug_transreg_syntax.htm"))
 ("tree" . ("statug" "statug_tree_syntax.htm"))
 ("ttest" . ("statug" "statug_ttest_syntax.htm"))
 ("varclus" . ("statug" "statug_varclus_syntax.htm"))
 ("varcomp" . ("statug" "statug_varcomp_syntax01.htm"))
 ("variogram" . ("statug" "statug_variogram_syntax01.htm"))
 ("gampl" . ("statug" "statug_hpgam_syntax01.htm"))
 ("hpcandisc" . ("statug" "statug_hpcandisc_syntax01.htm"))
 ("hpfmm" . ("statug" "statug_hpfmm_syntax01.htm"))
 ("hpgenselect" . ("statug" "statug_hpgenselect_syntax01.htm"))
 ("hplmixed" . ("statug" "statug_hplmixed_syntax01.htm"))
 ("hplogistic" . ("statug" "statug_hplogistic_syntax01.htm"))
 ("hpnlmod" . ("statug" "statug_hpnlin_syntax01.htm"))
 ("hppls" . ("statug" "statug_hppls_syntax.htm"))
 ("hpprincomp" . ("statug" "statug_hpprincomp_syntax01.htm"))
 ("hpquantselect" . ("statug" "statug_hpqtr_syntax01.htm"))
 ("hpreg" . ("statug" "statug_hpreg_syntax01.htm"))
 ("hpsplit" . ("statug" "statug_hpsplit_syntax01.htm"))
 ("boolrule" . ("casml" "casml_boolrule_syntax01.htm"))
 ("textmine" . ("casml" "casml_textmine_syntax01.htm"))
 ("tmscore" . ("casml" "casml_tmscore_syntax01.htm"))
 ("correlation" . ("casstat" "casstat_corr_syntax.htm"))
 ("freqtab" . ("casstat" "casstat_freqtab_syntax.htm"))
 ("gammod" . ("casstat" "casstat_gammod_syntax01.htm"))
 ("genselect" . ("casstat" "casstat_genselect_syntax01.htm"))
 ("ica" . ("casstat" "casstat_ica_syntax01.htm"))
 ("lmixed" . ("casstat" "casstat_lmixed_syntax01.htm"))
 ("mbc" . ("casstat" "casstat_mbc_syntax.htm"))
 ("modelmatrix" . ("casstat" "casstat_modelmatrix_syntax01.htm"))
 ("nlmod" . ("casstat" "casstat_nlmod_syntax01.htm"))
 ("pca" . ("casstat" "casstat_pca_syntax01.htm"))
 ("plsmod" . ("casstat" "casstat_plsmod_syntax.htm"))
 ("qtrselect" . ("casstat" "casstat_qtrselect_syntax01.htm"))
 ("regselect" . ("casstat" "casstat_regselect_syntax01.htm"))
 ("spc" . ("casstat" "casstat_spc_details62.htm"))
 ("treesplit" . ("casstat" "casstat_treesplit_syntax01.htm"))
 ("cardinality" . ("casstat" "casstat_cardinality_syntax03.htm"))
 ("tsinfo" . ("casforecast" "casforecast_tsinfo_syntax.htm"))
 ("tsmodel" . ("casecon" "casecon_tsmodel_syntax02.htm"))
 ("corr" . ("procstat" "procstat_corr_syntax.htm"))
 ("freq" . ("procstat" "procstat_freq_syntax.htm"))
 ("univariate" . ("procstat" "procstat_univariate_gettingstarted04.htm"))
 ("sql" . ("sqlproc" "n1oihmdy7om5rmn1aorxui3kxizl.htm"))
 ("hpbin" . ("prochp" "prochp_hpbin_syntax07.htm"))
 ("hpcorr" . ("prochp" "prochp_hpcorr_syntax.htm"))
 ("hpdmdb" . ("prochp" "prochp_hpdmdb_syntax01.htm"))
 ("hpds2" . ("prochp" "prochp_hpds2_syntax01.htm"))
 ("hpimpute" . ("prochp" "prochp_hpimpute_syntax01.htm"))
 ("hpsample" . ("prochp" "prochp_hpsample_syntax01.htm"))
 ("hpsummary" . ("prochp" "prochp_hpsummary_syntax01.htm"))
 ("arima" . ("etsug" "etsug_arima_syntax.htm"))
 ("autoreg" . ("etsug" "etsug_autoreg_syntax.htm"))
 ("computab" . ("etsug" "etsug_computab_syntax.htm"))
 ("copula" . ("etsug" "etsug_copula_syntax02.htm"))
 ("countreg" . ("etsug" "etsug_countreg_syntax.htm"))
 ("datasource" . ("etsug" "etsug_datasrc_syntax.htm"))
 ("entropy" . ("etsug" "etsug_entropy_syntax.htm"))
 ("esm" . ("etsug" "etsug_esm_syntax.htm"))
 ("expand" . ("etsug" "etsug_expand_syntax.htm"))
 ("hpcdm" . ("etsug" "etsug_hpcdm_syntax.htm"))
 ("hpcopula" . ("etsug" "etsug_hpcopula_syntax02.htm"))
 ("hpcountreg" . ("etsug" "etsug_hpcountreg_syntax.htm"))
 ("hppanel" . ("etsug" "etsug_hppanel_syntax.htm"))
 ("hpqlim" . ("etsug" "etsug_hpqlim_syntax.htm"))
 ("hpseverity" . ("etsug" "etsug_hpseverity_syntax.htm"))
 ("loan" . ("etsug" "etsug_loan_syntax.htm"))
 ("mdc" . ("etsug" "etsug_mdc_syntax.htm"))
 ("model" . ("etsug" "etsug_model_sect012.htm"))
 ("panel" . ("etsug" "etsug_panel_syntax.htm"))
 ("pdlreg" . ("etsug" "etsug_pdlreg_syntax.htm"))
 ("qlim" . ("etsug" "etsug_qlim_syntax.htm"))
 ("severity" . ("etsug" "etsug_severity_syntax.htm"))
 ("similarity" . ("etsug" "etsug_similarity_syntax.htm"))
 ("simlin" . ("etsug" "etsug_simlin_syntax.htm"))
 ("spatialreg" . ("etsug" "etsug_spatialreg_syntax.htm"))
 ("spectra" . ("etsug" "etsug_spectra_syntax.htm"))
 ("ssm" . ("etsug" "etsug_ssm_syntax.htm"))
 ("statespace" . ("etsug" "etsug_statespa_syntax.htm"))
 ("syslin" . ("etsug" "etsug_syslin_syntax.htm"))
 ("timedata" . ("etsug" "etsug_timedata_syntax.htm"))
 ("timeid" . ("etsug" "etsug_timeid_syntax.htm"))
 ("timeseries" . ("etsug" "etsug_timeseries_syntax.htm"))
 ("tscsreg" . ("etsug" "etsug_tscsreg_syntax.htm"))
 ("ucm" . ("etsug" "etsug_ucm_syntax.htm"))
 ("varmax" . ("etsug" "etsug_varmax_syntax.htm"))
 ("x11" . ("etsug" "etsug_x11_syntax.htm"))
 ("x12" . ("etsug" "etsug_x13_syntax.htm"))
 ("x13" . ("etsug" "etsug_x13_syntax.htm"))
 ("fsbrowse" . ("acdcdb" "p0gg19wrn2q4mon1lwlds79psnr2.htm"))
 ("fsedit" . ("acdcdb" "p0gg19wrn2q4mon1lwlds79psnr2.htm"))
 ("fsletter " . ("acdcdb" "p0gg19wrn2q4mon1lwlds79psnr2.htm"))
 ("fsview" . ("acdcdb" "p0gg19wrn2q4mon1lwlds79psnr2.htm"))
 ("append" . ("proc" "p1bjrbc5esr90on12o8vs7gyv8ue.htm"))
 ("authlib" . ("proc" "p151dxf9titjztn1xp4r3au5we7x.htm"))
 ("calendar" . ("proc" "p0o6x3nlesudu6n1wipok8q92knu.htm"))
 ("catalog" . ("proc" "p1il9pe2plu3bpn0zza8igp6xue4.htm"))
 ("chart" . ("proc" "n0pwvsa3dxdyrzn1gsna5auc0fsy.htm"))
 ("cimport" . ("proc" "p1be86uw07acoin1aodzspbmkg8m.htm"))
 ("compare" . ("proc" "n0c1y14wyd3u7yn1dmfcpaejllsn.htm"))
 ("contents" . ("proc" "n1hqa4dk5tay0an15nrys1iwr5o2.htm"))
 ("copy" . ("proc" "p04y85z9f13uaan10s1k40ptx6gs.htm"))
 ("cport" . ("proc" "n0orwyj6rj0hq9n1bmsbozl1jgcw.htm"))
 ("datasets" . ("proc" "p0xdkenol7pi1cn14p0iq38shax4.htm"))
 ("datekeys" . ("proc" "p02teyw4mikic6n1kkzovv1wn4n4.htm"))
 ("delete" . ("proc" "n1469bsvzgd33jn0z4xecxmx2bgo.htm"))
 ("display" . ("proc" "n02ci2yf9rg65rn16avs4ge1nri6.htm"))
 ("ds2" . ("proc" "n0ox2hnyx7twb2n13200g5hqqsmy.htm"))
 ("dstods2" . ("proc" "p0ydariamjl8ken1ahfrvdxldyrx.htm"))
 ("export" . ("proc" "n0ku4pxzx3d2len10ozjgyjbrpl9.htm"))
 ("fcmp" . ("proc" "p0urpv7yyzylqsn1g2fycva2bs3n.htm"))
 ("fedsql" . ("proc" "n06w5kqwkurgk2n1irjo6h94fkcq.htm"))
 ("fontreg" . ("proc" "p199jutvq45hmun1tg4h11umqnsw.htm"))
 ("format" . ("proc" "n1c16dxnndwfzyn14o1kb8a4312m.htm"))
 ("fslist" . ("proc" "p0kmaczvgok09qn1tx9lzzxmj9ru.htm"))
 ("groovy" . ("proc" "p0pq7kp0misx44n14vvub91rzihh.htm"))
 ("hadoop" . ("proc" "n02m72os18twk1n17a8o6o885598.htm"))
 ("hdmd" . ("proc" "p0l3psv40hsx26n18b7ttb7a32o6.htm"))
 ("http" . ("proc" "n197g47i7j66x9n15xi0gaha8ov6.htm"))
 ("import" . ("proc" "n18jyszn33umngn14czw2qfw7thc.htm"))
 ("javainfo" . ("proc" "n0ip2xay8coflrn1g6iy7u376k9w.htm"))
 ("json" . ("proc" "n0nejfk9q0pzmnn181l92qk3ah2e.htm"))
 ("lua" . ("proc" "p0lqta2cbq9b44n12h28nil7a093.htm"))
 ("means" . ("proc" "p0f0fjpjeuco4gn1ri963f683mi4.htm"))
 ("migrate" . ("proc" "p0z08wz8jd550kn152x6t8yo7fl2.htm"))
 ("options" . ("proc" "n18l72ouwixr9un16q91zsfjeu3b.htm"))
 ("optload" . ("proc" "p18y99r0z9n3d3n1schuouoyv4tr.htm"))
 ("optsave" . ("proc" "n0qx6so0ctrt1vn16jmiaxznhx7z.htm"))
 ("plot" . ("proc" "p0mofisj5dox85n176ibozlk8jpn.htm"))
 ("pmenu" . ("proc" "n1bst3az8mv11nn1g78htyb36ytt.htm"))
 ("presenv" . ("proc" "p1jkqvjsiwpf68n1o2y7zya9e9jz.htm"))
 ("print" . ("proc" "n17dcq1elcvpvkn1pkecj41cva6j.htm"))
 ("printto" . ("proc" "p1hwvc03z4tqlkn1owzhzo8e7ulu.htm"))
 ("product_status" . ("proc" "p167ky4zsoxrn2n1myz6iisao1wc.htm"))
 ("proto" . ("proc" "n16bo79ptv9kugn16m2x0rrg1uh6.htm"))
 ("prtdef" . ("proc" "n0iqj7fujgyxa5n12cegrvudi2ll.htm"))
 ("pwencode" . ("proc" "n0dc6in0v7nfain1f2whl6f5x66p.htm"))
 ("qdevice" . ("proc" "n1qgi4n3ko5gcvn11df2ygxvkis7.htm"))
 ("rank" . ("proc" "p16s2o8e4bnqrin1phywxdaxqba7.htm"))
 ("registry" . ("proc" "p0dakeryjykv6rn1x83x60323cz3.htm"))
 ("report" . ("proc" "n1dz7jdasx5t56n1rmlx346dyk6n.htm"))
 ("s3" . ("proc" "n1h9c1jnr8v8nwn1l6g83kyne5ds.htm"))
 ("scaproc" . ("proc" "n0pbvhhaw4f7f7n1ukrvacsgulx1.htm"))
 ("scoreaccel" . ("proc" "p1vd98jpf7dve8n1t5q5n7fhn36c.htm"))
 ("soap" . ("proc" "p0vfcfb2tsfqz2n15q6flvduy7da.htm"))
 ("sort" . ("proc" "p02bhn81rn4u64n1b6l00ftdnxge.htm"))
 ("sqoop" . ("proc" "n0pozq3rgnj1q0n1pcvweostxogo.htm"))
 ("standard" . ("proc" "p08lchizi3ii8yn10lzjjegwwazv.htm"))
 ("stream" . ("proc" "n12zrkr08eiacmn17lcv4fmt79tb.htm"))
 ("summary" . ("proc" "p0aq3hsvflztfzn1xa2wt6s35oy6.htm"))
 ("tabulate" . ("proc" "n00yutbvvckjwrn1ldg5xkvjy1pu.htm"))
 ("timeplot" . ("proc" "p0nz7lq1tyoz2hn1v0z2fd6iymz3.htm"))
 ("transpose" . ("proc" "p1r2tjnp8ewe3sn1acnpnrs3xbad.htm"))
 ("xsl" . ("proc" "p0209aryd0fkqtn1061f7d84uni4.htm"))
 ("sgdesign" . ("grstatproc" "n0p9wopcnykhjcn0zk3vdxp55i3k.htm"))
 ("sgpanel" . ("grstatproc" "n0zgx9a7en0g5vn1tjpja6sxdiwr.htm"))
 ("sgplot" . ("grstatproc" "n0yjdd910dh59zn1toodgupaj4v9.htm"))
 ("sgrender" . ("grstatproc" "n131vj4eka12zpn1iwbqufke8l4z.htm"))
 ("sgscatter" . ("grstatproc" "n1je1qlb5bvtypn1rhbeyqw5h9rc.htm"))
 ("geocode" . ("grmapref" "p087i29802bnfgn1qn9isdqzde0h.htm"))
 ("ginside" . ("grmapref" "p1frskc294tbapn1wwumr6m4d3ka.htm"))
 ("gmap" . ("grmapref" "n01e0bon6tv6ncn1a7dlrr5fh5tr.htm"))
 ("gproject" . ("grmapref" "n0h81palrr0ucqn11g9r0y3incml.htm"))
 ("greduce" . ("grmapref" "n0grevqu4asrarn1lmo5eblj0sxz.htm"))
 ("gremove" . ("grmapref" "p18fpxx3tn5br4n1ql9aghlvht2p.htm"))
 ("mapimport" . ("grmapref" "p0qsj9oazola04n10usle21dvk67.htm"))
 ("sgmap" . ("grmapref" "n18zwxj3couxmgn161trks2usk2v.htm"))
 ("ganno" . ("graphref" "p198vvex23amofn14tfpdoxr2ldp.htm"))
 ("gareabar" . ("graphref" "n0d98duo2krvhnn1xrx1xlsk4fpw.htm"))
 ("gbarline" . ("graphref" "p0gjzffw4b5o69n1qvhxlhv93jcq.htm"))
 ("gchart" . ("graphref" "n1dvg3qp3e9pann10us1wxo0qe0q.htm"))
 ("gcontour" . ("graphref" "p1p773d7i495lnn1rbh99a57a629.htm"))
 ("gdevice" . ("graphref" "p16lpnocinbyd6n1k25978duqibj.htm"))
 ("gfont" . ("graphref" "n02mmdv4o3kayen1h2oiuh3z8a70.htm"))
 ("goptions" . ("graphref" "n0ldbseideb55sn1pyba3to2931s.htm"))
 ("gplot" . ("graphref" "n1e0onjgevom4kn1hbiibc6w34oe.htm"))
 ("greplay" . ("graphref" "n1xeeaqfpcqb2sn1oelt59x2z8pt.htm"))
 ("gslide" . ("graphref" "p0kr9k2lndrguyn1m3hiyvzu9q5k.htm"))
 ("gtile" . ("graphref" "p03yt7egmjxdu6n1mqitkfva5zf5.htm"))
 ("g3d" . ("graphref" "p08z2bo1q18b2bn1exmky9q1t1h7.htm"))
 ("g3grid" . ("graphref" "n1spr0z93tjza0n1xx94gfaxklfb.htm"))
 ("optmodel" . ("casmopt" "casmopt_optmodel_syntax02.htm"))
 ("adaptivereg" . ("statug" "statug_adaptivereg_syntax01.htm"))
 ("distance" . ("statug" "statug_distance_syntax01.htm"))
 ("loess" . ("statug" "statug_loess_syntax01.htm"))
 ("mi" . ("statug" "statug_mi_syntax01.htm"))
 ("mianalyze" . ("statug" "statug_mianalyze_syntax01.htm"))
 ("seqdesign" . ("statug" "statug_seqdesign_syntax01.htm"))
 ("seqtest" . ("statug" "statug_seqtest_syntax01.htm"))
 ("stdrate" . ("statug" "statug_stdrate_syntax01.htm"))
 ("surveyimpute" . ("statug" "statug_surveyimpute_syntax01.htm"))
 ("kclus" . ("casstat" "casstat_kclus_syntax01.htm"))
 ("logselect" . ("casstat" "casstat_logselect_syntax01.htm"))
 ("phselect" . ("casstat" "casstat_phselect_syntax01.htm"))
 ("assess" . ("casstat" "casstat_assess_syntax01.htm"))
 ("binning" . ("casstat" "casstat_binning_syntax01.htm"))
 ("varimpute" . ("casstat" "casstat_varimpute_syntax01.htm"))
 ("smcalib" . ("casforecast" "casforecast_smcalib_syntax01.htm"))
 ("smproject" . ("casforecast" "casforecast_smproject_syntax01.htm"))
 ("smscore" . ("casforecast" "casforecast_smscore_syntax01.htm"))
 ("partition" . ("casstat" "casstat_partition_syntax02.htm"))
 ("smspec" . ("casforecast" "casforecast_smspec_syntax02.htm"))
 ("prtexp" . ("proc" "p0kqciwgisfnd8n1e2i5goiehp3i.htm"))
 ("tmodel" . ("etsug" "etsug_tmodel_syntax12.htm"))))

(defun sas-doc-proc-dwim ()
  "return help for proc name at point."
  (interactive)
  (let* ((procname (sas--get-point-symbol))
         (helplist (cdr (assoc procname sas-doc-proc-list)))
         (url-prochelp (concat
                        "https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.3/"
                        (car helplist) "/" (car (cdr helplist)))))
    (browse-url url-prochelp)))

(defvar sas-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?*  ". 23" ) ; comment character
    (modify-syntax-entry ?/  ". 14" ) ; comment character
    (modify-syntax-entry ?\; "." )
    (modify-syntax-entry ?%  "w" )
    (modify-syntax-entry ?&  "w" )
    (modify-syntax-entry ?_  "w" )
    (modify-syntax-entry ?.  "w" )
    (modify-syntax-entry ?\\ "." )  ;; backslash is punctuation
    (modify-syntax-entry ?+  "." )
    (modify-syntax-entry ?-  "." )
    (modify-syntax-entry ?=  "." )
    (modify-syntax-entry ?<  "." )
    (modify-syntax-entry ?>  "." )
    (modify-syntax-entry ?|  "." )
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?<  "." )
    (modify-syntax-entry ?>  "." )
    tab)
  "Syntax table for `sas-mode'.")

(defvar sas-inferior-mode-font-lock-comment01
  (list
  ;; .log NOTE: messages
       (cons "^NOTE [0-9]+-[0-9]+: Line generated by the invoked macro"
             font-lock-comment-face)
       (cons "^NOTE: .*$"                          font-lock-comment-face)
       (cons "^      Autorisation.*$"                   font-lock-comment-face)
       (cons "^      [A-Z].*$"                   font-lock-comment-face)
       (cons "^      \\(cpu\\|real\\) time[ ]+[0-9].*$"
             font-lock-comment-face)
       (cons "^NOTE: The infile "                  font-lock-comment-face)
       (cons "^NOTE: 1 record was read from the infile "
             font-lock-comment-face)
       (cons "^NOTE: [1-9][0-9]* records were read from the infile "
             font-lock-comment-face)
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

(defvar sas-inferior-mode-font-lock-errors02
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

(defvar sas-inferior-mode-font-lock-warnings03
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

(defvar sas-inferior-mode-font-lock-defaults
  (append sas-inferior-mode-font-lock-comment01
sas-inferior-mode-font-lock-errors02
sas-inferior-mode-font-lock-warnings03
sas-mode-font-lock-override04
sas-mode-font-lock-execblocks05
sas-mode-font-lock-statements06
sas-mode-font-lock-procname07
sas-mode-font-lock-basegraphstatements08
sas-mode-font-lock-macrosfunctions09
sas-mode-font-lock-functions10))
(defvar sas-mode-font-lock-defaults
  (append sas-mode-font-lock-override04
sas-mode-font-lock-execblocks05
sas-mode-font-lock-statements06
sas-mode-font-lock-procname07
sas-mode-font-lock-basegraphstatements08
sas-mode-font-lock-macrosfunctions09
sas-mode-font-lock-functions10))

(require 'smie)
(defvar sas-smie-sample-grammar nil
  "Sample BNF grammar for `smie'.")
(setq sas-smie-sample-grammar
      (smie-prec2->grammar
       (smie-merge-prec2s
        (smie-bnf->prec2
         '((atom)
           (formula
            ("(" formula ")")
            (atom "*" atom)
            (atom "|" atom)
                  (atom))
           ;; (instsp (instsp ";" instsp) (instp))
           (instsd (instsd ";" instsd) (instd))
           (instsm (instsm ";" instsm) (instm))
           (instsp (instsp ";" instsp) (instp))
           (instp
                   ("dataequal" "=" atom)
                    ("model" atom "=" formula)
                    (output)
                   (atom))
           ;; (instp (dataeq)
           ;;        (output)
           ;;        (instp))
           ;; (model ("model" atom "=" atoms))
            (output ("output" instp)
                    ("output" "out" "=" instp))
           (instd ("do" instsd "end")
                  ("if" instd "then" instd)
                  ("else" instd)
                  (instd))
           (instm ("%do" instsm "%end")
                  ("%if" instm "%then" instm)
                  ("%else" instm)
                  (instm))
           (procrun ("proc" instsp "run"))
           (datarun ("data" instsd "run"))
           (macro ("%macro" instsm "%mend")))
         '((assoc ";")
           (assoc "=")))
        (smie-precs->prec2
         '((assoc ";")
           (assoc "&" "AND")
           (assoc "|" "OR")
           (nonassoc "IN")
           (nonassoc ">" "GT")
           (nonassoc ">=" "GE")
           (nonassoc "!=" "~=" "NE")
           (nonassoc "EQ")
           (nonassoc "<=" "LE")
           (nonassoc "<" "LT")
           (assoc "+" "-")
           (assoc "*" "/")
           (assoc "><" "MIN")
           (assoc "<>" "MAX")
           (nonassoc "~" "^" "NOT")    ;And unary "+" and "-".
           (right "**")))
       )))

(defun sas--data-token ()
  "choose beetween data ... run and data= ."
  (save-excursion
    (let ((tok2 (smie-default-forward-token)))
      (cond
       ((string= tok2 "=")
        "dataequal")
       ((string= tok2 "data")
        (if (string= (smie-default-forward-token) "=")
            "dataequal"
          "data"))
       (t "data")))))

(defun sas-forward-token ()
  (forward-comment (point-max))
  (let ((tok (smie-default-forward-token)))
        (cond
         ((equal tok "data") (sas--data-token))
         (t tok))))

(defun sas-backward-token ()
  (let ((tok (smie-default-backward-token)))
    (forward-comment (- (point)))
    (cond
     ;; ((and (equal tok "") (looking-at "\n"))
     ;;  (let ((pos (point)))
     ;;    (if (not (= 0 (mod (skip-chars-backward "\\\\") 2)))
     ;;        (sas-backward-token)
     ;;      (goto-char pos)
     ;;      tok)))
     ((equal tok "data") (sas--data-token))
     (t tok))))

(defun sas-smie-sample-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (pcase (list kind token)
    ;; From Octave-mode:
    ;; We could set smie-indent-basic instead, but that would have two
    ;; disadvantages:
    ;; - changes to sas-block-offset wouldn't take effect immediately.
    ;; - edebug wouldn't show the use of this variable.
    ('(:after "%macro") smie-indent-basic)
    ('(:after "proc") smie-indent-basic)
    ('(:after "data") smie-indent-basic)
    ('(:after "dataequal") 0)
    (`(:before . ,(or `"do" `"("))
     (if (smie-rule-hanging-p) (smie-rule-parent)))    ;; (message "token backward: %s" tok)
    ('(:after "run") (smie-rule-parent))
    ('(:after "%mend") (smie-rule-parent))
    ('(:elem arg) 1)))

;;;###autoload
(define-derived-mode sas-mode prog-mode "sas"
  "Major mode for editing SAS source. "
  :group 'sas-mode
   ;(setq-local indent-line-function #'sas-indent-line)
 (setq-local sentence-end ";[\t\n */]*"
              paragraph-start "^[ \t]*$"
              paragraph-separate "^[ \t]*$"
              paragraph-ignore-fill-prefix t
              adaptive-fill-mode nil
              comment-start "/*"
              comment-start-skip "/[*]"
              comment-end "*/"
              comment-end-skip "[*]/"
              comment-column 50
              smie-indent-basic 4)
  (set-syntax-table sas-mode-syntax-table)
  (smie-setup sas-smie-sample-grammar #'sas-smie-sample-rules
              :forward-token #'sas-forward-token
              :backward-token #'sas-backward-token)
  (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(sas-mode-font-lock-defaults nil t)))
(add-hook 'sas-mode-hook 'sas--initialize)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Ss][Aa][Ss]\\'" . sas-mode))

(provide 'sas)
;;; sas.el ends here
