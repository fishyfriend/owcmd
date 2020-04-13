;;; owcmd.el --- Run a single command in the other window -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jacob First

;; Author: Jacob First <jacob.first@member.fsf.org>
;; Version: 0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: convenience
;; URL: https://github.com/fishyfriend/owcmd

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; owcmd.el provides a command to switch to the next window, run a
;; command, then switch back to the original window automatically.  Use
;; it to save keystrokes when you need to do just one thing in the other
;; window, like scroll the buffer, jump to a definition, paste some text,
;; or quit a temporary buffer.  It is a sort of generalization of the
;; built-in commands `scroll-other-window' and `scroll-other-window-down'.
;;
;; Installation:
;;
;; Put owcmd.el somewhere in your `load-path', then load it.
;;
;;   (require 'owcmd)
;;
;; Quickstart:
;;
;; To use the package, just bind the command `owcmd-execute-other-window'
;; to some key combination and you're ready to go.  No minor mode is
;; used.
;;
;;   (define-key global-map (kbd "C-c w") #'owcmd-execute-other-window)
;;
;; If you're using Evil, the following binding is recommended:
;;
;;   (evil-global-set-key 'normal (kbd "C-w C-e") #'owcmd-execute-other-window)
;;
;; Now every time you press the shortcut key, Emacs will temporarily
;; select the next window until you run another command.  You can press
;; the shortcut key multiple times to select a different target window.
;; Try this now, assuming you bound "C-c w" as above:
;;
;;   - Open a file containing at least 5 lines and move point to line 1.
;;   - Type "C-x 2" to split the current window.
;;   - Type "C-c w <down>" to move point down one line in the other
;;     window (i.e., the window just opened).
;;   - Type "C-c w C-u 3 <down>" to move point down three more lines in
;;     the other window.
;;   - Type "C-x 2" to split the first window again.
;;   - Type "C-c w" several times to cycle through the open windows, then
;;     type a movement key to move point in the selected window.  Do this
;;     again, this time selecting a different window.  Notice that the
;;     original window gets reselected each time.
;;
;; As this example shows, prefix arguments are supported by default.
;;
;; Configuration:
;;
;; You can configure the package's behavior through variables whose names
;; are prefixed with "owcmd-".

;;; Code:

(defcustom owcmd-cancel-reselect-on-new-window t
  "Don't reselect original window if the command selected a window."
  :group 'owcmd
  :type 'boolean)

(defcustom owcmd-cancel-reselect-commands '("^isearch-.*"
                                            "^query-replace\\(-regexp\\)?$"
                                            recentf-open-file
                                            recentf-open-files)
  "List of commands that cancel reselection of the original window.
Each item is a symbol or string; if a string, it will be interpreted as a
regexp.  This setting is applied by checking the current command name as
reported by `this-command' in a post-command hook."
  :group 'owcmd
  :type '(repeat (choice symbol regexp)))

(defcustom owcmd-ignore-commands '(owcmd-execute-other-window
                                   ;; god-mode compatibility
                                   god-mode-self-insert)
  "List of commands to ignore, checked just after a command completes.
Each item is a symbol or string; if a string, it will be interpreted as a
regexp.  Commands in this list will not trigger reselection of the
original window and will not prevent a subsequent command from doing so.
This setting is applied by checking the current command name as reported
by `this-command' in a post-command hook.

If you wrap `owcmd-execute-other-window' in another command, you should
add that command's name to `owcmd-ignore-commands'."
  :group 'owcmd
  :type '(repeat (choice symbol regexp)))

(defcustom owcmd-ignore-prefix-commands '(digit-argument
                                          negative-argument
                                          universal-argument-minus
                                          universal-argument-more
                                          universal-argument-other-key
                                          ;; evil-god-state compatibility
                                          evil-execute-in-god-state)
  "List of commands to ignore, checked just before any command is run.
Each item is a symbol or string; if a string, it will be interpreted as a
regexp.  Commands in this list will not trigger reselection of the
original window and will not prevent a subsequent command from doing so.
This setting is applied by checking the current command name as reported
by `this-command' in a pre-command hook.

Unsurprisingly, `owcmd-ignore-prefix-commands' is useful mainly for
ignoring prefix commands (e.g. `digit-argument'), which alter the values
of both `this-command' and `real-this-command' so that the actual command
name can't be checked in a post-command hook.  For ignoring other
commands that aren't prefix commands, you should generally use
`owcmd-ignore-commands'."
  :group 'owcmd
  :type '(repeat (choice symbol regexp)))

(defvar owcmd--ignore-prefix nil
  "Whether current command is an ignored prefix command.")

(defvar owcmd--calling-window nil
  "The selected window when `owcmd-execute-other-window' was called.")

(defvar owcmd--target-window nil
  "The window selected for running a command.")

(defun owcmd--match-in-list (symbol listvar)
  "Return t if SYMBOL is a match for any symbol or regexp in LISTVAR."
  (let* ((items listvar)
         (found nil))
    (while (and items (not found))
      (let ((item (car items)))
        (setq found
              (cond ((symbolp item) (eq symbol item))
                    ((stringp item) (string-match item (symbol-name symbol)))
                    (t nil))
              items
              (cdr items))))
    found))

(defun owcmd--handle-command-pre ()
  "Set up global state for handling prefix commands."
  (when (owcmd--match-in-list this-command owcmd-ignore-prefix-commands)
    (setq owcmd--ignore-prefix t)))

(defun owcmd--handle-command-post ()
  "Select original window if appropriate, and update global state."
  (if owcmd--ignore-prefix
      (setq owcmd--ignore-prefix nil)
    (unless (or (owcmd--match-in-list this-command owcmd-ignore-commands)
                (minibuffer-window-active-p (selected-window)))
      (when (and (not (owcmd--match-in-list this-command
                                          owcmd-cancel-reselect-commands))
                 (or (eq (selected-window) owcmd--target-window)
                     (not owcmd-cancel-reselect-on-new-window))
                 (window-live-p owcmd--calling-window))
        (select-window owcmd--calling-window))
      (owcmd--clean-up-state))))

(defun owcmd--clean-up-state ()
  "Return global environment to default state."
  (remove-hook 'pre-command-hook #'owcmd--handle-command-pre)
  (remove-hook 'post-command-hook #'owcmd--handle-command-post)
  (setq owcmd--calling-window nil
        owcmd--target-window nil
        owcmd--ignore-prefix nil))

;;;###autoload
(defun owcmd-execute-other-window ()
  "Switch to the next window, switching back after the next command.

Running `owcmd-execute-other-window' multiple times in sequence causes
the window selection to cycle through available windows, similar to what
`other-window' does.  The original window selection (i.e., the window
that was selected when `owcmd-execute-other-window' was run the first
time) is preserved and is reselected after any other command completes.

By default, reselection of the original window is suppressed after a
command other than `owcmd-execute-other-window' changes the window
selection.  This behavior is configurable using
`owcmd-cancel-reselect-on-new-window'.  Reselection is also suppressed if
the command just run is present in the list
`owcmd-cancel-reselect-commands'.

Ignoring commands:

`owcmd-execute-other-window' can ignore certain commands so that they
neither trigger reselection of the original window, nor end the current
excursion into the other window.  For example, one would usually like to
ignore a prefix argument and switch back to the original window only
after the command that receives the prefix argument has executed.  Two
configuration variables are provided for ignoring commands,
`owcmd-ignore-commands' and `owcmd-ignore-prefix-commands'.

The minibuffer is disregarded for purposes of determining the next
window.  Additionally, any commands that complete while the minibuffer
window is selected are ignored, just as though they were listed in
`owcmd-ignore-commands'.  (If this were not the case, commands like
`eval-expression' would not work properly because any minibuffer editing
would trigger reselection of the original window.)

If you wrap `owcmd-execute-other-window' in another command, you should
add that command's name to `owcmd-ignore-commands'.

Quirks:

`owcmd-execute-other-window' selects windows using `select-window' with
the NORECORD argument set to nil.  Thus, any actions that trigger on new
window selection are likely to be triggered when using
`owcmd-execute-other-window', both when the other window is selected and
again when the original window is reselected.

You may observe a 4s delay in returning to the original window after
executing a command via `execute-extended-command' (bound to \\<global-map>\\[execute-extended-command]).
If you type text or run any other command during the delay, the original
window will be reselected immediately, so this issue should not interrupt
your workflow.  (The odd behavior is due to an implementation quirk of
the way `execute-extended-command' displays suggested keybindings for the
command just run.  It is likely to be fixed in a future version of
Emacs.)"
  (interactive)
  (let ((target-window (next-window (selected-window) 'no-minibuffer)))
    (cond ((eq target-window owcmd--calling-window) ; user has cycled around
           (message "Execution in other window cancelled")
           (select-window target-window)
           (owcmd--clean-up-state))
          ((eq target-window (selected-window))
           (message "No other window")
           (owcmd--clean-up-state))
          (t
           (unless owcmd--calling-window
             (setq owcmd--calling-window (selected-window)))
           (setq owcmd--target-window target-window)
           (add-hook 'pre-command-hook #'owcmd--handle-command-pre)
           (add-hook 'post-command-hook #'owcmd--handle-command-post)
           (select-window target-window)
           (message "Executing next command in other window ...")))))

(provide 'owcmd)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; owcmd.el ends here
