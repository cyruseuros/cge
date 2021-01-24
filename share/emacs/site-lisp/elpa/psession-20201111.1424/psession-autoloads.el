;;; psession-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "psession" "psession.el" (0 0 0 0))
;;; Generated autoloads from psession.el

(autoload 'psession-make-persistent-variable "psession" "\
Make symbol variable VAR persistent with psession.

Do not make `minibuffer-history' variables persistent from here,
enable instead `psession-savehist-mode'.

Variable VAR is added to `psession-object-to-save-alist'.

When used interactively or SAVE is non nil, save VAR in
`psession-object-to-save-alist' with customize.

This function is meant to be used interactively, but
if called from elisp in e.g. -your init file- you don't need to specify
SAVE arg.

\(fn VAR &optional SAVE)" t nil)

(autoload 'psession-save-winconf "psession" "\
Save persistently current window config to PLACE.
Arg PLACE is the key of an entry in `psession--winconf-alist'.

\(fn PLACE)" t nil)

(autoload 'psession-restore-winconf "psession" "\
Restore window config CONF.
Arg CONF is an entry in `psession--winconf-alist'.

\(fn CONF)" t nil)

(autoload 'psession-delete-winconf "psession" "\
Delete window config CONF from `psession--winconf-alist'.

\(fn CONF)" t nil)

(defvar psession-savehist-mode nil "\
Non-nil if Psession-Savehist mode is enabled.
See the `psession-savehist-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `psession-savehist-mode'.")

(custom-autoload 'psession-savehist-mode "psession" nil)

(autoload 'psession-savehist-mode "psession" "\
Save minibuffer-history variables persistently.

If called interactively, enable Psession-Savehist mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar psession-autosave-mode nil "\
Non-nil if Psession-Autosave mode is enabled.
See the `psession-autosave-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `psession-autosave-mode'.")

(custom-autoload 'psession-autosave-mode "psession" nil)

(autoload 'psession-autosave-mode "psession" "\
Auto save emacs session when enabled.

If called interactively, enable Psession-Autosave mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar psession-mode nil "\
Non-nil if Psession mode is enabled.
See the `psession-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `psession-mode'.")

(custom-autoload 'psession-mode "psession" nil)

(autoload 'psession-mode "psession" "\
Persistent emacs sessions.

If called interactively, enable Psession mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psession" '("psession-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; psession-autoloads.el ends here
