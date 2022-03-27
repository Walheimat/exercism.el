;;; exercism.el --- Download and submit exercism exercises. -*- lexical-binding: t; -*-

;; Author: Walheimat <krister.schuchardt@gmail.com>
;; Maintainer: Walheimat <krister.schuchardt@gmail.com>
;; Keywords: exercism, learning, languages
;; Version: 0.0.1
;; Package-Requires ((emacs "27.1"))

;;; Commentary:

;; This package provides convenience functions for interacting with
;; exercism exercises using the exercism binary.

;;; Code:

(require 'json)

;;;; Customization:

(defgroup exercism nil
  "Convenience wrapper to interact with exercism."
  :group 'convenience)

(defcustom exercism-bin (executable-find "exercism")
  "The path to the exercism binary."
  :type 'string
  :group 'exercism)

(defcustom exercism-prefix nil
  "Prefix key to activate command map."
  :type 'key-sequence
  :group 'exercism)

(defcustom exercism-bind-command-map t
  "Whether to bind the command map automatically."
  :type 'boolean
  :group 'exercism)

;;;; Variables

(defconst exercism--submit-fmt "submit %s")
(defconst exercism--submit-desc "Submitting %s [%s]")
(defconst exercism--download-fmt "download --exercise=%s --track=%s")
(defconst exercism--download-desc "Downloading %s [%s]")
(defconst exercism--open-fmt "open %s")
(defconst exercism--open-desc "Opening %s [%s]")

(defconst exercism--dir ".exercism")
(defconst exercism--config-file "config.json")
(defconst exercism--metadata-file "metadata.json")

(defvar exercism--buffer-name "*exercism*")
(defvar exercism--process-name "exercism")

(defconst exercism--successful-download-string "Downloaded to")
(defvar exercism--last-downloaded nil)

;;;; Files:

(defun exercism--locate-project-root()
  "Locate the project root of the exercism project."
  (when-let ((rel (or buffer-file-name default-directory)))
    (locate-dominating-file rel exercism--dir)))

(defalias 'exercism--exercise-p 'exercism--locate-project-root)

(defun exercism--locate-exercism()
  "Locate the exercism directory in project."
  (concat
   (exercism--locate-project-root)
   exercism--dir))

(defun exercism--read-json (json-file)
  "Read JSON-FILE."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents json-file)
      (goto-char (point-min))
      (json-read))))

(defun exercism--find-file (file)
  "Find an exercism FILE."
  (expand-file-name file (exercism--locate-exercism)))

(defun exercism--read-config ()
  "Read the config file."
  (exercism--read-json (exercism--find-file exercism--config-file)))

(defun exercism--read-metadata ()
  "Read the metadata file."
  (exercism--read-json (exercism--find-file exercism--metadata-file)))

;;;; Data access:

(defun exercism--solution-files ()
  "Get the solutions files from the config."
  (when-let ((root (exercism--locate-project-root)))
    (seq-map
     (lambda (it) (expand-file-name it root))
     (cdr (assoc 'solution (assoc 'files (exercism--read-config)))))))

(defun exercism--current-track ()
  "Get the current track."
  (cdr (assoc 'track (exercism--read-metadata))))

(defun exercism--current-exercise ()
  "Get the current exercise's name."
  (cdr (assoc 'exercise (exercism--read-metadata))))

(defun exercism--current-url ()
  "Get the current exercise's URL."
  (cdr (assoc 'url (exercism--read-metadata))))

;;;; Utility:

(defun exercism--get-buffer ()
  "Get the exercism buffer."
  (get-buffer-create exercism--buffer-name))

(defun exercism--command (subcommand)
  "Get full command from SUBCOMMAND."
  (let ((bin (or exercism-bin (exercism--error "Executable not found"))))
    (concat bin " " subcommand)))

(defun exercism--notify (str)
  "Notify about STR."
  (let* ((replaced (replace-regexp-in-string "\\(\\ \\|\\\n\\)\\{2,\\}" " " str))
         (onelined (string-replace "\n" " " replaced))
         (cutoff (string-replace
                  (format "Process %s finished" exercism--process-name)
                  " "
                  onelined))
         (trimmed (string-trim cutoff)))
    (message trimmed)))

(defun exercism--error (&optional error)
  "Show user ERROR."
  (user-error (or error "Not in an exercism directory")))

;;;; Core:

(defun exercism--execute (subcommand &optional description)
  "Execute SUBCOMMAND.

Optionally use DESCRIPTION for progress indication."
  (with-current-buffer (exercism--get-buffer)
    (erase-buffer)
    (let* ((comm (exercism--command subcommand))
           (proc (start-process-shell-command exercism--process-name (current-buffer) comm))
           (rep (make-progress-reporter (or description "Exercising"))))
      (while (eq 'run (process-status proc))
        (sit-for 0.1)
        (progress-reporter-update rep))
      (exercism--notify (buffer-string))
      (buffer-string))))

(defun exercism--download-extract-path (string)
  "Extract the path to the downloaded exercise from STRING."
  (when (string-match exercism--successful-download-string string)
    (let* ((segments (split-string (string-trim string) "\n"))
           (path (nth 1 segments)))
      (setq exercism--last-downloaded path))))

(defun exercism--download (name track)
  "Download exercise NAME for TRACK."
  (let ((subc (format exercism--download-fmt name track))
        (descr (format exercism--download-desc name track)))
    (exercism--download-extract-path (exercism--execute subc descr))))

(defun exercism--submit ()
  "Submit the solutions."
  (let* ((files (string-join (exercism--solution-files) " "))
         (exercise (exercism--current-exercise))
         (track (exercism--current-track))
         (subc (format exercism--submit-fmt files))
         (descr (format exercism--submit-desc exercise track)))
    (exercism--execute subc descr)))

(defun exercism--open ()
  "Open exercise NAME."
  (let* ((url (exercism--current-url))
         (exercise (exercism--current-exercise))
         (track (exercism--current-track))
         (descr (format exercism--open-desc exercise track)))
    (if (browse-url-can-use-xdg-open)
        (progn
          (exercism--notify descr)
          (browse-url-xdg-open url))
      (exercism--error "Can't open URL"))))

;;;; Keymap:

(defvar exercism-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'exercism-download)
    (define-key map (kbd "s") #'exercism-submit)
    (define-key map (kbd "o") #'exercism-open)
    (define-key map (kbd "r") #'exercism-dired-recent)
    map))
(fset 'exercism-command-map exercism-command-map)

(when (and exercism-prefix exercism-bind-command-map)
  (global-set-key exercism-prefix 'exercism-command-map))

;;;; Public API:

;;;###autoload
(defun exercism-download (name &optional track)
  "Download exercise NAME for TRACK."
  (interactive (list (read-string "Name of the exercise: ")))
  (exercism--download name (or track
                               (read-string
                                "Name of the track: "
                                (exercism--current-track)))))

;;;###autoload
(defun exercism-dired-recent ()
  "Go to the directory of the last downloaded directory."
  (interactive)
  (if exercism--last-downloaded
      (pop-to-buffer-same-window (dired-noselect exercism--last-downloaded))
    (exercism--error "No recently downloaded exercise")))

;;;###autoload
(defun exercism-submit ()
  "Submit the solution."
  (interactive)
  (if (exercism--exercise-p)
      (exercism--submit)
    (exercism--error)))

;;;###autoload
(defun exercism-open ()
  "Open exercise."
  (interactive)
  (if (exercism--exercise-p)
      (exercism--open)
    (exercism--error)))

(provide 'exercism)

;;; exercism.el ends here
