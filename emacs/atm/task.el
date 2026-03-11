;; -*- lexical-binding: t; -*-

(defgroup task nil
  "Task CLI support."
  :prefix "task/")

(defcustom task/buffer-name "*Task output*"
  "Name for the task output buffer."
  :type 'string
  :group 'task)

(defcustom task/binary-path "task"
  "Path to the task binary."
  :type 'string
  :group 'task)

(defcustom task/buffer-mode 'shell-mode
  "Major mode to use in the task buffer."
  :type 'symbol
  :group 'task)

(defun task/run (workdir task)
  "Runs a task."
  (interactive (list
                (if current-prefix-arg
                    (read-directory-name "Working directory: "))
                (read-string "Task to execute: ")))
  (let* ((task-buffer (get-buffer-create task/buffer-name))
         (dir (if workdir
                  (expand-file-name workdir)
                default-directory))
         (shell-command (format "cd %s && %s %s"
                                dir
                                task/binary-path
                                task)))
    (with-current-buffer task-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Invoking task command: %s %s\n\n" task/binary-path task))
      (setq buffer-read-only t)

      (display-buffer task-buffer)

      (setq default-directory dir)

      (funcall task/buffer-mode)
      (goto-char (point-min))
      (start-process "task" task-buffer "task" task))))

(provide 'task)
