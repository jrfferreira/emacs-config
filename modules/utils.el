;;; Collection of useful functions for the config
(require 'cl)

(defun utils-arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
	 (mapconcat
	  (lambda (x) (format "%s%s%s" quote x quote))
	  (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

(defun utils-kill-current-buffer ()
  "Kill the active buffer"
  (interactive)
  (kill-buffer (current-buffer))
  )

(defun utils-open-user-init-file ()
  "Open the `user-init-file` in another screen"
  (interactive)
  (find-file-other-window user-init-file)
  )

(defun utils-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)))


(defun utils-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun utils-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun utils-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun utils-window-focus ()
  "Close other windows to focus on this one. Activate again to undo this."
  (interactive)
  (if (and (one-window-p)
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defun utils-kill-other-buffers ()
  "Kill all buffers except current one and toolkit (*Messages* and *dashboard*). Close other windows."
  (interactive)
  (mapc 'kill-buffer (remove-if
                       (lambda (x)
                         (or
                           (string-equal (buffer-name) (buffer-name x))
                           (string-equal "*Messages*" (buffer-name x))
                           (string-equal "*dashboard*" (buffer-name x))))
                       (buffer-list)))
  (delete-other-windows))

(provide 'utils)
