;;; package -- Summary
;;; Commentary:
;;; Live shared code editing across editors and computers!
;;; Code:

(defconst ej-header-py "
import hashlib
import requests
import os
import json
import base64
import sys

SERVER = \"http://localhost:8000\"
file = sys.argv[1]
")

(defconst ej-unlock-py (concat ej-header-py "
request = {
    'Key': file
}

# Make a request to the unlock server, sending the key to unlock
print \"Unlock Response:\", requests.post(SERVER+\"/unlock\", data=json.dumps(request)).text
"))

(defconst ej-watch-py (concat ej-header-py "
# Compute the hash
m = hashlib.sha1()
body = sys.stdin.read()
m.update(body)
hash = base64.b64encode(m.digest())

request = {
    'Key': file,
    'Target': { 'Hash': hash }
}

res = json.loads(requests.post(SERVER+\"/watch\", data=json.dumps(request)).text)
if res['Locked']:
    print \"RO\"
    print res['Target']['Data']
else:
    print \"RW\"
"))

(defconst ej-push-py (concat ej-header-py "
print \"Push Start\"
m = hashlib.sha1()
body = sys.stdin.read()
m.update(body)

request = {
    'Key': file,
    'Updated': {
        'Hash': base64.b64encode(m.digest()),
        'Data': body
    }
}
print \"Send Push!\"
print \"Push Response:\", requests.post(SERVER+\"/push\", data=json.dumps(request)).text
"))

(defvar ej-timer nil)
(defvar ej-changed-list nil)
(defvar ej-locked-buffers nil)
(defvar ej-active-buffers nil)

(defun ej-invoke-py (outbuffer code)
  "Outputs to OUTBUFFER.  Invoke the given CODE in the python interpreter."
  (start-process "python" outbuffer "python" "-c" code
                 (file-name-nondirectory (buffer-file-name))))

(defun ej-mode-after-save ()
  "Unlock the file."
  (ej-invoke-py "*ej-log*" ej-unlock-py)
  (setq ej-changed-list
        (remove (current-buffer) ej-changed-list))
  (setq ej-locked-buffers
        (remove (current-buffer) ej-locked-buffers)))

(defun ej-sync ()
  "Pushed change in buffers to the server & lock with push messages.
Read changes in unlocked buffers and update buffer."
  ; Deal with any changed buffers
  (dolist (buff ej-changed-list)
    (add-to-list 'ej-locked-buffers buff) ; The buffer is now locked
    (with-current-buffer buff
      (let ((proc (ej-invoke-py "*ej-log*" ej-push-py)))
        ; Send the entire buffer
        (process-send-region proc (point-min) (point-max))
        (process-send-eof proc))))

  ; Sync other buffers
  (dolist (buff ej-active-buffers)
    (unless (member buff ej-locked-buffers) ; TODO(michael): Don't update hidden buffers
      (with-current-buffer buff
        (let* ((tbuff (get-buffer-create (concat "*" (buffer-name buff) "-ej-swap*")))
               (__ignore__ (with-current-buffer tbuff (erase-buffer)))
               (proc (ej-invoke-py tbuff ej-watch-py)))
          ; Send the entire buffer
          (process-send-region proc (point-min) (point-max))
          (process-send-eof proc)

          ; Wait for the process to complete
          (when (accept-process-output proc 0.5)
            (save-excursion ; TODO(michael): Should move pointer back to position after chng
              (if (with-current-buffer tbuff
                    (goto-char (point-min))
                    (looking-at "RO\n"))
                  (progn ; Read only
                    ; Copy the temp buffer into the current buffer
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (goto-char (point-min))
                      (insert-buffer-substring tbuff 4)) ; TODO(michael): This might not work

                    (unless buffer-read-only (read-only-mode)))
                (progn ; Read/Write
                  (when buffer-read-only (message "Rw") (setq buffer-read-only nil))))))))))

  (setq ej-changed-list nil))

(defun ej-mode-after-change (start end length)
  "Add the buffer to the ej-changed list.
Ignore START, END, LENGTH."
  (add-to-list 'ej-changed-list (current-buffer)))

(defun ej-mode ()
  "In ej-mode is a mode in which worlds."
  (interactive)
  (add-to-list 'ej-active-buffers (current-buffer))
  ; Listen for changes, and react to them
  (add-hook 'after-change-functions 'ej-mode-after-change nil t)
  ; Release locks when the file is saved
  (add-hook 'after-save-hook 'ej-mode-after-save nil t)
  ; Ensure that the timer is running
  (unless (timerp ej-timer)
    (setq ej-timer (run-with-timer 0.5 0.5 'ej-sync))))


(provide 'ej)
;;; ej.el ends here
