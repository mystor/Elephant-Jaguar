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

SERVER = \"http://localhost:8000\"
")

(defconst ej-unlock-py (concat ej-header-py "
file = argv[2]
request = {'Key': file}

# Make a request to the unlock server, sending the key to unlock
requests.post(SERVER+\"/unlock\", data=json.dumps(request))
"))

(defconst ej-watch-py (concat ej-header-py "
file = sys.argv[2]

# Compute the hash
m = hashlib.sha1()
body = sys.stdin.read()
m.update(body)
hash = base64.b64encode(m.digest())


request = {'Key': file, 'Hash': hash}

res = json.loads(requests.post(SERVER+\"/watch\", data=json.dumps(request)).text)
if res['Locked']:
    print \"ReadOnly\"
else:
    print \"ReadWrit\"

print res['Target']['Data']
"))

(defconst ej-push-py (concat ej-header-py "
file = sys.argv[2]

m = hashlib.sha1()
body = sys.stdin.read()
m.update(body)

request = {
    'Hash': base64.b64encode(m.digest()),
    'Data': body
}

requests.post(SERVER+\"/push\", data=json.dumps(request))
"))

(defvar ej-timer nil)
(defvar ej-changed-list nil)
(defvar ej-locked-buffers nil)
(defvar ej-active-buffers nil)

(defun ej-invoke-py (code)
  "Invoke the given CODE in the python interpreter."
  (start-process "python" "*ej-log*" "python" (buffer-file-name) "-c" code))

(defun ej-mode-after-save ()
  "Unlock the file, and send the current contents to the server."
  (let ((proc (ej-invoke-py ej-unlock-py)))
    (process-send-region proc (point-min) (point-max))
    (remove-from-list 'ej-locked-buffers (current-buffer))))

(defun ej-sync ()
  "Ej Sync."
  ; Deal with any changed buffers
  (dolist (buff ej-changed-list)
    (add-to-list 'ej-locked-buffers buff)
    (with-current-buffer buff
      (let ((proc (ej-invoke-py ej-push-py)))
        (process-send-region-proc (point-min) (point-max)))))
  (setq ej-changed-list nil)

  ; Sync other buffers
  (dolist (buff ej-active-buffers)
    (unless (member buff ej-locked-buffers)
      (with-current-buffer buff
        (let ((proc (start-process "python" (concat "*" buff "-swap*") "python" (buffer-file-name)
                                   "-c" ej-watch-py)))
          (process-send-region-proc (point-min) (point-max))
          (when (accept-process-output proc 0.5)
            (save-excursion
              (clear-buffer)
              (with-current-buffer (concat "*" buff "-swap*")
                (append-to-buffer buff (point-min) (point-max))))))))))

(defun ej-mode-after-change (start end length)
  "Add the buffer to the ej-changed list.
Ignore START, END, LENGTH."
  (add-to-list ej-changed-list (current-buffer)))

(defun ej-mode ()
  "In ej-mode is a mode in which worlds."
  (interactive)
  (add-to-list ej-active-buffers (current-buffer))
  ; Listen for changes, and react to them
  (add-hook 'after-change-functions 'ej-mode-after-change nil t)
  ; Release locks when the file is saved
  (add-hook 'after-save-hook 'ej-mode-after-save nil t)
  ; Ensure that the timer is running
  (unless (timerp ej-timer)
    (setq ej-timer (run-with-timer 0.5 0.5 'ej-sync))))


(provide 'ej)
;;; ej.el ends here
