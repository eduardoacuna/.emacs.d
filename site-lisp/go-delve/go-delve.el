(require 'indicators)
(require 'go-mode)
(require 'go-extra)

;; (setq proc (make-network-process :name "delve"
;;                                  :host 'local
;;                                  :service 8181))

;; (setq delve-response nil)

;; (defun update-delve-response (proc text)
;;   (setq delve-response text))

;; (set-process-filter proc 'update-delve-response)

;; (progn
;;   (process-send-string proc "{\"method\":\"RPCServer.FindLocation\",\"params\":[{\"Scope\":{\"GoroutineID\":-1,\"Frame\":0},\"Loc\":\"main.main\"}],\"id\":1}")
;;   (accept-process-output proc 1))

;; (setq delve-decoded-response
;;       (let ((json-object-type 'plist))
;;         (json-read-from-string delve-response)))


;; (json-encode '(:method "RPCServer.FindLocation" :params [(:Scope (:GoroutineID: -1 :Frame 0) :Loc "main.main")] :id 1))

(defvar go-delve-server-process nil
  "The current delve server process.")

(defvar go-delve-server-output nil
  "The delve server output.")

(defun go-delve-server-process-filter (process text)
  (setq go-delve-server-output text))

(defvar go-delve-client-process nil
  "The current delve client process.")

(defvar go-delve-last-response nil
  "The latest delve response.")

(defun go-delve-client-process-filter (process json-string)
  (setq go-delve-last-response (json-read-from-string json-string)))

(defun go-delve-debug-init ()
  (interactive)
  (unless (or go-delve-server-process go-delve-client-process)
    (let ((default-directory (go-extra-project-path)))
     (setq go-delve-server-process
           (start-process "delve" "delve-server" "dlv"
                          "debug" "--headless" "--api-version=2" "--log" "--listen=127.0.0.1:8181"))
     (set-process-filter go-delve-server-process 'go-delve-server-process-filter)
     (while (accept-process-output go-delve-server-process 5)
       (when (string-match-p "server listening at" go-delve-server-output)
         
         (setq go-delve-client-process
               (make-network-process :name "delve-client"
                                     :host 'local
                                     :service 8181))
         (setq go-delve-last-response nil)
         (set-process-filter go-delve-client-process 'go-delve-client-process-filter)))
     (unless go-delve-server-output
       (go-delve-debug-clear)
       (message "delve init failure")))))

(defun go-delve-debug-clear ()
  (interactive)
  (when go-delve-server-process
    (delete-process go-delve-server-process))
  (when go-delve-client-process
    (delete-process go-delve-client-process))
  (setq go-delve-server-process nil
        go-delve-client-process nil
        go-delve-last-response  nil))

(defun go-delve-debug-restart ()
  (interactive)
  (go-delve-debug-clear)
  (go-delve-debug-init))

(defun go-delve-send-message (msg)
  (interactive)
  (let ((json-object-type 'plist)
        (json-array-type  'vector))
    (process-send-string go-delve-client-process (json-encode msg))
    (when (accept-process-output go-delve-client-process 5)
      go-delve-last-response)))

;; (message (go-delve-send-message '(:method "RPCServer.FindLocation"
;;                                           :params ['(:Scope '(:GoroutineID -1 :Frame 0) :Loc "main.main")]
;;                                           :id 1)))

;; {"method":"RPCServer.FindLocation","params":[{"Scope":{"GoroutineID":-1,"Frame":0},"Loc":"main.main"}],"id":1}

(provide 'go-delve)
