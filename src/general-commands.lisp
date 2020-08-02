(in-package #:egora-admin)

(defun login (username password url api)
  (let ((con (make-connection username password url api)))
    (password-login con)
    (joined-rooms con t)
    (all-users-in-joined-rooms con t)
    con))

(defun all-users-in-joined-rooms (connection &optional (destructive nil))
  (mapcar (lambda (alist)
            (list (first alist) (members-in-room connection (first alist) destructive)))
          (current-rooms (status connection))))

(defun all-users-ids-in-joined-rooms (connection &optional (destructive nil))
  (mapcar (lambda (alist)
            (list (first alist) (members-in-room-ids connection (first alist) destructive)))
          (current-rooms (status connection))))

(defun send-message-to-all-rooms (connection message)
  "Sends the message MESSAGE to all the rooms that CONNECTION is in."
  (mapcar (lambda (alist)
            (send-message-to-room connection (first alist) message))
          (current-rooms (status connection))))



