(in-package #:egora-admin)

(defun login (username password url api)
  (let ((con (make-connection username password url api)))
    (password-login con)
    con))

(defun all-users-in-joined-rooms (connection)
  (let ((joined-rooms (joined-rooms connection)))
    (mapcar (lambda (id)
              (list id (members-in-room connection id)))
            (pkv joined-rooms :|joined_rooms|))))

(defun all-users-ids-in-joined-rooms (connection)
  (let ((joined-rooms (joined-rooms connection)))
    (mapcar (lambda (id)
              (list id (members-in-room-ids connection id)))
            (pkv joined-rooms :|joined_rooms|))))

(defun send-message-to-all-rooms (connection message)
  "Sends the message MESSAGE to all the rooms that CONNECTION is in."
  (let ((joined (pkv (joined-rooms connection)  :|joined_rooms| )))
    (mapcar (lambda (id)
              (send-message-to-room connection id message))
            joined)))

(defun ban-id-from-all-rooms-conn-is-in (connection id &optional (reason-why "Naughty naughty"))
  ()
