(in-package #:egora-admin)

(defun password-login-plist (connection)
  (list :|type| "m.login.password"
        :|identifier| (list :|type| "m.id.user"
                            :|user| (username connection))
        :|password| (password connection)))

(defun password-login (connection)
  "Takes a CONNECTION object and attempts to login."
  (auth-req (:post-no-auth connection ("login") (password-login-plist connection) resp)
    (setf (device-id connection) (pkv resp :|device_id|))
    (setf (auth connection) (make-instance 'auth :token (pkv resp :|access_token|)))
    (setf (logged-in-p connection) t)
    (sync connection)
    connection))

(defun public-rooms (connection)
  "Returns all the public rooms accessible by CONNECTION."
  (auth-req (:get connection ("publicRooms") nil resp)
    resp))

(defun sync (connection)
  "Gets the latest sync object from the server using CONNECTION."
  (let ((plist))
    (when (slot-boundp (status connection) 'latest-sync)
      (setf plist (list :|since| (pkv (latest-sync (status connection)) :|next_batch|))))
    (auth-req (:get connection ("sync") plist resp)
      (setf (latest-sync (status connection)) resp))))

(defun join-room (connection id)
  "Makes CONNECTION joined the room denoted by ID. Assuming it can."
  (auth-req (:post connection ("join/" id) (list :roomid id) resp)
    (push (second resp)  (current-rooms (status  connection)))))

(defun joined-rooms (connection &optional (destructive nil))
  "Returns the rooms that CONNECTION is within."
  (auth-req (:get connection ("joined_rooms") nil resp)
    (when destructive 
      (let ((c-r    (current-rooms (status connection)))
            (joined (pkv resp :|joined_rooms|)))
        (if (null c-r)
            (setf (current-rooms (status connection))
                  (mapcar (lambda (id) (list id nil)) joined))
            (let ((diff (set-difference (mapcar #'list joined) c-r
                                        :key #'car :test #'equal)))
              (when diff
                (setf (current-rooms (status connection))
                      (append c-r (mapcar (lambda (id) (list id nil)) joined))))))))
    resp))

(defun room-aliases (connection room-id)
  (auth-req (:get connection ("rooms/" room-id "/aliases") nil resp)
    resp));;I don't understand why this isn't working

(defun make-auth (connection)
  "Creates a plist which represents an auth token that can be sent to the server using data within
CONNECTION."
  (list :|auth| (list :|access_token| (token (auth connection)))))

(defun logout (connection)
  "Logs out CONNECTION."
  (auth-req (:post connection ("logout") nil resp)
    resp
    (setf (logged-in-p connection) nil)))

(defun send-message-to-room (connection room-id message)
  "Sends the message MESSAGE to the ROOM-ID, assuming CONNECTION is within it."
  (auth-req (:post connection ("/rooms/" room-id "/send/m.room.message")
                   (list :|msgtype| "m.text" :|body| message) resp)
    resp))

(defun kick-user-from-room (connection room-id user-id &optional (reason-why "kicked"))
  "Kicks the user denoted by USER-ID from ROOM-ID with the REASON-WHY."
  (auth-req (:post connection ("/rooms/" room-id "/kick")
                   (list :|user_id| user-id :|reason| reason-why) resp)
    resp))

(defun ban-user-from-room (connection room-id user-id &optional (reason-why "banned"))
  "Bans the user denoted by USER-ID from ROOM-ID with the REASON-WHY."
  (auth-req (:post connection ("/rooms/" room-id "/ban")
                   (list :|user_id| user-id :|reason| reason-why) resp)
    resp))

(defun unban-user-from-room (connection room-id user-id)
  "Unbans the user denoted by USER-ID from ROOM-ID."
  (auth-req (:post connection ("/rooms/" room-id "/ban")
                   (list :|user_id| user-id) resp)
    resp))

(defun members-in-room (connection room-id &optional (destructive nil))
  "Gets the members of ROOM-ID."
  (auth-req (:get connection ("/rooms/" room-id "/members")
                  nil resp)
    (when destructive
      (rplacd (assoc room-id (current-rooms (status connection)) :test #'equal)
              (rest resp)))
    resp))

(defun members-in-room-ids (connection room-id &optional (destructive nil))
  "Gets the members id's of ROOM-ID."
  (auth-req (:get connection ("/rooms/" room-id "/joined_members")
                  nil resp)
    (when destructive
      (rplacd (assoc room-id (current-rooms (status connection)) :test #'equal) resp))
    resp))

(defun admin-whois (connection user-id)
  "Performs a whois call on USER-ID, only an admin can call this on non self users."
  (auth-req (:post connection ("/admin/whois/" user-id)
                   nil resp)
    resp))

;; (defun messages-in-room (connection room-id)
;;   "Returns a list of message and state events for room denoted by ROOM-ID."
;;   (auth-req (:get connection ("rooms/" room-id "/messages")
;;                   (list :|from| "t17-34_16375_36_15_90_1_32_51_2" :|dir| "f"
;;                         :|to| "s61_0_0_0_0_0_0_0_0")
;;                   resp)
;;     resp))

