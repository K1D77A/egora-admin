;;;; egora-admin.lisp

(in-package #:egora-admin)

(in-package #:egora-admin)
(defparameter *url* "https://matrix.k1d77a.com")
(defparameter *api* "/_matrix/client/r0/")

(defparameter *conced* (str:concat *url* *api*))

(defconstant +content-type+ "application/json; charset=utf-8")

;;;I really need to add some better error reporting



;;there is also an optional discover information thingy but I'll ignore that for now

(defun make-connection (username password)
  (make-instance 'connection :username username :password password))

(defun plist-key-val (plist key)
  "gets the value associated with KEY in PLIST"
  (let ((pos (position key plist)))
    (if (integerp pos)
        (nth (1+ pos) plist);;slow but who cares
        nil)))

(defmacro pkv (plist key)
  `(plist-key-val ,plist ,key))

(defun post-r (url &optional (plist nil))
  (handler-case
      (if plist
          (dex:post url :headers `(("contentType" . ,+content-type+))
                        :content  (jojo:to-json plist :from :plist))
          (dex:post url :headers `(("contentType" . ,+content-type+))))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))

(defun post-api (url &rest encoder-and-its-args)
  (apply #'post-r (apply #'str:concat  url) encoder-and-its-args))

(defun get-api (url)
  (handler-case 
      (dex:get (apply #'str:concat url))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))



(defmacro with-token (connection token-name &body body)
  `(let ((,token-name (token (auth ,connection))))
     ,@body))

(defun tokenize (token &rest args)
  (append args (list "?access_token=") (list token)))

(defmacro auth-req (method connection url plist response-var &body body)
  (check-type url list)
  (let ((req (case method
               (:post 
                `(jojo:parse (post-api 
                              (tokenize (token (auth ,connection))
                                        (url ,connection) (api ,connection) ,@url) ,plist)))
               (:get
                `(jojo:parse (get-api (tokenize (token (auth ,connection))
                                                (url ,connection) (api ,connection)
                                                ,@url))))
               (otherwise (error "invalid HTTP method")))))

    `(let ((,response-var ,req))
       ,@body)))

(defun password-login-plist (connection)
  (list :|type| "m.login.password"
        :|identifier| (list :|type| "m.id.user"
                            :|user| (username connection))
        :|password| (password connection)))

(defun password-login (connection)
  "Takes a CONNECTION object and attempts to login."
  (auth-req :post connection ("login") (password-login-plist connection) resp
    (setf (device-id connection) (pkv resp :|device_id|))
    (setf (auth connection) (make-instance 'auth :token (pkv resp :|access_token|)))
    connection))

(defun public-rooms (connection)
  "Returns all the public rooms accessible by CONNECTION."
  (auth-req :get connection ("publicRooms") nil resp
    resp))

(defun sync (connection)
  "Gets the latest sync object from the server using CONNECTION."
  (auth-req :get connection ("sync") nil resp
    resp))

(defun join-room (connection id)
  "Makes CONNECTION joined the room denoted by ID. Assuming it can."
  (auth-req :post connection ("join/" id) (list :roomid id) resp
    resp))

(defun joined-rooms (connection)
  "Returns the rooms that CONNECTION is within."
  (auth-req :get connection ("joined_rooms") nil resp
    resp))

(defun make-auth (connection)
  "Creates a plist which represents an auth token that can be sent to the server using data within
CONNECTION."
  (list :|auth| (list :|access_token| (token (auth connection)))))

(defun logout (connection)
  "Logs out CONNECTION."
  (auth-req :post connection ("logout") nil resp
    resp))

(defun send-message-to-room (connection room-id message)
  "Sends the message MESSAGE to the ROOM-ID, assuming CONNECTION is within it."
  (auth-req :post connection ("/roomsd/" room-id "/send/m.room.message")
      (list :|msgtype| "m.text" :|body| message) resp
    resp))

(defun send-message-to-all-rooms (connection message)
  "Sends the message MESSAGE to all the rooms that CONNECTION is in."
  (let ((joined (pkv (joined-rooms connection)  :|joined_rooms| )))
    (mapcar (lambda (id)
              (send-message-to-room connection id message))
            joined)))

