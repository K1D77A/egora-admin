;;;; egora-admin.lisp

(in-package #:egora-admin)

(defparameter *url* "https://matrix.k1d77a.com")
(defparameter *api* "/_matrix/client/r0/")

(defparameter *conced* (str:concat *url* *api*))

(defconstant +content-type+ "application/json; charset=utf-8")

;;;I really need to add some better error reporting

(defparameter *url* "https://matrix.k1d77a.com")
(defparameter *api* "/_matrix/client/r0/")

;;there is also an optional discover information thingy but I'll ignore that for now


(defun plist-key-val (plist key)
  "Gets the value associated with KEY in PLIST."
  (let ((pos (position key plist)))
    (if (integerp pos)
        (nth (1+ pos) plist);;slow but who cares
        nil)))

(defmacro pkv (plist key)
  `(plist-key-val ,plist ,key))

;;;post requests 

(defun post-r (url &optional (plist nil))
  (handler-case
      (if plist
          (dex:post url :headers `(("contentType" . ,+content-type+))
                        :content  (jojo:to-json plist :from :plist))
          (dex:post url :headers `(("contentType" . ,+content-type+))))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))

(defun post-api (url &rest encoder-and-its-args)
  (apply #'post-r (apply #'str:concat url) encoder-and-its-args))

(defun admin-post (url token &optional (plist nil))
  (handler-case
      (if plist
          (dex:post url :headers `(("contentType" . ,+content-type+)
                                   ("Authorization" . ,(format nil "Bearer ~A" token)))
                        :content  (jojo:to-json plist :from :plist))
          (dex:post url :headers `(("contentType" . ,+content-type+)
                                   ("Authorization" . ,(format nil "Bearer ~A" token)))))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))


(defun admin-post-api (url token &rest plist)
  (apply #'admin-post (apply #'str:concat url) token plist))


;;;put requests 
(defun put-r (url plist)
  (handler-case
      (dex:put url :headers `(("contentType" . ,+content-type+))
                   :content  (jojo:to-json plist :from :plist))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))

(defun admin-put-r (url token plist)
  (handler-case
      (dex:put url :headers `(("contentType" . ,+content-type+)
                              ("Authorization" . ,(format nil "Bearer ~A" token)))
                   :content  (jojo:to-json plist :from :plist))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))

(defun put-api (url plist)
  (apply #'put-r (apply #'str:concat url) plist))

(defun admin-put-api (url token &rest plist)
  (apply #'admin-put-r (apply #'str:concat url) token plist))


;;;get requests

(defun get-api (url)
  (handler-case 
      (dex:get (apply #'str:concat url))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))

(defun admin-get-api (url token)
  (handler-case 
      (dex:get (apply #'str:concat url)
               :headers `(("Authorization" . ,(format nil "Bearer ~A" token))))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))

(defun admin-get (url token)
  (dex:get url :headers `(("Authorization" . ,(format nil "Bearer ~A" token)))))

;;;delete requests

(defun admin-delete-api (url token)
  (handler-case 
      (dex:delete (apply #'str:concat url)
                  :headers `(("Authorization" . ,(format nil "Bearer ~A" token))))
    (condition (c);;total catchall oh well
      (signal-condition-from-response (jojo:parse (dexador.error:response-body c))))))

(defun admin-delete (url token)
  (dex:delete url :headers `(("Authorization" . ,(format nil "Bearer ~A" token)))))

(defmacro with-token (connection token-name &body body)
  `(let ((,token-name (token (auth ,connection))))
     ,@body))

(defun tokenize (token &rest args)
  (append args (list "?access_token=") (list token)))

(defmacro auth-req ((method connection url plist response-var) &body body)
  (check-type url list)
  (let ((req (case method
               (:post-no-auth
                `(jojo:parse (post-api (list (url ,connection) (api ,connection) ,@url) ,plist)))
               (:post 
                `(jojo:parse (post-api 
                              (tokenize (token (auth ,connection))
                                        (url ,connection) (api ,connection) ,@url) ,plist)))
               (:get
                `(jojo:parse (get-api (tokenize (token (auth ,connection))
                                                (url ,connection) (api ,connection)
                                                ,@url))))
               (:admin-post
                `(jojo:parse (admin-post-api
                              (list (url ,connection) ,@url)
                              (token (auth ,connection)) ,plist)))
               (:admin-get
                `(jojo:parse (admin-get-api
                              (list (url ,connection) ,@url)
                              (token (auth ,connection)))))
               (:admin-put
                `(jojo:parse (admin-put-api
                              (list (url ,connection) ,@url)
                              (token (auth ,connection)) ,plist)))
               (:admin-delete
                `(jojo:parse (admin-delete-api
                              (list (url ,connection) ,@url)
                              (token (auth ,connection)))))
               (otherwise (error "invalid HTTP method")))))
    `(let ((,response-var ,req))
       ,@body)))




