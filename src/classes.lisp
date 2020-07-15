(in-package #:egora-admin)

(defclass connection ()
  ((logged-in-p
    :initform nil
    :accessor logged-in-p)
   (url
    :accessor url
    :initarg :url
    :type string)
   (api
    :accessor api
    :initarg :api
    :type string)
   (username
    :accessor username
    :type string
    :initarg :username)
   (password
    :accessor password
    :type string
    :initarg :password)
   (auth
    :accessor auth
    :type auth
    :initarg :auth)
   (device-id
    :accessor device-id
    :type string)))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type t :identity t)
    (format stream "~&URL: ~S~%Username: ~S~%Logged in: ~SAuth: ~S~%Device-id: ~S~%"
            (str:concat (url connection) (api connection))
            (username connection)
            (logged-in-p connection)
            (auth connection)
            (device-id connection))))

(defun make-connection (username password url api)
  (make-instance 'connection :username username :password password :url url :api api))

(defclass auth ()
  ((token
    :accessor token
    :initarg :token
    :type string)))

(define-condition api-error ()
  ((api-error-error
    :accessor api-error-error
    :initarg :api-error-error
    :type string)
   (api-error-code
    :accessor api-error-code
    :initarg :api-error-code
    :type string)
   (api-error-description
    :accessor api-error-description)))

(defmethod print-object ((api-error api-error) stream)
  (print-unreadable-object (api-error stream :type t :identity t)
    (format stream "~&Error code: ~S~%Error value: ~S~%Description: ~S~%"
            (api-error-code api-error)
            (api-error-error api-error)
            (api-error-description api-error))))

(define-condition m-forbidden (api-error)
  ((api-error-description
    :initform "Forbidden access, e.g. joining a room without permission, failed login.")))

(define-condition m-unknown-token (api-error)
  ((api-error-description
    :initform "The access token specified was not recognised.
An additional response parameter, soft_logout, might be present on the response for 401 HTTP status codes. See the soft logout section for more information.")))

(define-condition m-missing-token (api-error)
  ((api-error-description
    :initform
    "No access token was specified for the request.")))

(define-condition m-bad-json (api-error)
  ((api-error-description
    :initform
    "Request contained valid JSON, but it was malformed in some way, e.g. missing required keys, invalid values for keys.")))

(define-condition m-not-json (api-error)
  ((api-error-description
    :initform "Request did not contain valid JSON.")))

(define-condition m-not-found (api-error)
  ((api-error-description
    :initform "No resource was found for this request.")))

(define-condition m-limit-exceeded (api-error)
  ((api-error-description
    :initform "Too many requests have been sent in a short period of time. Wait a while then try again.")))

(define-condition m-unknown (api-error)
  ((api-error-description
    :initform "An unknown error has occurred.")))

(define-condition m-unrecognized (api-error)
  ((api-error-description
    :initform "The server did not understand the request.")))

(define-condition m-unauthorized (api-error)
  ((api-error-description
    :initform "The request was not correctly authorized. Usually due to login failures.")))


(defparameter *string->condition* (make-hash-table :test #'equal))

(defun add-string->condition (string condition-sym)
  (setf (gethash string *string->condition*) condition-sym))

(defun get-string->condition (string)
  (let ((condition (gethash string *string->condition*)))
    (unless condition
      (error (format nil "Condition for ~S not defined" string)))
    condition))

(add-string->condition "M_FORBIDDEN" 'm-forbidden)
(add-string->condition "M_UNKNOWN_TOKEN" 'm-unknown-token)
(add-string->condition "M_MISSING_TOKEN" 'm-missing-token)
(add-string->condition "M_BAD_JSON" 'm-bad-json)
(add-string->condition "M_NOT_JSON" 'm-not-json)
(add-string->condition "M_NOT_FOUND" 'm-not-found)
(add-string->condition "M_LIMIT_EXCEEDED" 'm-limit-exceeded)
(add-string->condition "M_UNKNOWN" 'm-unknown)
(add-string->condition "M_UNRECOGNIZED" 'm-unrecognized)
(add-string->condition "M_UNAUTHORIZED" 'm-unauthorized)

(defun signal-condition-from-response (response)
  (let* ((code (pkv response :|errcode|))
         (error-val (pkv response :|error|))
         (condition (get-string->condition code)))
    (error condition :api-error-code code :api-error-error error-val)))
