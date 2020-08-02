(in-package #:egora-site)

(defun merge-directories (x y)
  "Merges x and y into a new pathname"
  (merge-pathnames
   (make-pathname :directory (list :relative y))
   x))

(defparameter *server*  (make-instance 'hunchentoot:easy-acceptor
                                       :document-root (merge-directories (uiop:getcwd) "site")
                                       :port 4242 :name 'server))

(defun start-server ()
  (hunchentoot:start *server*))


(define-easy-handler (css :uri "/css/main.css" :default-request-type :get)
    ()
  (setf (content-type*) "text/css")
  (lass:compile-and-write
   '(body :padding 0.5vw)
   '(.hidden :display none)
   '(.header
     :margin-left auto
     :margin-right auto
     :width 44%           
     )
   '(.content-container
     :border-style solid
     :border-color "#9B4DCA"
     :border-width 0.25vw 0.25vw
     :border-radius 0.5vw
     :padding-right 1vw
     :padding-left 1vw
     :padding-bottom 2vw
     :padding-top 1vw
     :display block
     :text-align center
     :margin-left auto
     :margin-right auto
     :margin-bottom 1vw
     :margin-top 1vw)
   '(.content-video
     :width auto
     :height 10vw)
   '(.info-container
     :padding auto
     :margin auto)
   '(.info-container
     (p
      :margin auto)))
  )

(defmacro with-page ((&key title) &body body)
`(spinneret:with-html
   (:doctype)
   (:html
    (:head
     (:title ,title)
     (:link :rel "application/javascript" :href "/js/jquery-3.5.1.min.js")
     (:link :rel "application/javascript" :href "/js/scripts.js")
     (:link :rel "stylesheet" :type "text/css" :href "/css/normalize.css")
     (:link :rel "stylesheet" :type "text/css" :href "/css/milligram.css")
     (:link :rel "stylesheet" :type "text/css" :href "/css/main.css"))
    (:body ,@body))))

(defun main-page ()
  (with-page (:title "main")
    (:h1 "login")
    (:div :id "form"
          (:form :id "login" :action "/login" :method "post"
                 (:fieldset
                  (:label :for "username-field")
                  (:input :name "username"
                          :placeholder "username"
                          :id "username-field"
                          :required "true")
                  (:label :for "password-field")
                  (:input :name "password"
                          :id "password-field"
                          :required "true"
                          :type "password")
                  (:input :type "submit"
                          :class "button-primary"
                          :value "Submit"))))))

(defun verified-session-p (session request)
  (and session
       (session-verify request)
       (not (hunchentoot:session-too-old-p session))))

(defmacro verifying-session (&body body)
  "will 403 if session fails to verify has an implicit progn"
  `(if (verified-session-p *session* *request*)
       (progn ,@body)
       (setf (return-code*) +http-forbidden+)))

(define-easy-handler (index :uri "/" :default-request-type :get)
    ()
  (with-output-to-string (x)
    (let ((*standard-output* x))
      (main-page))))

(defparameter *cons* (make-hash-table))

(defun hash-password (string)
  (sha-512 string))

(defun sha-512 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha512
                             (ironclad:ascii-string-to-byte-array str))))

(define-easy-handler (index :uri "/")
    ()
  :request-type :get
  (with-output-to-string (x)
    (let ((*standard-output* x))
      (main-page))))


(define-easy-handler (login :uri "/login" :default-request-type :POST)
    (username password)
  (let ((con
          (handler-case
              (egora-admin:login username password egora-admin:*url* egora-admin:*api*)
            (egora-admin:api-error ()
              nil))))
    (if con
        (let ((sess (start-session)))
          (setf (gethash (session-id sess) *cons*) con)
          (redirect "/admin"))
        (redirect "/"))))


(define-easy-handler (admin :uri "/admin")
    ()
  :request-type :get
  (verifying-session
    (with-output-to-string (x)
      (print-object *session* x))))

(define-easy-handler (logout :uri "/logout")
    ()
  :request-type :get
  (remove-session *session*)
  (egora-admin:logout (gethash (session-id *session*) *cons*))
  (redirect "/"))

