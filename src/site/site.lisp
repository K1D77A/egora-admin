(in-package #:egora-site)

(defun merge-directories (x y)
  "Merges x and y into a new pathname"
  (merge-pathnames
   (make-pathname :directory (list :relative y))
   x))

(defvar *page-hash* (make-hash-table :test #'equal))
(defparameter *server*  (make-instance 'hunchentoot:easy-acceptor
                                       :document-root (merge-directories (uiop:getcwd) "site")
                                       :port 4242 :name 'server))

(defun start-server ()
  (hunchentoot:start *server*))

(defun file-to-string (file)
  (check-type file (or string pathname))
  (let ((fi ""))
    (with-open-file (s file :if-does-not-exist :error)
      (loop :for line := (read-line s nil)
            :while line :do (setf fi (concatenate 'string fi
                                                  (format nil "~A~%" line)))))
    fi))



(defun add-page-from-string (string)
  (check-type string string)
  (add-page (intern string 'keyword)
            (lambda ()
              (file-to-string (concatenate 'string "." string)))))

(defun add-page (name func)
  (check-type name keyword)
  (check-type func function)
  (setf (gethash name *page-hash*) (funcall func)))

(defun get-page (name)
  (check-type name keyword)
  (gethash name *page-hash*))

(defun reload-page (key func)
  (check-type key keyword)
  (check-type func function)
  (add-page key func))

;; (add-page :milligram (lambda () (file-to-string "site/css/milligram.css")))
;; (add-page :jquery (lambda () (file-to-string "site/js/jquery-3.5.1.min.js")))

;; (defun reload-scripts ()
;;   (reload-page :scripts
;;                (lambda ()
;;                  (file-to-string "site/js/scripts.js"))))

;; (defun reload-main-css ()
;;   (reload-page :main-css
;;                (lambda ()
;;                  (file-to-string "site/css/main.css"))))

;; (defun reload-normalize-css ()
;;   (reload-page :normalize-css
;;                (lambda ()
;;                  (file-to-string "site/css/normalize.css"))))

;; (setf (ningle:route *app* "/css/normalize.css" :method :GET)
;;       (lambda (params)
;;         (declare (ignore params))
;;         (setf (lack.response:response-headers ningle:*response*)
;;               (append (lack.response:response-headers ningle:*response*)
;;                       (list :content-type "text/css")))
;;         (get-page :normalize-css)))

;; (setf (ningle:route *app* "/js/scripts.js" :method :GET)
;;       (lambda (params)
;;         (declare (ignore params))
;;         (setf (lack.response:response-headers ningle:*response*)
;;               (append (lack.response:response-headers ningle:*response*)
;;                       (list :content-type "application/javascript")))
;;         (reload-scripts)
;;         (get-page :scripts)))

;; (setf (ningle:route *app* "/js/jquery-3.5.1.min.js" :method :GET)
;;       (lambda (params)
;;         (declare (ignore params))
;;         (setf (lack.response:response-headers ningle:*response*)
;;               (append (lack.response:response-headers ningle:*response*)
;;                       (list :content-type "application/javascript")))
;;         (get-page :jquery)))

;; (setf (ningle:route *app* "/css/milligram.css" :method :GET)
;;       (lambda (params)
;;         (declare (ignore params))
;;         (setf (lack.response:response-headers ningle:*response*)
;;               (append (lack.response:response-headers ningle:*response*)
;;                       (list :content-type "text/css")))
;;         (get-page :milligram)))

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




;; (setf (ningle:route *app* "/" :method :GET)
;;       (lambda (params)
;;         (declare (ignore params))
;;         (lambda (env)
;;           (with-output-to-string (x)
;;             (let ((*standard-output* x))
;;               (main-page (getf env :lack.session))
;;               `(200 (:content-type "text/plain")
;;                     ,x))))))

;; (setf (ningle:route *app* "/login" :method :POST)
;;       (lambda (params)
;;         (declare (ignore params))
;;         (lambda (env)
;;           (let* ((params (getf env :body-parameters))
;;                  (user (alexandria:assoc-value params "username" :test #'string=))
;;                  (pass (alexandria:assoc-value params "password" :test #'string=))
;;                  (session (getf env :lack.session))
;;                  (con (handler-case
;;                           (egora-admin:login user pass egora-admin:*url* egora-admin:*api*)
;;                         (egora-admin:api-error ()
;;                           nil))))
;;             (if con
;;                 (progn (setf (gethash :login session)
;;                              user)
;;                        (setf *connection* con)
;;                        `(200 (:content-type "text/plain")
;;                              ,(format nil "logged in and session ~S created " session)))
;;                 `(200 (:content-type "text/plain")
;;                       "failed login"))))))


