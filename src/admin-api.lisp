(in-package #:egora-admin)

;;;admin API https://github.com/matrix-org/synapse/blob/master/docs/admin_api/user_admin_api.rst

(defun admin-query-user-account (connection user-id)
  "Queries the admin API. Uses CONNECTION to return information about USER-ID"
  (auth-req (:admin-get connection ("/_synapse/admin/v2/users/" user-id) nil resp )
    resp))

(defun admin-list-user-accounts (connection &optional (from "0") (limit "10") (guests "false"))
  "Queries the admin API. Uses CONNECTION to list user accounts."
  (check-type from string)
  (check-type limit string)
  (check-type guests string)
  (auth-req (:admin-get connection ("/_synapse/admin/v2/users?from="
                                    from "&limit=" limit "&guests=" guests)
                        nil resp)
    resp))

(defun admin-query-current-sessions-for-user (connection user-id)
  "Queries the admin API. Uses CONNECTION and returns information about USER-ID's sessions."
  (auth-req (:admin-get connection ("/_synapse/admin/v1/whois/" user-id) nil resp )
    resp))

(defun admin-deactivate-account (connection user-id)
  "Queries the admin API. Uses CONNECTION and deactivates USER-ID's account."
  (auth-req (:admin-post connection ("/_synapse/admin/v1/deactivate/" user-id) 
                         (list :|erase| t) resp)
    resp))

(defun admin-reset-password (connection user-id &optional (new-pass "newpass1") (logout-devices t))
  "Queries the admin API. Uses CONNECTION and resets USER-ID's password to NEW-PASS."
  (check-type new-pass string)
  (check-type logout-devices boolean)
  (auth-req (:admin-post connection ("/_synapse/admin/v1/reset_password/" user-id) 
                         (list :|new_password| new-pass :|logout_devices| logout-devices) resp)
    resp))

(defun admin-server-administrator (connection user-id)
  "Queries the admin API. Uses CONNECTION and returns whether USER-ID is an administrator."
  (auth-req (:admin-get connection ("/_synapse/admin/v1/users/" user-id "/admin") nil resp )
    resp))

(defun admin-change-user-to/from-administrator (connection user-id to/from)
  "Queries the admin API. Uses CONNECTION and deactivates USER-ID's account."
  (check-type to/from boolean)
  (auth-req (:admin-put connection ("/_synapse/admin/v1/users/" user-id "/admin")
                        (list :|admin| to/from) resp )
    resp))

(defun admin-create-or-modify-account (connection user-id
                                       &optional (password nil) (display-name nil)
                                         (avatar-url nil)
                                         (admin nil) (deactivated nil))
  "Queries the admin API. Uses CONNECTION and creates or modifies an account 
denoted by USER-ID.
user_id: fully-qualified user id: for example, @user:server.com

Body parameters:

    password, optional. If provided, the user's password is updated and all devices are logged out.
    displayname, optional, defaults to the value of user_id.
    threepids, optional, allows setting the third-party IDs (email, msisdn) belonging to a user.
    avatar_url, optional, must be a MXC URI.
    admin, optional, defaults to false.
    deactivated, optional, defaults to false.
 Threepids is current not supported"
  (let ((plist ()))
    (when password (setf plist (append (list :|password| password) plist)))
    (when display-name (setf plist (append (list :|displayname| display-name) plist)))
    (when avatar-url (setf plist (append (list :|avatar_url| avatar-url) plist)))
    (auth-req (:admin-put connection ("/_synapse/admin/v2/users/" user-id)
                          (append plist (list :|admin| admin :|deactivated| deactivated))
                          resp)
      resp)))

(defun admin-list-all-of-a-user-devices (connection user-id)
  "Queries the admin API. Uses CONNECTION and lists all the devices associated with USER-ID."
  (auth-req (:admin-get connection ("/_synapse/admin/v2/users/" user-id "/devices")
                        nil resp)
    resp))

(defun admin-delete-a-users-devices (connection user-id list-of-devices)
  "Queries the admin API. Uses CONNECTION and deletes the devices listed in LIST-OF-DEVICES
that are associated with USER-ID."
  (check-type list-of-devices list)
  (auth-req (:admin-post connection ("/_synapse/admin/v2/users/" user-id "/delete_devices") 
                         (list :|devices| list-of-devices) resp)
    resp))

(defun admin-show-a-users-device (connection user-id device-id) 
  "Queries the admin API. Uses CONNECTION and lists all the devices associated with USER-ID."
  (auth-req (:admin-get connection ("/_synapse/admin/v2/users/" user-id "/devices/" device-id)
                        nil resp)
    resp))

(defun admin-change-a-users-device-data (connection user-id device-id display-name)
  "Queries the admin API. Uses CONNECTION and changes the DISPLAY-NAME of DEVICE-ID associated with
USER-ID."
  (auth-req (:admin-put connection ("/_synapse/admin/v2/users/" user-id "/devices/" device-id)
                        (list :|display_name| display-name) resp )
    resp))


(defun admin-delete-a-device (connection user-id device-id) 
  "Queries the admin API. Uses CONNECTION and deletes the DEVICE-ID associated with USER-ID."
  (auth-req (:admin-delete connection ("/_synapse/admin/v2/users/" user-id "/devices/" device-id)
                           nil resp)
    resp))
