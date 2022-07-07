;;;; settings.lisp
;; 
(defpackage #:lw.settings
  (:documentation "Wrapper around LispWorks user-preference facility")
  (:use cl)
  (:add-use-defaults t)
  (:export
   make-settings
   company
   application-name
   application-version
   get-value
   set-value))

(in-package #:lw.settings)

(defconstant +product-root+ #+windows "Software" #-windows ".config"
  "Root path in the generic settings storage. On Windows it is a
registry root path, where the software name is located, and on
Linux it is a directory .config which follows the XDG Base Directory Specification")

(defconstant +settings-path+ "Settings"
  "Name of the root level of the settings, right next after the application name")

(defclass settings ()
  ((company :reader company
            :initarg :company
            :initform "com.github.fourier"
            :documentation "Company name")
   (name :reader application-name
         :initarg :application-name
         :initform "MediaImport"
         :documentation "Application name")
   (version :reader application-version
            :initarg :application-version
            :initform "1.0"
            :documentation "Application version")
   (product-symbol :reader product
    :documentation "A symbol produced from the company name"))
  (:documentation "Settings class provides application-specific persistence settings"))

(defmethod print-object ((self settings) out)
  "Print overload for the SETTINGS class"
  (print-unreadable-object (self out :type t)
    (format out "~%   Application name: ~s" (application-name self))
    (format out "~%   Company name: ~s" (company self))
    (format out "~%   Application version: ~s" (application-version self))))


(defmethod initialize-instance :after ((self settings) &key)
  "Constructor for SETTINGS class"
  (with-slots (company name version product-symbol) self
    ;; TODO: handle spaces and other symbols. Maybe replace
    ;; with dashes?
    (setf product-symbol (intern name "KEYWORD"))
    (setf (sys:product-registry-path product-symbol)
          (list +product-root+ company name version))))

(defun make-settings (application-name application-company &optional (application-version "1.0"))
  "Creates an instance of the settings class for persistent storage of the setttings, i.e.
in registry in Windows or in .config/ directory in Linux.
Parameters:
APPLICATION-NAME (string) the name of the application, i.e. MyBrowser
APPLICATION-COMPANY (string) the company or reverse url, i.e. com.example.my
APPLICATION-VERSION (string) the current version of the settings instance. Different versions
could co-exist."
  (check-type application-name string)
  (check-type application-company string)
  (check-type application-version string)
  (make-instance 'settings :application-name application-name
                 :application-version application-version
                 :company application-company))

(defmethod get-value ((self settings) key &optional fallback-value)
  "Get the value identified by KEY from the storage SELF.
If FALLBACK-VALUE specified, use this if not found (and update the storage)"
  (with-slots (product-symbol) self
    ;; handle paths like "Presets/Mypreset"
    (let ((path (split-sequence "/" key)))
      ;; if single key prepend with "Settings"
      (if (= 1 (length path))
          (setf path (list +settings-path+))
          ;; otherwise split the path and a key
          (setf key (car (last path))
                path (butlast path)))
      (multiple-value-bind (value result)
          (user-preference path key :product product-symbol)
        (cond ((and result value) (values value result))
              (fallback-value
               (progn
                 (setf (user-preference path key :product product-symbol) fallback-value)
                 (values (user-preference path key :product product-symbol) t)))
              (t (values nil nil)))))))


(defmethod set-value ((self settings) key value)
  "Set and save the VALUE identified by the KEY in storage SELF."
  (with-slots (product-symbol) self
    (let ((path (split-sequence "/" key)))
      ;; if single key prepend with "Settings"
      (if (= 1 (length path))
          (setf path (list +settings-path+))
          ;; otherwise split the path and a key
          (setf key (car (last path))
                path (butlast path)))
      (setf (user-preference path key :product product-symbol) value)
      value)))



