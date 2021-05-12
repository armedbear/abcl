(in-package :asdf)

(defclass bundle (jar-file)
  ;; bootdelegation and system-packages correspond to the framework
  ;; config vars org.osgi.framework.bootdelegation and
  ;; org.osgi.framework.system.packages.extra implementation is to
  ;; restart OSGI with the values appended to the existing
  ;; configuration in *osgi-configuration* I thought I understood the
  ;; difference but I don't - only system-packages has worked for me.
  ;; These should be lists of strings .  What these accomplish might
  ;; better be done with "extension bundles" but I haven't tried them
  ;; yet.
  ((bootdelegation :initarg :bootdelegation :initform nil)
   (system-packages :initarg :system-packages :initform nil)))

(defmethod perform ((operation load-op) (c bundle))
  (let ((extra-bootdelegation (slot-value c 'bootdelegation))
	(extra-system-packages (slot-value c 'system-packages)))
    (if (or extra-bootdelegation extra-system-packages)
	(warn "not handling :bootdelegation and :system-packages args yet"))
    (jss:add-bundle  (component-pathname c))))

(defmethod perform ((operation compile-op) (c bundle))
  (jss:add-bundle (component-pathname c)))

