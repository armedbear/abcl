(defparameter *bank-account-impl*
  (let ((balance 1000))
    (jinterface-implementation
     "BankAccount"

     "getBalance" 
       (lambda ()
         balance)
     "deposit" 
        (lambda (amount) 
          (let ((amount (jobject-lisp-value amount)))
            (setf balance (+ balance amount))))
     "withdraw" 
       (lambda (amount)
         (let ((amount (jobject-lisp-value amount)))
           (setf balance (- balance amount)))))))

(defun get-bank-account-impl () 
  *bank-account-impl*)
