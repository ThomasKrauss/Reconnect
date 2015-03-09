("Pretty much like with-standard-io-syntax but enforce downcase and pretty print. Symbols are thus printed in lower case and without their package names, if it is not needed."
 ("Example"
  (let ((sym 'test))
    (string= (with-output-to-string (s)
               (with-standard-io-syntax
                 (print sym s)))
             (format nil "~%~a::~s " (package-name (symbol-package sym)) sym)))
  (let ((sym 'test))
    (string= (with-output-to-string (s)
               (with-custom-io-syntax
                (print sym s)))
             (format nil "~%~(~s~) " sym)))))
                   
         