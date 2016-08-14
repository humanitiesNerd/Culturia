;; The 'nil' configuration applies to all modes.
((nil . ((indent-tabs-mode . nil)
         (tab-width . 2)
         (eval . (progn
                   (put 'for-each-element-in-file 'scheme-indent-function 1)
                   (put 'test-check 'scheme-indent-function 1)
                   (put 'call-with-cursor 'scheme-indent-function 2)
                   (put 'with-cnx 'scheme-indent-function 1))))))
