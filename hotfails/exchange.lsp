src/quicklisp/local-projects/scalpl/exchange.lisp:171:3:
  style-warning: 
    Item of type (MEMBER ASSET) can't be found using
         :key CLASS-OF which returns CLASS.
    --> PROGN SB-IMPL::%DEFUN SB-IMPL::%DEFUN
              SB-INT:NAMED-LAMBDA FUNCTION 
    --> BLOCK 
    ==>
      (REMOVE 'SCALPL.EXCHANGE:ASSET
              (MAPCAR 'CDR SCALPL.EXCHANGE::REGISTRY)
              :TEST-NOT 'SUBTYPEP :KEY 'CLASS-OF)
