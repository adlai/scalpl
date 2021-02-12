(defpackage #:scalpl.public-domain-utils
  (:use #:c2cl #:anaphora #:parse-float #:decimals
        #:string-case #:local-time #:split-sequence)
  (:export ; if these are complex enough that their documentation
   ;; string overflows into the format restandardizatation debate,
   ;; then they belong in some else's liquid utility exhaust pipe!
   ;;
   ;; mathematics
   #:icbrt
   #:shorten-uid
   #:dbz-guard
   #:string-octets #:octets
   ;; metaprogram
   #:once-only
   #:shallow-copy
   #:slot-reduce #:slot-reducer #:slot-setter #:aslot-setter
   #:aand1 #:awhen1 #:amvp1
   #:rehome-symbol
   #:rehome-class
   #:string-case
   #:break-errors
   #:kw #:mvwrap
   #:subseq*
   #:with-aslots
   #:lazy-do-instances
   ;; homicidally
   #:parse-price
   #:parse-float
   #:strftime
   ;; scubampfkhx
   #:concatenate-url-parameters
   #:urlencode-params
   ;; json
   #:read-json
   #:decode-json
   #:getjso
   #:mapjso
   #:mapcar-jso
   #:jso-keys
   #:with-json-slots
   #:mapjso*
   ))
