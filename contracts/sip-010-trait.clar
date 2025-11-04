;; SIP-010 trait
(define-trait sip-010-trait
    (
        ;; Transfer from the caller to a new principal
        (transfer (uint principal principal (optional (buff 34))) (response bool uint))
        ;; Get the token balance of the specified principal
        (get-balance (principal) (response uint uint))
    ))