;; stx-smartvault.clar
;; A simple vault contract for STX + SIP-010 fungible tokens (template)
;; Contract name: stx-smartvault

;; Define SIP-010 trait
(define-trait ft-trait
    (
        ;; Transfer from the caller to a new principal
        (transfer (uint principal principal (optional (buff 34))) (response bool uint))
        ;; Get the token balance of the specified principal
        (get-balance (principal) (response uint uint))
    ))

(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_POSITIVE (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_PAUSED (err u103))
(define-constant ERR_INVALID_TOKEN (err u104))
(define-constant ERR_NO_DEPOSIT_DETECTED (err u105))

;; -- Admin and pause
(define-data-var admin principal tx-sender) ;; initially the deployer
(define-data-var paused bool false)

(define-read-only (get-admin)
  (ok (var-get admin)))

(define-read-only (is-paused)
  (ok (var-get paused)))

(define-private (only-admin)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (ok true)))

(define-public (set-admin (new-admin principal))
  (begin
    (try! (only-admin))
    (asserts! (not (is-eq new-admin tx-sender)) ERR_UNAUTHORIZED)  ;; Prevent setting self as admin
    (var-set admin new-admin)
    (ok true)))

(define-public (pause)
  (begin
    (try! (only-admin))
    (var-set paused true)
    (ok true)))

(define-public (unpause)
  (begin
    (try! (only-admin))
    (var-set paused false)
    (ok true)))

;; -- Storage maps
;; STX balances: key = (owner principal) -> balance uint
(define-map stx-balances
  {owner: principal}
  {balance: uint})

;; FT balances: key = (owner principal, token principal) -> balance uint
(define-map ft-balances
  {owner: principal, token: principal}
  {balance: uint})

;; -- Helpers
(define-read-only (get-stx-balance-of (owner principal))
  (match (map-get? stx-balances {owner: owner})
    balance-entry (ok (get balance balance-entry))
    (ok u0)))

(define-read-only (get-ft-balance-of (owner principal) (token <ft-trait>))
  (match (map-get? ft-balances {owner: owner, token: (contract-of token)})
    balance-entry (ok (get balance balance-entry))
    (ok u0)))

(define-private (add-stx-balance (owner principal) (amount uint))
  (let ((prev (default-to u0 (get balance (map-get? stx-balances {owner: owner})))))
    (map-set stx-balances {owner: owner} {balance: (+ prev amount)})
    (ok true)))

(define-private (sub-stx-balance (owner principal) (amount uint))
  (let ((prev (default-to u0 (get balance (map-get? stx-balances {owner: owner})))))
    (asserts! (>= prev amount) ERR_INSUFFICIENT_BALANCE)
    (map-set stx-balances {owner: owner} {balance: (- prev amount)})
    (ok true)))

(define-private (add-ft-balance (owner principal) (token <ft-trait>) (amount uint))
  (let ((prev (default-to u0 (get balance (map-get? ft-balances {owner: owner, token: (contract-of token)})))))
    (map-set ft-balances {owner: owner, token: (contract-of token)} {balance: (+ prev amount)})
    (ok true)))

(define-private (sub-ft-balance (owner principal) (token <ft-trait>) (amount uint))
  (let ((prev (default-to u0 (get balance (map-get? ft-balances {owner: owner, token: (contract-of token)})))))
    (asserts! (>= prev amount) ERR_INSUFFICIENT_BALANCE)
    (map-set ft-balances {owner: owner, token: (contract-of token)} {balance: (- prev amount)})
    (ok true)))

;; -----------------------------------------------------------------
;; STX deposit workflow (safe pattern)
;;
;; NOTE: Contract cannot "pull" STX from a user. Best practice:
;; 1) User sends STX to the contract address in a separate STX transfer transaction.
;; 2) Then user calls `credit-stx` specifying the expected amount. The contract reads its own STX balance
;;    (stx-get-balance) and credits the caller according to the delta observed since last bookkeeping.
;;
;; This two-step helps prevent replay/unknown-attachment problems.
;; -----------------------------------------------------------------

;; Read contract's STX balance (this built-in name may vary by environment)
(define-read-only (contract-stx-balance)
  ;; stx-get-balance returns balance of a principal; in many Clarity setups
  ;; the built-in is (stx-get-balance? <principal>) or (get-balance <principal>).
  ;; If your toolchain uses a different built-in name, replace below accordingly.
  (ok (stx-get-balance (as-contract tx-sender)))) ;; NOTE: intentionally checks contract principal

;; For safety we keep track of how much STX the contract has already accounted for total.
(define-map contract-accounted-stx
  {dummy: bool} ;; single-key map; bool key is just a workaround
  {amount: uint})

(define-read-only (get-contract-accounted-stx)
  (ok (default-to u0 (get amount (map-get? contract-accounted-stx {dummy: true})))))

(define-private (set-contract-accounted-stx (a uint))
  (ok (map-set contract-accounted-stx {dummy: true} {amount: a})))

;; Call this after you (externally) send STX to the contract address.
;; The function will compute the difference between current on-chain STX balance
;; of the contract and the previously-accounted total; it credits the caller up to that delta.
(define-public (credit-stx)
  (begin
    (if (var-get paused) (err ERR_PAUSED)
      (let (
            (contract-balance (stx-get-balance (as-contract tx-sender))) ;; current contract STX balance
            (accounted (default-to u0 (get amount (map-get? contract-accounted-stx {dummy: true}))))
           )
        (let ((delta (if (>= contract-balance accounted) (- contract-balance accounted) u0)))
          (if (<= delta u0)
              (err ERR_NO_DEPOSIT_DETECTED)
              (begin
                ;; credit the caller with delta
                (unwrap-panic (add-stx-balance tx-sender delta))
                ;; update accounted total
                (map-set contract-accounted-stx {dummy: true} {amount: contract-balance})
                (ok delta))))))))

;; Withdraw STX from vault to caller
(define-public (withdraw-stx (amount uint))
  (begin
    ;; First check if paused
    (asserts! (not (var-get paused)) ERR_PAUSED)
    ;; Check amount
    (asserts! (> amount u0) ERR_NOT_POSITIVE)
    ;; Now try to subtract balance
    (try! (sub-stx-balance tx-sender amount))
    ;; Try to transfer STX
    (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
    ;; Update contract's accounted balance
    (let ((contract-balance (stx-get-balance (as-contract tx-sender))))
      (map-set contract-accounted-stx {dummy: true} {amount: contract-balance})
      (ok amount))))

;; -----------------------------------------------------------------
;; Fungible Token (SIP-010) flow
;;
;; Safe pattern:
;; 1) User calls the token contract's 'transfer' to send tokens to this contract principal.
;; 2) Then user calls `credit-ft` to have the contract check the token balance received and credit their vault.
;;
;; Withdrawal uses ft-transfer? from contract to user (using as-contract).
;; -----------------------------------------------------------------

;; Note: This is now a public function since contract-call? can't be used in read-only functions
;; Note: This needs to be a public function since contract-call? can't be used in read-only functions
(define-public (contract-ft-balance (token <ft-trait>))
  ;; Direct contract-call? is considered safe since we're using a trait bound parameter
  (let ((balance-result (contract-call? token get-balance (as-contract tx-sender))))
    (match balance-result
      success (ok success)
      error (err ERR_INVALID_TOKEN))))

;; Credit FT: credits caller with delta (amount transferred to the contract since last accounted)
(define-public (credit-ft (token <ft-trait>))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (let ((balance-result (contract-call? token get-balance (as-contract tx-sender))))
      (asserts! (is-ok balance-result) ERR_INVALID_TOKEN)
      (let ((contract-balance (unwrap-panic balance-result))
            (prev (default-to u0 (get balance (map-get? ft-balances {owner: tx-sender, token: (contract-of token)})))))
        ;; Calculate the delta of new tokens received
        (let ((delta (if (>= contract-balance prev) (- contract-balance prev) u0)))
          (asserts! (> delta u0) ERR_NO_DEPOSIT_DETECTED)
          ;; Credit the user's balance
          (unwrap-panic (add-ft-balance tx-sender token delta))
          (ok delta))))))

;; Withdraw FT: sends tokens from contract to caller and decrements their balance
(define-public (withdraw-ft (token <ft-trait>) (amount uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (> amount u0) ERR_NOT_POSITIVE)
    (try! (sub-ft-balance tx-sender token amount))
    (let ((transfer-result (contract-call? token transfer amount (as-contract tx-sender) tx-sender none)))
      (asserts! (is-ok transfer-result) ERR_INVALID_TOKEN)
      (ok amount))))

;; -- View helper to check balances (STX + FT)
(define-read-only (vault-overview (owner principal) (token <ft-trait>))
  (let ((stx-balance (unwrap-panic (get-stx-balance-of owner)))
        (ft-balance (unwrap-panic (get-ft-balance-of owner token))))
    (ok {stx: stx-balance, ft: ft-balance})))

;; -- Misc administrative withdrawal (admin can withdraw tokens or STX by moving them out of contract)
(define-public (admin-withdraw-stx (to principal) (amount uint))
  (begin
    (try! (only-admin))
    (asserts! (> amount u0) ERR_NOT_POSITIVE)
    (let ((transfer-result (stx-transfer? amount (as-contract tx-sender) to)))
      (asserts! (is-ok transfer-result) ERR_INSUFFICIENT_BALANCE)
      (ok true))))

(define-public (admin-withdraw-ft (token <ft-trait>) (to principal) (amount uint))
  (begin
    (try! (only-admin))
    (asserts! (> amount u0) ERR_NOT_POSITIVE)
    (let ((transfer-result (contract-call? token transfer amount (as-contract tx-sender) to none)))
      (asserts! (is-ok transfer-result) ERR_INVALID_TOKEN)
      (ok true))))
