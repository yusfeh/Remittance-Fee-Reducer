;; title: Remittance-Fee-Reducer
;; version: 1.0.0
;; summary: P2P remittance system reducing fees by avoiding traditional banking
;; description: Enables direct peer-to-peer money transfers with escrow protection and minimal fees

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_TRANSFER_NOT_FOUND (err u102))
(define-constant ERR_TRANSFER_ALREADY_COMPLETED (err u103))
(define-constant ERR_TRANSFER_EXPIRED (err u104))
(define-constant ERR_INVALID_RECIPIENT (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_TRANSFER_NOT_READY (err u107))
(define-constant ERR_AGENT_NOT_FOUND (err u108))
(define-constant ERR_ALREADY_REGISTERED (err u109))
(define-constant ERR_INVALID_EXCHANGE_RATE (err u110))
(define-constant ERR_INSUFFICIENT_LIQUIDITY (err u111))
(define-constant ERR_CURRENCY_NOT_SUPPORTED (err u112))
(define-constant ERR_EXCHANGE_SLIPPAGE (err u113))

(define-constant MIN_TRANSFER_AMOUNT u1000000)
(define-constant MAX_TRANSFER_AMOUNT u100000000000)
(define-constant BASE_FEE_RATE u250)
(define-constant AGENT_COMMISSION_RATE u100)
(define-constant ESCROW_TIMEOUT_BLOCKS u1008)
(define-constant EXCHANGE_FEE_RATE u50)
(define-constant MAX_SLIPPAGE_RATE u500)
(define-constant MIN_LIQUIDITY_THRESHOLD u100000000)

(define-data-var next-transfer-id uint u1)
(define-data-var contract-paused bool false)
(define-data-var total-volume uint u0)
(define-data-var total-fees-collected uint u0)
(define-data-var next-exchange-id uint u1)

(define-map transfers uint {
    sender: principal,
    recipient: principal,
    amount: uint,
    fee: uint,
    agent: (optional principal),
    status: (string-ascii 20),
    created-at: uint,
    expires-at: uint,
    completion-code: (optional (string-ascii 32)),
    exchange-rate: (optional uint)
})

(define-map user-profiles principal {
    total-sent: uint,
    total-received: uint,
    reputation-score: uint,
    registration-block: uint,
    kyc-verified: bool
})

(define-map agents principal {
    commission-rate: uint,
    total-volume: uint,
    active: bool,
    supported-currencies: (list 10 (string-ascii 3)),
    reputation: uint
})

(define-map exchange-rates (string-ascii 6) uint)

(define-map pending-disputes uint {
    transfer-id: uint,
    disputer: principal,
    reason: (string-ascii 256),
    created-at: uint
})

(define-map currency-pools (string-ascii 3) {
    total-liquidity: uint,
    available-liquidity: uint,
    exchange-volume: uint,
    last-updated: uint,
    active: bool
})

(define-map exchange-transactions uint {
    sender: principal,
    recipient: principal,
    source-amount: uint,
    target-amount: uint,
    source-currency: (string-ascii 3),
    target-currency: (string-ascii 3),
    exchange-rate-used: uint,
    exchange-fee: uint,
    created-at: uint,
    status: (string-ascii 20)
})

(define-public (register-user (kyc-verified bool))
    (let ((user tx-sender))
        (asserts! (is-none (map-get? user-profiles user)) ERR_ALREADY_REGISTERED)
        (map-set user-profiles user {
            total-sent: u0,
            total-received: u0,
            reputation-score: u100,
            registration-block: stacks-block-height,
            kyc-verified: kyc-verified
        })
        (ok true)
    )
)

(define-public (register-agent (commission-rate uint) (currencies (list 10 (string-ascii 3))))
    (let ((agent tx-sender))
        (asserts! (<= commission-rate u500) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? agents agent)) ERR_ALREADY_REGISTERED)
        (map-set agents agent {
            commission-rate: commission-rate,
            total-volume: u0,
            active: true,
            supported-currencies: currencies,
            reputation: u100
        })
        (ok true)
    )
)

(define-public (update-exchange-rate (currency-pair (string-ascii 6)) (rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> rate u0) ERR_INVALID_EXCHANGE_RATE)
        (map-set exchange-rates currency-pair rate)
        (ok true)
    )
)

(define-public (initiate-transfer (recipient principal) (amount uint) (agent (optional principal)))
    (let (
        (sender tx-sender)
        (transfer-id (var-get next-transfer-id))
        (calculated-fee (calculate-fee amount))
        (total-cost (+ amount calculated-fee))
        (current-block stacks-block-height)
    )
        (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq sender recipient)) ERR_INVALID_RECIPIENT)
        (asserts! (and (>= amount MIN_TRANSFER_AMOUNT) (<= amount MAX_TRANSFER_AMOUNT)) ERR_INVALID_AMOUNT)
        (asserts! (>= (stx-get-balance sender) total-cost) ERR_INSUFFICIENT_BALANCE)
        
        (match agent
            some-agent (asserts! (get active (default-to {commission-rate: u0, total-volume: u0, active: false, supported-currencies: (list), reputation: u0} (map-get? agents some-agent))) ERR_AGENT_NOT_FOUND)
            true
        )
        
        (try! (stx-transfer? total-cost sender (as-contract tx-sender)))
        
        (map-set transfers transfer-id {
            sender: sender,
            recipient: recipient,
            amount: amount,
            fee: calculated-fee,
            agent: agent,
            status: "pending",
            created-at: current-block,
            expires-at: (+ current-block ESCROW_TIMEOUT_BLOCKS),
            completion-code: none,
            exchange-rate: none
        })
        
        (var-set next-transfer-id (+ transfer-id u1))
        (var-set total-volume (+ (var-get total-volume) amount))
        (var-set total-fees-collected (+ (var-get total-fees-collected) calculated-fee))
        
        (ok transfer-id)
    )
)

(define-public (complete-transfer (transfer-id uint) (completion-code (string-ascii 32)))
    (let (
        (transfer-data (unwrap! (map-get? transfers transfer-id) ERR_TRANSFER_NOT_FOUND))
        (sender (get sender transfer-data))
        (recipient (get recipient transfer-data))
        (amount (get amount transfer-data))
        (fee (get fee transfer-data))
        (agent (get agent transfer-data))
        (current-block stacks-block-height)
    )
        (asserts! (is-eq tx-sender recipient) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status transfer-data) "pending") ERR_TRANSFER_ALREADY_COMPLETED)
        (asserts! (< current-block (get expires-at transfer-data)) ERR_TRANSFER_EXPIRED)
        
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        
        (match agent
            some-agent (let ((agent-commission (/ (* fee AGENT_COMMISSION_RATE) u1000)))
                (try! (as-contract (stx-transfer? agent-commission tx-sender some-agent)))
                (map-set agents some-agent 
                    (merge (default-to {commission-rate: u0, total-volume: u0, active: false, supported-currencies: (list), reputation: u0} (map-get? agents some-agent))
                           {total-volume: (+ (get total-volume (default-to {commission-rate: u0, total-volume: u0, active: false, supported-currencies: (list), reputation: u0} (map-get? agents some-agent))) amount)}))
            )
            true
        )
        
        (map-set transfers transfer-id 
            (merge transfer-data {
                status: "completed",
                completion-code: (some completion-code)
            })
        )
        
        (update-user-stats sender recipient amount)
        (ok true)
    )
)

(define-public (cancel-transfer (transfer-id uint))
    (let (
        (transfer-data (unwrap! (map-get? transfers transfer-id) ERR_TRANSFER_NOT_FOUND))
        (sender (get sender transfer-data))
        (amount (get amount transfer-data))
        (fee (get fee transfer-data))
        (current-block stacks-block-height)
    )
        (asserts! (or (is-eq tx-sender sender) (>= current-block (get expires-at transfer-data))) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status transfer-data) "pending") ERR_TRANSFER_ALREADY_COMPLETED)
        
        (try! (as-contract (stx-transfer? (+ amount fee) tx-sender sender)))
        
        (map-set transfers transfer-id 
            (merge transfer-data {status: "cancelled"})
        )
        
        (ok true)
    )
)

(define-public (create-dispute (transfer-id uint) (reason (string-ascii 256)))
    (let (
        (transfer-data (unwrap! (map-get? transfers transfer-id) ERR_TRANSFER_NOT_FOUND))
        (current-block stacks-block-height)
    )
        (asserts! (or (is-eq tx-sender (get sender transfer-data)) (is-eq tx-sender (get recipient transfer-data))) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status transfer-data) "pending") ERR_TRANSFER_NOT_READY)
        
        (map-set pending-disputes transfer-id {
            transfer-id: transfer-id,
            disputer: tx-sender,
            reason: reason,
            created-at: current-block
        })
        
        (ok true)
    )
)

(define-public (resolve-dispute (transfer-id uint) (refund-to-sender bool))
    (let (
        (transfer-data (unwrap! (map-get? transfers transfer-id) ERR_TRANSFER_NOT_FOUND))
        (dispute-data (unwrap! (map-get? pending-disputes transfer-id) ERR_TRANSFER_NOT_FOUND))
        (sender (get sender transfer-data))
        (recipient (get recipient transfer-data))
        (amount (get amount transfer-data))
        (fee (get fee transfer-data))
    )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        (if refund-to-sender
            (try! (as-contract (stx-transfer? (+ amount fee) tx-sender sender)))
            (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        )
        
        (map-set transfers transfer-id 
            (merge transfer-data {status: "resolved"})
        )
        
        (map-delete pending-disputes transfer-id)
        (ok true)
    )
)

(define-public (withdraw-fees)
    (let ((fees-available (stx-get-balance (as-contract tx-sender))))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> fees-available u0) ERR_INSUFFICIENT_BALANCE)
        (try! (as-contract (stx-transfer? fees-available tx-sender CONTRACT_OWNER)))
        (ok fees-available)
    )
)

(define-public (pause-contract (paused bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused paused)
        (ok true)
    )
)

(define-public (update-agent-status (active bool))
    (let ((agent tx-sender))
        (asserts! (is-some (map-get? agents agent)) ERR_AGENT_NOT_FOUND)
        (map-set agents agent 
            (merge (unwrap-panic (map-get? agents agent)) {active: active})
        )
        (ok true)
    )
)

(define-read-only (get-transfer (transfer-id uint))
    (map-get? transfers transfer-id)
)

(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user)
)

(define-read-only (get-agent (agent principal))
    (map-get? agents agent)
)

(define-read-only (get-exchange-rate (currency-pair (string-ascii 6)))
    (map-get? exchange-rates currency-pair)
)

(define-read-only (calculate-fee (amount uint))
    (let ((base-fee (/ (* amount BASE_FEE_RATE) u10000)))
        (if (< base-fee u10000) u10000 base-fee)
    )
)

(define-read-only (get-transfer-cost (amount uint))
    (+ amount (calculate-fee amount))
)

(define-read-only (get-contract-stats)
    {
        total-transfers: (- (var-get next-transfer-id) u1),
        total-volume: (var-get total-volume),
        total-fees: (var-get total-fees-collected),
        contract-balance: (stx-get-balance (as-contract tx-sender)),
        is-paused: (var-get contract-paused)
    }
)

(define-read-only (get-user-transfers (user principal) (limit uint))
    (let ((max-id (var-get next-transfer-id)))
        (ok "User transfers query requires off-chain indexing")
    )
)

(define-read-only (get-transfer-if-user-involved (offset uint))
    (let ((transfer-id (- (var-get next-transfer-id) offset)))
        (match (map-get? transfers transfer-id)
            some-transfer (if (or (is-eq tx-sender (get sender some-transfer)) 
                                 (is-eq tx-sender (get recipient some-transfer)))
                            (some {id: transfer-id, data: some-transfer})
                            none)
            none
        )
    )
)

(define-read-only (estimate-transfer-time (amount uint) (agent (optional principal)))
    (let ((base-time u6))
        (match agent
        some-agent (if (>= (get reputation (default-to {commission-rate: u0, total-volume: u0, active: false, supported-currencies: (list), reputation: u0} (map-get? agents some-agent))) u150)
        (/ base-time u2)
        base-time)
        (* base-time u2)
        )
    )
)

(define-read-only (get-active-agents)
    (ok "Active agents query requires off-chain indexing")
)

(define-read-only (is-transfer-expired (transfer-id uint))
    (match (map-get? transfers transfer-id)
        some-transfer (>= stacks-block-height (get expires-at some-transfer))
        false
    )
)

(define-private (update-user-stats (sender principal) (recipient principal) (amount uint))
    (begin
        (map-set user-profiles sender
            (merge (default-to {total-sent: u0, total-received: u0, reputation-score: u100, registration-block: stacks-block-height, kyc-verified: false} (map-get? user-profiles sender))
                   {total-sent: (+ (get total-sent (default-to {total-sent: u0, total-received: u0, reputation-score: u100, registration-block: stacks-block-height, kyc-verified: false} (map-get? user-profiles sender))) amount)}))
        
        (map-set user-profiles recipient
            (merge (default-to {total-sent: u0, total-received: u0, reputation-score: u100, registration-block: stacks-block-height, kyc-verified: false} (map-get? user-profiles recipient))
                   {total-received: (+ (get total-received (default-to {total-sent: u0, total-received: u0, reputation-score: u100, registration-block: stacks-block-height, kyc-verified: false} (map-get? user-profiles recipient))) amount)}))
        
        (update-reputation sender u5)
        (update-reputation recipient u3)
    )
)

(define-private (update-reputation (user principal) (points uint))
    (let ((current-profile (default-to {total-sent: u0, total-received: u0, reputation-score: u100, registration-block: stacks-block-height, kyc-verified: false} (map-get? user-profiles user))))
        (map-set user-profiles user
            (merge current-profile {
                reputation-score: (min u1000 (+ (get reputation-score current-profile) points))
            })
        )
    )
)

(define-private (min (a uint) (b uint))
    (if (<= a b) a b)
)

(define-private (max (a uint) (b uint))
    (if (>= a b) a b)
)

(define-public (batch-transfer (recipients (list 10 {recipient: principal, amount: uint})))
    (let ((sender tx-sender))
        (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
        (fold process-batch-transfer recipients (ok (list)))
    )
)

(define-private (process-batch-transfer (transfer-data {recipient: principal, amount: uint}) (prev-result (response (list 10 uint) uint)))
    (match prev-result
        ok-list (match (initiate-transfer (get recipient transfer-data) (get amount transfer-data) none)
                    ok-id (ok (unwrap! (as-max-len? (append ok-list ok-id) u10) ERR_INVALID_AMOUNT))
                    err-val (err err-val))
        err-val (err err-val)
    )
)

(define-public (emergency-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-read-only (get-pending-dispute (transfer-id uint))
    (map-get? pending-disputes transfer-id)
)

(define-read-only (calculate-agent-earnings (agent principal))
    (let ((agent-data (unwrap! (map-get? agents agent) ERR_AGENT_NOT_FOUND)))
        (ok (/ (* (get total-volume agent-data) (get commission-rate agent-data)) u10000))
    )
)

(define-read-only (get-transfer-history (user principal))
    (let ((user-profile (map-get? user-profiles user)))
        (match user-profile
            some-profile {
                total-sent: (get total-sent some-profile),
                total-received: (get total-received some-profile),
                transaction-count: (+ (/ (get total-sent some-profile) MIN_TRANSFER_AMOUNT) (/ (get total-received some-profile) MIN_TRANSFER_AMOUNT)),
                reputation: (get reputation-score some-profile)
            }
            {total-sent: u0, total-received: u0, transaction-count: u0, reputation: u0}
        )
    )
)

(define-public (extend-transfer-deadline (transfer-id uint) (additional-blocks uint))
    (let (
        (transfer-data (unwrap! (map-get? transfers transfer-id) ERR_TRANSFER_NOT_FOUND))
        (sender (get sender transfer-data))
    )
        (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status transfer-data) "pending") ERR_TRANSFER_ALREADY_COMPLETED)
        (asserts! (<= additional-blocks u504) ERR_INVALID_AMOUNT)
        
        (map-set transfers transfer-id 
            (merge transfer-data {
                expires-at: (+ (get expires-at transfer-data) additional-blocks)
            })
        )
        (ok true)
    )
)

(define-public (create-currency-pool (currency (string-ascii 3)) (initial-liquidity uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> initial-liquidity MIN_LIQUIDITY_THRESHOLD) ERR_INSUFFICIENT_LIQUIDITY)
        (asserts! (is-none (map-get? currency-pools currency)) ERR_ALREADY_REGISTERED)
        (map-set currency-pools currency {
            total-liquidity: initial-liquidity,
            available-liquidity: initial-liquidity,
            exchange-volume: u0,
            last-updated: stacks-block-height,
            active: true
        })
        (ok true)
    )
)

(define-public (add-liquidity (currency (string-ascii 3)) (amount uint))
    (let (
        (pool-data (unwrap! (map-get? currency-pools currency) ERR_CURRENCY_NOT_SUPPORTED))
        (provider tx-sender)
    )
        (asserts! (is-eq provider CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (get active pool-data) ERR_CURRENCY_NOT_SUPPORTED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (map-set currency-pools currency {
            total-liquidity: (+ (get total-liquidity pool-data) amount),
            available-liquidity: (+ (get available-liquidity pool-data) amount),
            exchange-volume: (get exchange-volume pool-data),
            last-updated: stacks-block-height,
            active: true
        })
        (ok true)
    )
)

(define-public (remove-liquidity (currency (string-ascii 3)) (amount uint))
    (let (
        (pool-data (unwrap! (map-get? currency-pools currency) ERR_CURRENCY_NOT_SUPPORTED))
        (provider tx-sender)
    )
        (asserts! (is-eq provider CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (get active pool-data) ERR_CURRENCY_NOT_SUPPORTED)
        (asserts! (>= (get available-liquidity pool-data) amount) ERR_INSUFFICIENT_LIQUIDITY)
        (map-set currency-pools currency {
            total-liquidity: (- (get total-liquidity pool-data) amount),
            available-liquidity: (- (get available-liquidity pool-data) amount),
            exchange-volume: (get exchange-volume pool-data),
            last-updated: stacks-block-height,
            active: (> (- (get total-liquidity pool-data) amount) MIN_LIQUIDITY_THRESHOLD)
        })
        (ok true)
    )
)

(define-public (exchange-transfer (recipient principal) (stx-amount uint) (target-currency (string-ascii 3)) (min-expected-amount uint))
    (let (
        (sender tx-sender)
        (exchange-id (var-get next-exchange-id))
        (exchange-pair (unwrap! (construct-currency-pair "STX" target-currency) ERR_CURRENCY_NOT_SUPPORTED))
        (exchange-rate (unwrap! (map-get? exchange-rates exchange-pair) ERR_INVALID_EXCHANGE_RATE))
        (pool-data (unwrap! (map-get? currency-pools target-currency) ERR_CURRENCY_NOT_SUPPORTED))
        (target-amount (calculate-exchange-amount stx-amount exchange-rate))
        (exchange-fee (calculate-exchange-fee stx-amount))
        (total-cost (+ stx-amount exchange-fee))
        (slippage (calculate-slippage target-amount min-expected-amount))
    )
        (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq sender recipient)) ERR_INVALID_RECIPIENT)
        (asserts! (and (>= stx-amount MIN_TRANSFER_AMOUNT) (<= stx-amount MAX_TRANSFER_AMOUNT)) ERR_INVALID_AMOUNT)
        (asserts! (>= (stx-get-balance sender) total-cost) ERR_INSUFFICIENT_BALANCE)
        (asserts! (get active pool-data) ERR_CURRENCY_NOT_SUPPORTED)
        (asserts! (>= (get available-liquidity pool-data) target-amount) ERR_INSUFFICIENT_LIQUIDITY)
        (asserts! (<= slippage MAX_SLIPPAGE_RATE) ERR_EXCHANGE_SLIPPAGE)
        (asserts! (>= target-amount min-expected-amount) ERR_EXCHANGE_SLIPPAGE)
        
        (try! (stx-transfer? total-cost sender (as-contract tx-sender)))
        
        (map-set exchange-transactions exchange-id {
            sender: sender,
            recipient: recipient,
            source-amount: stx-amount,
            target-amount: target-amount,
            source-currency: "STX",
            target-currency: target-currency,
            exchange-rate-used: exchange-rate,
            exchange-fee: exchange-fee,
            created-at: stacks-block-height,
            status: "completed"
        })
        
        (map-set currency-pools target-currency {
            total-liquidity: (get total-liquidity pool-data),
            available-liquidity: (- (get available-liquidity pool-data) target-amount),
            exchange-volume: (+ (get exchange-volume pool-data) target-amount),
            last-updated: stacks-block-height,
            active: (get active pool-data)
        })
        
        (var-set next-exchange-id (+ exchange-id u1))
        (var-set total-volume (+ (var-get total-volume) stx-amount))
        (var-set total-fees-collected (+ (var-get total-fees-collected) exchange-fee))
        
        (update-user-stats sender recipient stx-amount)
        (ok exchange-id)
    )
)

(define-private (construct-currency-pair (from-currency (string-ascii 3)) (to-currency (string-ascii 3)))
    (let ((pair (concat from-currency to-currency)))
        (if (is-eq (len pair) u6)
            (some pair)
            none
        )
    )
)

(define-private (calculate-exchange-amount (amount uint) (exchange-rate uint))
    (/ (* amount exchange-rate) u1000000)
)

(define-private (calculate-exchange-fee (amount uint))
    (let ((base-fee (/ (* amount EXCHANGE_FEE_RATE) u10000)))
        (if (< base-fee u5000) u5000 base-fee)
    )
)

(define-private (calculate-slippage (expected uint) (actual uint))
    (if (> expected actual)
        (/ (* (- expected actual) u10000) expected)
        u0
    )
)

(define-read-only (get-exchange-quote (stx-amount uint) (target-currency (string-ascii 3)))
    (let (
        (exchange-pair (unwrap! (construct-currency-pair "STX" target-currency) ERR_CURRENCY_NOT_SUPPORTED))
        (exchange-rate (unwrap! (map-get? exchange-rates exchange-pair) ERR_INVALID_EXCHANGE_RATE))
        (pool-data (unwrap! (map-get? currency-pools target-currency) ERR_CURRENCY_NOT_SUPPORTED))
        (target-amount (calculate-exchange-amount stx-amount exchange-rate))
        (exchange-fee (calculate-exchange-fee stx-amount))
    )
        (asserts! (get active pool-data) ERR_CURRENCY_NOT_SUPPORTED)
        (asserts! (>= (get available-liquidity pool-data) target-amount) ERR_INSUFFICIENT_LIQUIDITY)
        (ok {
            target-amount: target-amount,
            exchange-fee: exchange-fee,
            total-cost: (+ stx-amount exchange-fee),
            exchange-rate: exchange-rate,
            available-liquidity: (get available-liquidity pool-data)
        })
    )
)

(define-read-only (get-currency-pool (currency (string-ascii 3)))
    (map-get? currency-pools currency)
)

(define-read-only (get-exchange-transaction (exchange-id uint))
    (map-get? exchange-transactions exchange-id)
)

(define-read-only (get-supported-currencies)
    (ok "Supported currencies query requires off-chain indexing")
)

(register-user false)
