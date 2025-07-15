
;; title: Bitcoin-Backed-Synthetic-Assets-Protocol
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u1001))
(define-constant ERR-INVALID-AMOUNT (err u1002))
(define-constant ERR-VAULT-NOT-FOUND (err u1003))
(define-constant ERR-PRICE-EXPIRED (err u1004))
(define-constant ERR-VAULT-UNDERCOLLATERALIZED (err u1005))
(define-constant ERR-LIQUIDATION-FAILED (err u1006))
(define-constant ERR-POOL-INSUFFICIENT-LIQUIDITY (err u1007))
(define-constant ERR-ASSET-NOT-SUPPORTED (err u1008))
(define-constant ERR-COOLDOWN-PERIOD (err u1009))
(define-constant ERR-MAX-SUPPLY-REACHED (err u1010))
(define-constant ERR-ORACLE-DATA-UNAVAILABLE (err u1011))
(define-constant ERR-GOVERNANCE-REJECTION (err u1012))

;; System parameters
(define-constant MIN-COLLATERALIZATION-RATIO u150) ;; 150%
(define-constant LIQUIDATION-THRESHOLD u120) ;; 120%
(define-constant LIQUIDATION-PENALTY u10) ;; 10%
(define-constant PROTOCOL-FEE u5) ;; 0.5%
(define-constant ORACLE-PRICE-EXPIRY u3600) ;; 1 hour
(define-constant COOLDOWN-PERIOD u86400) ;; 24 hours
(define-constant PRECISION-FACTOR u1000000) ;; 6 decimals


;; Supported Asset Types
(define-map supported-assets
  { asset-id: uint }
  {
    name: (string-ascii 24),
    is-active: bool,
    max-supply: uint,
    current-supply: uint,
    collateral-ratio: uint
  }
)

;; Vaults - where users lock their BTC collateral to mint synthetic assets
(define-map vaults
  { owner: principal, asset-id: uint }
  {
    collateral-amount: uint,
    debt-amount: uint,
    last-update: uint,
    liquidation-in-progress: bool
  }
)

;; Price Oracle data
(define-map asset-prices
  { asset-id: uint }
  {
    price: uint,
    last-update: uint,
    source: principal
  }
)

;; Liquidity Pools
(define-map liquidity-pools
  { asset-id: uint }
  {
    stx-balance: uint,
    synthetic-balance: uint,
    total-shares: uint
  }
)

;; LP Token balances
(define-map lp-balances
  { asset-id: uint, owner: principal }
  { shares: uint }
)

;; User Balances for synthetic assets
(define-map synthetic-asset-balances
  { asset-id: uint, owner: principal }
  { balance: uint }
)

;; Protocol Parameters controlled by governance
(define-data-var protocol-paused bool false)
(define-data-var governance-address principal tx-sender)
(define-data-var treasury-address principal tx-sender)
(define-data-var total-protocol-fees uint u0)

(define-public (set-governance-address (new-address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (ok (var-set governance-address new-address))
  )
)

(define-public (set-treasury-address (new-address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (ok (var-set treasury-address new-address))
  )
)

(define-public (pause-protocol)
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (ok (var-set protocol-paused true))
  )
)

(define-public (resume-protocol)
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (ok (var-set protocol-paused false))
  )
)

(define-public (add-supported-asset (asset-id uint) (name (string-ascii 24)) (max-supply uint) (collateral-ratio uint))
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (asserts! (>= collateral-ratio MIN-COLLATERALIZATION-RATIO) ERR-INVALID-AMOUNT)
    (ok (map-set supported-assets 
      { asset-id: asset-id } 
      { 
        name: name, 
        is-active: true, 
        max-supply: max-supply, 
        current-supply: u0, 
        collateral-ratio: collateral-ratio 
      }
    ))
  )
)

(define-public (update-asset-status (asset-id uint) (is-active bool))
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (match (map-get? supported-assets { asset-id: asset-id })
      asset-data (ok (map-set supported-assets 
        { asset-id: asset-id } 
        (merge asset-data { is-active: is-active })
      ))
      ERR-ASSET-NOT-SUPPORTED
    )
  )
)

(define-public (update-collateral-ratio (asset-id uint) (new-ratio uint))
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (asserts! (>= new-ratio MIN-COLLATERALIZATION-RATIO) ERR-INVALID-AMOUNT)
    (match (map-get? supported-assets { asset-id: asset-id })
      asset-data (ok (map-set supported-assets 
        { asset-id: asset-id } 
        (merge asset-data { collateral-ratio: new-ratio })
      ))
      ERR-ASSET-NOT-SUPPORTED
    )
  )
)

(define-private (is-oracle (address principal))
  ;; In a production system, this would check against a list of approved oracles
  ;; For simplicity, we're just checking if it's the governance address
  (is-eq address (var-get governance-address))
)

(define-private (get-price (asset-id uint))
  (match (map-get? asset-prices { asset-id: asset-id })
    price-data (begin
      (asserts! (< (- stacks-block-height (get last-update price-data)) ORACLE-PRICE-EXPIRY) ERR-PRICE-EXPIRED)
      (ok (get price price-data))
    )
    ERR-ORACLE-DATA-UNAVAILABLE
  )
)

(define-private (get-btc-price)
  ;; For simplicity, we're using asset-id 0 as BTC
  (get-price u0)
)

(define-private (is-asset-supported (asset-id uint))
  (match (map-get? supported-assets { asset-id: asset-id })
    asset-data (get is-active asset-data)
    false
  )
)

;; New data variables
(define-data-var last-yield-distribution uint u0)
(define-data-var yield-fee-percentage uint u20) ;; 2% default yield fee
(define-data-var total-staked-tokens uint u0)
(define-data-var proposal-counter uint u0)

;; New data maps for additional features
;; Staking system
(define-map staked-balances
  { owner: principal }
  {
    amount: uint,
    lock-until: uint,
    accumulated-yield: uint,
    last-claim: uint
  }
)

;; Governance proposals
(define-map governance-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    description: (string-utf8 256),
    function-call: (buff 128),
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    execution-block: uint
  }
)

;; User proposal votes
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { 
    vote: bool,
    weight: uint
  }
)

;; Collateral utilization tracking for interest rates
(define-map asset-utilization
  { asset-id: uint }
  {
    total-collateral: uint,
    total-borrowed: uint,
    base-rate: uint,
    utilization-multiplier: uint,
    last-rate-update: uint
  }
)

;; Asset lock settings for time-locked assets
(define-map asset-locks
  { owner: principal, asset-id: uint }
  {
    locked-amount: uint,
    unlock-height: uint
  }
)

;; Oracle Access Control
(define-map authorized-oracles
  { address: principal }
  { 
    is-active: bool,
    asset-types: (list 10 uint)
  }
)

;; Add or update an oracle
(define-public (set-oracle (oracle-address principal) (is-active bool) (asset-types (list 10 uint)))
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-oracles
      { address: oracle-address }
      { 
        is-active: is-active,
        asset-types: asset-types
      }
    ))
  )
)

;; Enhanced oracle price update - requires authorization
(define-public (update-price (asset-id uint) (price uint))
  (begin
    (match (map-get? authorized-oracles { address: tx-sender })
      oracle-data
      (begin
        (asserts! (get is-active oracle-data) ERR-NOT-AUTHORIZED)
        (asserts! (> price u0) ERR-INVALID-AMOUNT)
        
        ;; Check if oracle is authorized for this asset type
        (asserts! (is-some (index-of (get asset-types oracle-data) asset-id)) ERR-NOT-AUTHORIZED)
        
        (ok (map-set asset-prices
          { asset-id: asset-id }
          {
            price: price,
            last-update: stacks-block-height,
            source: tx-sender
          }
        ))
      )
      ERR-NOT-AUTHORIZED
    )
  )
)


;; Get the current price with validation
(define-public (query-price (asset-id uint))
  (begin
    (match (map-get? asset-prices { asset-id: asset-id })
      price-data
      (begin
        (asserts! (< (- stacks-block-height (get last-update price-data)) ORACLE-PRICE-EXPIRY) ERR-PRICE-EXPIRED)
        (ok (get price price-data))
      )
      ERR-ORACLE-DATA-UNAVAILABLE
    )
  )
)

(define-constant ERR-INSURANCE-CLAIM-REJECTED (err u1013))
(define-constant ERR-REFERRAL-NOT-FOUND (err u1014))
(define-constant ERR-TRADING-PAIR-NOT-FOUND (err u1015))
(define-constant ERR-FLASH-LOAN-FAILED (err u1016))
(define-constant ERR-VAULT-LOCKED (err u1017))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1018))
(define-constant ERR-SWAP-SLIPPAGE-EXCEEDED (err u1019))
(define-constant ERR-LIMIT-ORDER-INVALID (err u1020))
(define-constant ERR-NFT-COLLATERAL-INVALID (err u1021))
(define-constant ERR-YIELD-FARM-NOT-FOUND (err u1022))

;; Insurance fund to cover bad debt from liquidations
(define-data-var insurance-fund-balance uint u0)
(define-data-var insurance-premium-rate uint u2) ;; 0.2% premium
(define-data-var insurance-coverage-ratio uint u80) ;; 80% coverage

(define-map insurance-claims 
  { claim-id: uint }
  {
    claimant: principal,
    asset-id: uint,
    amount: uint,
    status: (string-ascii 10), ;; "pending", "approved", "rejected"
    timestamp: uint
  }
)

(define-data-var claim-counter uint u0)

;; Contribute to insurance fund
(define-public (contribute-to-insurance-fund (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; In a real implementation, this would transfer STX from tx-sender to the contract
    ;; For this example, we're just incrementing the fund balance
    (var-set insurance-fund-balance (+ (var-get insurance-fund-balance) amount))
    (ok (var-get insurance-fund-balance))
  )
)

(define-public (file-insurance-claim (asset-id uint) (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let ((claim-id (var-get claim-counter)))
      (var-set claim-counter (+ claim-id u1))
      (map-set insurance-claims 
        { claim-id: claim-id }
        {
          claimant: tx-sender,
          asset-id: asset-id,
          amount: amount,
          status: "pending",
          timestamp: stacks-block-height
        }
      )
      (ok claim-id)
    )
  )
)

;; Review an insurance claim - governance only
(define-public (review-insurance-claim (claim-id uint) (approve bool))
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    
    (match (map-get? insurance-claims { claim-id: claim-id })
      claim-data
      (begin
        (if approve
          (begin
            ;; Calculate payout amount based on coverage ratio
            (let 
              (
                (payout-amount (/ (* (get amount claim-data) (var-get insurance-coverage-ratio)) u100))
              )
              ;; Check if insurance fund has enough balance
              (asserts! (<= payout-amount (var-get insurance-fund-balance)) ERR-INSUFFICIENT-COLLATERAL)
              
              ;; Update insurance fund balance
              (var-set insurance-fund-balance (- (var-get insurance-fund-balance) payout-amount))
              
              ;; In a real implementation, this would transfer the payout to the claimant
              ;; For this example, we're just updating the claim status
              
              ;; Update claim status
              (map-set insurance-claims
                { claim-id: claim-id }
                (merge claim-data { status: "approved" })
              )
              
              (ok payout-amount)
            )
          )
          (begin
            ;; Reject the claim
            (map-set insurance-claims
              { claim-id: claim-id }
              (merge claim-data { status: "rejected" })
            )
            (ok u0)
          )
        )
      )
      ERR-INSURANCE-CLAIM-REJECTED
    )
  )
)

;; Get insurance fund details
(define-public (get-insurance-fund-info)
  (ok {
    balance: (var-get insurance-fund-balance),
    premium-rate: (var-get insurance-premium-rate),
    coverage-ratio: (var-get insurance-coverage-ratio)
  })
)

;; Define trading pairs
(define-map trading-pairs
  { pair-id: uint }
  {
    asset-a-id: uint,
    asset-b-id: uint,
    reserve-a: uint,
    reserve-b: uint,
    fee: uint, ;; in basis points (1/100 of a percent)
    is-active: bool
  }
)

(define-data-var pair-counter uint u0)

;; Create a new trading pair
(define-public (create-trading-pair (asset-a-id uint) (asset-b-id uint) (fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get governance-address)) ERR-NOT-AUTHORIZED)
    (asserts! (is-asset-supported asset-a-id) ERR-ASSET-NOT-SUPPORTED)
    (asserts! (is-asset-supported asset-b-id) ERR-ASSET-NOT-SUPPORTED)
    (asserts! (not (is-eq asset-a-id asset-b-id)) ERR-INVALID-AMOUNT)
    (asserts! (<= fee u1000) ERR-INVALID-AMOUNT) ;; Max fee of 10%
    
    (let 
      (
        (pair-id (var-get pair-counter))
      )
      ;; Create the pair
      (map-set trading-pairs
        { pair-id: pair-id }
        {
          asset-a-id: asset-a-id,
          asset-b-id: asset-b-id,
          reserve-a: u0,
          reserve-b: u0,
          fee: fee,
          is-active: true
        }
      )
      
      ;; Increment pair counter
      (var-set pair-counter (+ pair-id u1))
      
      (ok pair-id)
    )
  )
)

