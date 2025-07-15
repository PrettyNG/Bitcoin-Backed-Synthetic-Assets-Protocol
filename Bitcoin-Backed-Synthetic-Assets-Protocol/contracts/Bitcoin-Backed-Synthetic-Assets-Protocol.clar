
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

;; Calculate price based on constant product formula (x * y = k)
(define-private (calculate-output-amount (input-amount uint) (input-reserve uint) (output-reserve uint) (fee uint))
  (let
    (
      (input-with-fee (* input-amount (- u10000 fee)))
      (numerator (* input-with-fee output-reserve))
      (denominator (+ (* input-reserve u10000) input-with-fee))
    )
    (/ numerator denominator)
  )
)

;; Swap assets
(define-public (swap (pair-id uint) (input-is-a bool) (input-amount uint) (min-output-amount uint))
  (begin
    (asserts! (not (var-get protocol-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (> input-amount u0) ERR-INVALID-AMOUNT)
    
    (match (map-get? trading-pairs { pair-id: pair-id })
      pair-data
      (begin
        (asserts! (get is-active pair-data) ERR-TRADING-PAIR-NOT-FOUND)
        
        (let
          (
            (input-reserve (if input-is-a (get reserve-a pair-data) (get reserve-b pair-data)))
            (output-reserve (if input-is-a (get reserve-b pair-data) (get reserve-a pair-data)))
            (input-asset-id (if input-is-a (get asset-a-id pair-data) (get asset-b-id pair-data)))
            (output-asset-id (if input-is-a (get asset-b-id pair-data) (get asset-a-id pair-data)))
            (output-amount (calculate-output-amount input-amount input-reserve output-reserve (get fee pair-data)))
          )
          ;; Check if output meets minimum requirements
          (asserts! (>= output-amount min-output-amount) ERR-SWAP-SLIPPAGE-EXCEEDED)
          
          ;; In a real implementation, this would transfer the input asset from the sender
          ;; and transfer the output asset to the sender
          ;; For this example, we're just updating the reserves
          
          ;; Update reserves
          (map-set trading-pairs
            { pair-id: pair-id }
            (merge pair-data {
              reserve-a: (if input-is-a 
                          (+ (get reserve-a pair-data) input-amount)
                          (- (get reserve-a pair-data) output-amount)),
              reserve-b: (if input-is-a
                          (- (get reserve-b pair-data) output-amount)
                          (+ (get reserve-b pair-data) input-amount))
            })
          )
          
          (ok output-amount)
        )
      )
      ERR-TRADING-PAIR-NOT-FOUND
    )
  )
)

;; Add liquidity to a pair
(define-public (add-liquidity (pair-id uint) (amount-a uint) (amount-b uint))
  (begin
    (asserts! (not (var-get protocol-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount-a u0) ERR-INVALID-AMOUNT)
    (asserts! (> amount-b u0) ERR-INVALID-AMOUNT)
    
    (match (map-get? trading-pairs { pair-id: pair-id })
      pair-data
      (begin
        (asserts! (get is-active pair-data) ERR-TRADING-PAIR-NOT-FOUND)
        
        ;; In a real implementation, this would transfer the input assets from the sender
        ;; For this example, we're just updating the reserves
        
        ;; Update reserves
        (map-set trading-pairs
          { pair-id: pair-id }
          (merge pair-data {
            reserve-a: (+ (get reserve-a pair-data) amount-a),
            reserve-b: (+ (get reserve-b pair-data) amount-b)
          })
        )
        
        (ok true)
      )
      ERR-TRADING-PAIR-NOT-FOUND
    )
  )
)

;; Flash loan data
(define-map flash-loans
  { loan-id: uint }
  {
    borrower: principal,
    asset-id: uint,
    amount: uint,
    fee: uint,
    is-active: bool,
    timestamp: uint
  }
)

(define-data-var flash-loan-counter uint u0)
(define-data-var flash-loan-fee-rate uint u9) ;; 0.09% fee

;; Execute a flash loan
(define-public (flash-loan (asset-id uint) (amount uint) (callback-contract principal) (callback-function (string-ascii 128)))
  (begin
    (asserts! (not (var-get protocol-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-asset-supported asset-id) ERR-ASSET-NOT-SUPPORTED)
    
    (let
      (
        (loan-id (var-get flash-loan-counter))
        (loan-fee (/ (* amount (var-get flash-loan-fee-rate)) u10000))
      )
      ;; Check if there's enough liquidity
      (match (map-get? liquidity-pools { asset-id: asset-id })
        pool-data
        (begin
          (asserts! (>= (get synthetic-balance pool-data) amount) ERR-POOL-INSUFFICIENT-LIQUIDITY)
          
          ;; Create the flash loan
          (map-set flash-loans
            { loan-id: loan-id }
            {
              borrower: tx-sender,
              asset-id: asset-id,
              amount: amount,
              fee: loan-fee,
              is-active: true,
              timestamp: stacks-block-height
            }
          )
          
          ;; Increment loan counter
          (var-set flash-loan-counter (+ loan-id u1))
          
          ;; In a real implementation, this would:
          ;; 1. Transfer the borrowed assets to the borrower
          ;; 2. Call the callback function on the callback contract
          ;; 3. Verify that the borrowed amount + fee has been repaid
          ;; 4. If not repaid, revert the transaction
          
          ;; For this example, we're just updating the loan status
          (map-set flash-loans
            { loan-id: loan-id }
            {
              borrower: tx-sender,
              asset-id: asset-id,
              amount: amount,
              fee: loan-fee,
              is-active: false,
              timestamp: stacks-block-height
            }
          )
          
          ;; Add the fee to the protocol fees
          (var-set total-protocol-fees (+ (var-get total-protocol-fees) loan-fee))
          
          (ok loan-id)
        )
        ERR-POOL-INSUFFICIENT-LIQUIDITY
      )
    )
  )
)

(define-map limit-orders
  { order-id: uint }
  {
    owner: principal,
    pair-id: uint,
    is-buy: bool, ;; true = buy asset-a with asset-b, false = sell asset-a for asset-b
    amount: uint,
    price: uint, ;; in terms of asset-b per asset-a * PRECISION_FACTOR
    filled-amount: uint,
    status: (string-ascii 10), ;; "open", "filled", "cancelled"
    expiration: uint
  }
)

(define-data-var order-counter uint u0)

;; Create a limit order
(define-public (create-limit-order (pair-id uint) (is-buy bool) (amount uint) (price uint) (expiration uint))
  (begin
    (asserts! (not (var-get protocol-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> price u0) ERR-INVALID-AMOUNT)
    (asserts! (> expiration stacks-block-height) ERR-INVALID-AMOUNT)
    
    (match (map-get? trading-pairs { pair-id: pair-id })
      pair-data
      (begin
        (asserts! (get is-active pair-data) ERR-TRADING-PAIR-NOT-FOUND)
        
        (let
          (
            (order-id (var-get order-counter))
            (required-balance (* amount price))
          )
          ;; In a real implementation, this would check that the user has the required balance
          ;; and lock the funds for the duration of the order
          
          ;; Create the order
          (map-set limit-orders
            { order-id: order-id }
            {
              owner: tx-sender,
              pair-id: pair-id,
              is-buy: is-buy,
              amount: amount,
              price: price,
              filled-amount: u0,
              status: "open",
              expiration: expiration
            }
          )
          
          ;; Increment order counter
          (var-set order-counter (+ order-id u1))
          
          (ok order-id)
        )
      )
      ERR-TRADING-PAIR-NOT-FOUND
    )
  )
)

;; Cancel a limit order
(define-public (cancel-limit-order (order-id uint))
  (begin
    (match (map-get? limit-orders { order-id: order-id })
      order-data
      (begin
        ;; Check that the order belongs to the sender or is expired
        (asserts! (or 
                    (is-eq tx-sender (get owner order-data))
                    (>= stacks-block-height (get expiration order-data))
                  ) ERR-NOT-AUTHORIZED)
        
        ;; Check that the order is still open
        (asserts! (is-eq (get status order-data) "open") ERR-LIMIT-ORDER-INVALID)
        
        ;; In a real implementation, this would release any locked funds back to the owner
        
        ;; Update order status
        (map-set limit-orders
          { order-id: order-id }
          (merge order-data { status: "cancelled" })
        )
        
        (ok true)
      )
      ERR-LIMIT-ORDER-INVALID
    )
  )
)

;; Execute a limit order - this would typically be called by keepers or automated systems
(define-public (execute-limit-order (order-id uint))
  (begin
    (asserts! (not (var-get protocol-paused)) ERR-NOT-AUTHORIZED)
    
    (match (map-get? limit-orders { order-id: order-id })
      order-data
      (begin
        ;; Check that the order is still open and not expired
        (asserts! (is-eq (get status order-data) "open") ERR-LIMIT-ORDER-INVALID)
        (asserts! (< stacks-block-height (get expiration order-data)) ERR-LIMIT-ORDER-INVALID)
        
        (match (map-get? trading-pairs { pair-id: (get pair-id order-data) })
          pair-data
          (begin
            (asserts! (get is-active pair-data) ERR-TRADING-PAIR-NOT-FOUND)
            
            (let
              (
                (current-price (/ (* (get reserve-b pair-data) PRECISION-FACTOR) (get reserve-a pair-data)))
              )
              ;; Check if the price conditions are met
              (asserts! (if (get is-buy order-data)
                          (<= current-price (get price order-data)) ;; For buy orders, current price must be <= limit price
                          (>= current-price (get price order-data)) ;; For sell orders, current price must be >= limit price
                        ) ERR-LIMIT-ORDER-INVALID)
              
              ;; In a real implementation, this would execute the swap and update balances
              
              ;; Update order status
              (map-set limit-orders
                { order-id: order-id }
                (merge order-data { 
                  status: "filled",
                  filled-amount: (get amount order-data)
                })
              )
              
              (ok true)
            )
          )
          ERR-TRADING-PAIR-NOT-FOUND
        )
      )
      ERR-LIMIT-ORDER-INVALID
    )
  )
)