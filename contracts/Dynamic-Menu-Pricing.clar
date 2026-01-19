;; title: Dynamic-Menu-Pricing
;; version: 1.0.0
;; summary: Dynamic pricing system for restaurant menus with surge pricing and customer voting
;; description: A smart contract that implements dynamic menu pricing with peak hour surge pricing and customer voting mechanisms

;; constants
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PRICE (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-INVALID-VOTE (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-INVALID-ITEM (err u106))
(define-constant ERR-INVALID-MULTIPLIER (err u107))
(define-constant ERR-RESERVATION-EXISTS (err u108))
(define-constant ERR-NO-RESERVATION (err u109))
(define-constant ERR-RESERVATION-EXPIRED (err u110))
(define-constant ERR-BUNDLE-NOT-FOUND (err u111))
(define-constant ERR-INVALID-BUNDLE (err u112))
(define-constant ERR-BUNDLE-INACTIVE (err u113))
(define-constant ERR-LOYALTY-EXISTS (err u114))
(define-constant ERR-FLASH-SALE-ACTIVE (err u115))
(define-constant ERR-NO-FLASH-SALE (err u116))
(define-constant ERR-FLASH-SALE-EXPIRED (err u117))

(define-constant BRONZE-THRESHOLD u100000)
(define-constant SILVER-THRESHOLD u500000)
(define-constant GOLD-THRESHOLD u1000000)
(define-constant BRONZE-DISCOUNT u5)
(define-constant SILVER-DISCOUNT u10)
(define-constant GOLD-DISCOUNT u15)

(define-constant PEAK-HOUR-START u11)
(define-constant PEAK-HOUR-END u14)
(define-constant EVENING-PEAK-START u18)
(define-constant EVENING-PEAK-END u21)
(define-constant BLOCKS-PER-HOUR u144)
(define-constant VOTING-PERIOD u1000)
(define-constant RESERVATION-DURATION u144)

;; data vars
(define-data-var contract-owner principal tx-sender)
(define-data-var base-surge-multiplier uint u150)
(define-data-var vote-threshold uint u10)
(define-data-var menu-item-counter uint u0)
(define-data-var bundle-counter uint u0)
(define-data-var flash-sale-counter uint u0)

;; data maps
(define-map menu-items 
  { item-id: uint }
  { 
    name: (string-ascii 50),
    base-price: uint,
    category: (string-ascii 20),
    active: bool,
    vote-count: uint,
    total-votes: uint,
    last-vote-block: uint
  }
)

(define-map customer-votes
  { voter: principal, item-id: uint }
  { vote: uint, block-height: uint }
)

(define-map surge-settings
  { setting: (string-ascii 20) }
  { value: uint }
)

(define-map daily-demand
  { item-id: uint, day: uint }
  { orders: uint, total-revenue: uint }
)

(define-map hourly-multipliers
  { hour: uint }
  { multiplier: uint }
)

(define-map item-reservations
  { customer: principal, item-id: uint }
  { 
    quantity: uint,
    reserved-price: uint,
    expiry-block: uint,
    created-block: uint
  }
)

(define-map menu-bundles
  { bundle-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 100),
    discount-percentage: uint,
    active: bool,
    orders-count: uint,
    total-revenue: uint,
    created-block: uint
  }
)

(define-map bundle-items
  { bundle-id: uint, item-id: uint }
  { quantity: uint }
)

(define-map bundle-daily-stats
  { bundle-id: uint, day: uint }
  { orders: uint, revenue: uint }
)

(define-map customer-loyalty
  { customer: principal }
  {
    total-spent: uint,
    tier: (string-ascii 10),
    orders-count: uint,
    joined-block: uint,
    last-order-block: uint
  }
)

(define-map flash-sales
  { sale-id: uint }
  {
    item-id: uint,
    discount-percentage: uint,
    start-block: uint,
    end-block: uint,
    max-quantity: uint,
    sold-quantity: uint,
    active: bool
  }
)

(define-map item-flash-sale
  { item-id: uint }
  { sale-id: uint }
)

;; public functions
(define-public (add-menu-item (name (string-ascii 50)) (base-price uint) (category (string-ascii 20)))
  (let ((item-id (+ (var-get menu-item-counter) u1)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (> base-price u0) ERR-INVALID-PRICE)
    (map-set menu-items 
      { item-id: item-id }
      {
        name: name,
        base-price: base-price,
        category: category,
        active: true,
        vote-count: u0,
        total-votes: u0,
        last-vote-block: stacks-block-height
      }
    )
    (var-set menu-item-counter item-id)
    (ok item-id)
  )
)

(define-public (update-menu-item (item-id uint) (name (string-ascii 50)) (base-price uint) (category (string-ascii 20)))
  (let ((item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (> base-price u0) ERR-INVALID-PRICE)
    (map-set menu-items 
      { item-id: item-id }
      (merge item {
        name: name,
        base-price: base-price,
        category: category
      })
    )
    (ok true)
  )
)

(define-public (toggle-menu-item (item-id uint))
  (let ((item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (map-set menu-items 
      { item-id: item-id }
      (merge item { active: (not (get active item)) })
    )
    (ok true)
  )
)

(define-public (vote-for-item (item-id uint) (vote uint))
  (let (
    (item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND))
    (existing-vote (map-get? customer-votes { voter: tx-sender, item-id: item-id }))
    (current-block stacks-block-height)
  )
    (asserts! (get active item) ERR-INVALID-ITEM)
    (asserts! (and (>= vote u1) (<= vote u5)) ERR-INVALID-VOTE)
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    
    (map-set customer-votes 
      { voter: tx-sender, item-id: item-id }
      { vote: vote, block-height: current-block }
    )
    
    (map-set menu-items 
      { item-id: item-id }
      (merge item {
        vote-count: (+ (get vote-count item) vote),
        total-votes: (+ (get total-votes item) u1),
        last-vote-block: current-block
      })
    )
    (ok true)
  )
)

(define-public (set-surge-multiplier (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (and (>= multiplier u100) (<= multiplier u300)) ERR-INVALID-MULTIPLIER)
    (var-set base-surge-multiplier multiplier)
    (ok true)
  )
)

(define-public (set-hourly-multiplier (hour uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (< hour u24) ERR-INVALID-MULTIPLIER)
    (asserts! (and (>= multiplier u50) (<= multiplier u300)) ERR-INVALID-MULTIPLIER)
    (map-set hourly-multipliers { hour: hour } { multiplier: multiplier })
    (ok true)
  )
)

(define-public (record-order (item-id uint) (quantity uint))
  (let (
    (item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND))
    (current-day (get-current-day))
    (current-demand (default-to { orders: u0, total-revenue: u0 } 
                    (map-get? daily-demand { item-id: item-id, day: current-day })))
    (base-price (get-dynamic-price item-id))
    (loyalty-discount (get-loyalty-discount tx-sender))
    (discounted-price (- base-price (/ (* base-price loyalty-discount) u100)))
    (total-cost (* discounted-price quantity))
  )
    (asserts! (get active item) ERR-INVALID-ITEM)
    (asserts! (> quantity u0) ERR-INVALID-ITEM)
    
    (map-set daily-demand 
      { item-id: item-id, day: current-day }
      {
        orders: (+ (get orders current-demand) quantity),
        total-revenue: (+ (get total-revenue current-demand) total-cost)
      }
    )
    (unwrap-panic (update-loyalty tx-sender total-cost))
    (ok total-cost)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (create-flash-sale (item-id uint) (discount-percentage uint) (duration-blocks uint) (max-quantity uint))
  (let (
    (sale-id (+ (var-get flash-sale-counter) u1))
    (item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND))
    (existing-sale (map-get? item-flash-sale { item-id: item-id }))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (get active item) ERR-INVALID-ITEM)
    (asserts! (and (> discount-percentage u0) (<= discount-percentage u70)) ERR-INVALID-MULTIPLIER)
    (asserts! (> duration-blocks u0) ERR-INVALID-MULTIPLIER)
    (asserts! (> max-quantity u0) ERR-INVALID-MULTIPLIER)
    (asserts! (is-none existing-sale) ERR-FLASH-SALE-ACTIVE)
    
    (map-set flash-sales
      { sale-id: sale-id }
      {
        item-id: item-id,
        discount-percentage: discount-percentage,
        start-block: current-block,
        end-block: (+ current-block duration-blocks),
        max-quantity: max-quantity,
        sold-quantity: u0,
        active: true
      }
    )
    (map-set item-flash-sale { item-id: item-id } { sale-id: sale-id })
    (var-set flash-sale-counter sale-id)
    (ok sale-id)
  )
)

(define-public (end-flash-sale (sale-id uint))
  (let (
    (sale (unwrap! (map-get? flash-sales { sale-id: sale-id }) ERR-NO-FLASH-SALE))
    (item-id (get item-id sale))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (get active sale) ERR-NO-FLASH-SALE)
    
    (map-set flash-sales
      { sale-id: sale-id }
      (merge sale { active: false })
    )
    (map-delete item-flash-sale { item-id: item-id })
    (ok true)
  )
)

(define-public (purchase-flash-sale (item-id uint) (quantity uint))
  (let (
    (sale-mapping (unwrap! (map-get? item-flash-sale { item-id: item-id }) ERR-NO-FLASH-SALE))
    (sale-id (get sale-id sale-mapping))
    (sale (unwrap! (map-get? flash-sales { sale-id: sale-id }) ERR-NO-FLASH-SALE))
    (item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND))
    (current-block stacks-block-height)
    (base-price (get base-price item))
    (discount (get discount-percentage sale))
    (discounted-price (- base-price (/ (* base-price discount) u100)))
    (total-cost (* discounted-price quantity))
    (new-sold-quantity (+ (get sold-quantity sale) quantity))
  )
    (asserts! (get active sale) ERR-NO-FLASH-SALE)
    (asserts! (<= current-block (get end-block sale)) ERR-FLASH-SALE-EXPIRED)
    (asserts! (<= new-sold-quantity (get max-quantity sale)) ERR-INSUFFICIENT-FUNDS)
    (asserts! (> quantity u0) ERR-INVALID-ITEM)
    
    (map-set flash-sales
      { sale-id: sale-id }
      (merge sale { sold-quantity: new-sold-quantity })
    )
    (unwrap-panic (update-loyalty tx-sender total-cost))
    (ok { sale-price: discounted-price, total-cost: total-cost, quantity: quantity })
  )
)

(define-public (create-bundle (name (string-ascii 50)) (description (string-ascii 100)) (discount-percentage uint))
  (let ((bundle-id (+ (var-get bundle-counter) u1)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (<= discount-percentage u50) ERR-INVALID-BUNDLE)
    
    (map-set menu-bundles
      { bundle-id: bundle-id }
      {
        name: name,
        description: description,
        discount-percentage: discount-percentage,
        active: false,
        orders-count: u0,
        total-revenue: u0,
        created-block: stacks-block-height
      }
    )
    (var-set bundle-counter bundle-id)
    (ok bundle-id)
  )
)

(define-public (bundle-item (bundle-id uint) (item-id uint) (quantity uint))
  (let (
    (bundle (unwrap! (map-get? menu-bundles { bundle-id: bundle-id }) ERR-BUNDLE-NOT-FOUND))
    (item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (get active item) ERR-INVALID-ITEM)
    (asserts! (> quantity u0) ERR-INVALID-BUNDLE)
    
    (map-set bundle-items
      { bundle-id: bundle-id, item-id: item-id }
      { quantity: quantity }
    )
    (ok true)
  )
)

(define-public (activate-bundle (bundle-id uint))
  (let ((bundle (unwrap! (map-get? menu-bundles { bundle-id: bundle-id }) ERR-BUNDLE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    
    (map-set menu-bundles
      { bundle-id: bundle-id }
      (merge bundle { active: true })
    )
    (ok true)
  )
)

(define-public (deactivate-bundle (bundle-id uint))
  (let ((bundle (unwrap! (map-get? menu-bundles { bundle-id: bundle-id }) ERR-BUNDLE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    
    (map-set menu-bundles
      { bundle-id: bundle-id }
      (merge bundle { active: false })
    )
    (ok true)
  )
)

(define-public (order-bundle (bundle-id uint))
  (let (
    (bundle (unwrap! (map-get? menu-bundles { bundle-id: bundle-id }) ERR-BUNDLE-NOT-FOUND))
    (base-bundle-price (get-bundle-price bundle-id))
    (loyalty-discount (get-loyalty-discount tx-sender))
    (discounted-bundle-price (- base-bundle-price (/ (* base-bundle-price loyalty-discount) u100)))
    (current-day (get-current-day))
    (current-stats (default-to { orders: u0, revenue: u0 }
                               (map-get? bundle-daily-stats { bundle-id: bundle-id, day: current-day })))
  )
    (asserts! (get active bundle) ERR-BUNDLE-INACTIVE)
    (asserts! (> discounted-bundle-price u0) ERR-INVALID-BUNDLE)
    
    (map-set menu-bundles
      { bundle-id: bundle-id }
      (merge bundle {
        orders-count: (+ (get orders-count bundle) u1),
        total-revenue: (+ (get total-revenue bundle) discounted-bundle-price)
      })
    )
    
    (map-set bundle-daily-stats
      { bundle-id: bundle-id, day: current-day }
      {
        orders: (+ (get orders current-stats) u1),
        revenue: (+ (get revenue current-stats) discounted-bundle-price)
      }
    )
    
    (unwrap-panic (record-bundle-item-demand bundle-id))
    (unwrap-panic (update-loyalty tx-sender discounted-bundle-price))
    (ok discounted-bundle-price)
  )
)

(define-public (reserve-item (item-id uint) (quantity uint))
  (let (
    (item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND))
    (current-block stacks-block-height)
    (current-price (get-dynamic-price item-id))
    (expiry-block (+ current-block RESERVATION-DURATION))
    (existing-reservation (map-get? item-reservations { customer: tx-sender, item-id: item-id }))
  )
    (asserts! (get active item) ERR-INVALID-ITEM)
    (asserts! (> quantity u0) ERR-INVALID-ITEM)
    (asserts! (is-none existing-reservation) ERR-RESERVATION-EXISTS)
    
    (map-set item-reservations 
      { customer: tx-sender, item-id: item-id }
      {
        quantity: quantity,
        reserved-price: current-price,
        expiry-block: expiry-block,
        created-block: current-block
      }
    )
    (ok { price: current-price, expiry: expiry-block })
  )
)

(define-public (cancel-reservation (item-id uint))
  (let (
    (reservation (unwrap! (map-get? item-reservations { customer: tx-sender, item-id: item-id }) ERR-NO-RESERVATION))
  )
    (map-delete item-reservations { customer: tx-sender, item-id: item-id })
    (ok true)
  )
)

(define-public (order-reserved-item (item-id uint))
  (let (
    (item (unwrap! (map-get? menu-items { item-id: item-id }) ERR-NOT-FOUND))
    (reservation (unwrap! (map-get? item-reservations { customer: tx-sender, item-id: item-id }) ERR-NO-RESERVATION))
    (current-block stacks-block-height)
    (current-day (get-current-day))
    (current-demand (default-to { orders: u0, total-revenue: u0 } 
                    (map-get? daily-demand { item-id: item-id, day: current-day })))
    (reserved-quantity (get quantity reservation))
    (reserved-price (get reserved-price reservation))
    (total-cost (* reserved-price reserved-quantity))
  )
    (asserts! (get active item) ERR-INVALID-ITEM)
    (asserts! (< current-block (get expiry-block reservation)) ERR-RESERVATION-EXPIRED)
    
    (map-delete item-reservations { customer: tx-sender, item-id: item-id })
    
    (map-set daily-demand 
      { item-id: item-id, day: current-day }
      {
        orders: (+ (get orders current-demand) reserved-quantity),
        total-revenue: (+ (get total-revenue current-demand) total-cost)
      }
    )
    (unwrap-panic (update-loyalty tx-sender total-cost))
    (ok total-cost)
  )
)

;; read only functions
(define-read-only (get-menu-item (item-id uint))
  (map-get? menu-items { item-id: item-id })
)

(define-read-only (get-dynamic-price (item-id uint))
  (let (
    (item (unwrap! (map-get? menu-items { item-id: item-id }) u0))
    (base-price (get base-price item))
    (surge-multiplier (get-surge-multiplier))
    (demand-multiplier (get-demand-multiplier item-id))
    (vote-multiplier (get-vote-multiplier item-id))
  )
    (/ (* (* (* base-price surge-multiplier) demand-multiplier) vote-multiplier) u10000)
  )
)

(define-read-only (get-surge-multiplier)
  (let (
    (current-hour (get-current-hour))
    (custom-multiplier (map-get? hourly-multipliers { hour: current-hour }))
  )
    (if (is-some custom-multiplier)
      (get multiplier (unwrap-panic custom-multiplier))
      (if (is-peak-hour current-hour)
        (var-get base-surge-multiplier)
        u100
      )
    )
  )
)

(define-read-only (get-demand-multiplier (item-id uint))
  (let (
    (current-day (get-current-day))
    (demand-data (map-get? daily-demand { item-id: item-id, day: current-day }))
  )
    (if (is-some demand-data)
      (let ((orders (get orders (unwrap-panic demand-data))))
        (if (> orders u10)
          u120
          (if (> orders u5)
            u110
            u100
          )
        )
      )
      u100
    )
  )
)

(define-read-only (get-vote-multiplier (item-id uint))
  (let (
    (item (unwrap! (map-get? menu-items { item-id: item-id }) u100))
    (total-votes (get total-votes item))
    (vote-count (get vote-count item))
  )
    (if (> total-votes u0)
      (let ((avg-rating (/ vote-count total-votes)))
        (if (>= avg-rating u4)
          u110
          (if (>= avg-rating u3)
            u100
            u95
          )
        )
      )
      u100
    )
  )
)

(define-read-only (is-peak-hour (hour uint))
  (or 
    (and (>= hour PEAK-HOUR-START) (<= hour PEAK-HOUR-END))
    (and (>= hour EVENING-PEAK-START) (<= hour EVENING-PEAK-END))
  )
)

(define-read-only (get-current-hour)
  (/ (mod stacks-block-height u3456) BLOCKS-PER-HOUR)
)

(define-read-only (get-current-day)
  (/ stacks-block-height u3456)
)

(define-read-only (get-customer-vote (voter principal) (item-id uint))
  (map-get? customer-votes { voter: voter, item-id: item-id })
)

(define-read-only (get-daily-demand (item-id uint) (day uint))
  (map-get? daily-demand { item-id: item-id, day: day })
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (get-menu-item-count)
  (var-get menu-item-counter)
)

(define-read-only (get-surge-settings)
  {
    base-surge-multiplier: (var-get base-surge-multiplier),
    vote-threshold: (var-get vote-threshold),
    peak-hour-start: PEAK-HOUR-START,
    peak-hour-end: PEAK-HOUR-END,
    evening-peak-start: EVENING-PEAK-START,
    evening-peak-end: EVENING-PEAK-END
  }
)

(define-read-only (get-item-analytics (item-id uint))
  (let (
    (item (unwrap! (map-get? menu-items { item-id: item-id }) none))
    (current-day (get-current-day))
    (demand-data (map-get? daily-demand { item-id: item-id, day: current-day }))
  )
    (some {
      item: item,
      current-price: (get-dynamic-price item-id),
      daily-demand: demand-data,
      surge-multiplier: (get-surge-multiplier),
      demand-multiplier: (get-demand-multiplier item-id),
      vote-multiplier: (get-vote-multiplier item-id)
    })
  )
)

(define-read-only (get-reservation (customer principal) (item-id uint))
  (map-get? item-reservations { customer: customer, item-id: item-id })
)

(define-read-only (is-reservation-valid (customer principal) (item-id uint))
  (let (
    (reservation (map-get? item-reservations { customer: customer, item-id: item-id }))
    (current-block stacks-block-height)
  )
    (if (is-some reservation)
      (< current-block (get expiry-block (unwrap-panic reservation)))
      false
    )
  )
)

(define-read-only (get-reservation-savings (customer principal) (item-id uint))
  (let (
    (reservation (map-get? item-reservations { customer: customer, item-id: item-id }))
    (current-price (get-dynamic-price item-id))
  )
    (if (is-some reservation)
      (let (
        (reserved-price (get reserved-price (unwrap-panic reservation)))
        (quantity (get quantity (unwrap-panic reservation)))
      )
        (some {
          reserved-total: (* reserved-price quantity),
          current-total: (* current-price quantity),
          savings: (* (- current-price reserved-price) quantity)
        })
      )
      none
    )
  )
)

(define-read-only (get-bundle (bundle-id uint))
  (map-get? menu-bundles { bundle-id: bundle-id })
)

(define-read-only (get-bundle-items (bundle-id uint))
  (fold get-single-bundle-item (list u1 u2 u3 u4 u5) { bundle-id: bundle-id, items: (list) })
)

(define-read-only (get-bundle-price (bundle-id uint))
  (let (
    (bundle (unwrap! (map-get? menu-bundles { bundle-id: bundle-id }) u0))
    (total-price (fold calculate-bundle-item-price (list u1 u2 u3 u4 u5) { bundle-id: bundle-id, total: u0 }))
    (discount (get discount-percentage bundle))
  )
    (- (get total total-price) (/ (* (get total total-price) discount) u100))
  )
)

(define-read-only (get-bundle-savings (bundle-id uint))
  (match (map-get? menu-bundles { bundle-id: bundle-id })
    bundle
    (let (
      (individual-price (fold calculate-bundle-item-price (list u1 u2 u3 u4 u5) { bundle-id: bundle-id, total: u0 }))
      (bundle-price (get-bundle-price bundle-id))
      (discount (get discount-percentage bundle))
    )
      {
        individual-total: (get total individual-price),
        bundle-price: bundle-price,
        savings: (- (get total individual-price) bundle-price),
        discount-percentage: discount
      }
    )
    {
      individual-total: u0,
      bundle-price: u0,
      savings: u0,
      discount-percentage: u0
    }
  )
)

(define-read-only (get-bundle-analytics (bundle-id uint))
  (let (
    (bundle (unwrap! (map-get? menu-bundles { bundle-id: bundle-id }) none))
    (current-day (get-current-day))
    (daily-stats (map-get? bundle-daily-stats { bundle-id: bundle-id, day: current-day }))
  )
    (some {
      bundle: bundle,
      current-price: (get-bundle-price bundle-id),
      savings: (get-bundle-savings bundle-id),
      daily-stats: daily-stats
    })
  )
)

(define-read-only (get-bundle-count)
  (var-get bundle-counter)
)

(define-read-only (get-flash-sale (sale-id uint))
  (map-get? flash-sales { sale-id: sale-id })
)

(define-read-only (get-item-flash-sale (item-id uint))
  (match (map-get? item-flash-sale { item-id: item-id })
    sale-mapping
    (map-get? flash-sales { sale-id: (get sale-id sale-mapping) })
    none
  )
)

(define-read-only (get-flash-sale-price (item-id uint))
  (match (map-get? item-flash-sale { item-id: item-id })
    sale-mapping
    (let (
      (sale (unwrap! (map-get? flash-sales { sale-id: (get sale-id sale-mapping) }) u0))
      (item (unwrap! (map-get? menu-items { item-id: item-id }) u0))
      (base-price (get base-price item))
      (discount (get discount-percentage sale))
    )
      (if (and (get active sale) (<= stacks-block-height (get end-block sale)))
        (- base-price (/ (* base-price discount) u100))
        u0
      )
    )
    u0
  )
)

(define-read-only (get-flash-sale-status (item-id uint))
  (match (map-get? item-flash-sale { item-id: item-id })
    sale-mapping
    (let (
      (sale (unwrap! (map-get? flash-sales { sale-id: (get sale-id sale-mapping) }) none))
      (current-block stacks-block-height)
      (remaining-blocks (if (> (get end-block sale) current-block) (- (get end-block sale) current-block) u0))
      (remaining-quantity (- (get max-quantity sale) (get sold-quantity sale)))
    )
      (some {
        active: (and (get active sale) (<= current-block (get end-block sale))),
        discount-percentage: (get discount-percentage sale),
        remaining-blocks: remaining-blocks,
        remaining-quantity: remaining-quantity,
        sold-quantity: (get sold-quantity sale)
      })
    )
    none
  )
)

(define-read-only (get-flash-sale-count)
  (var-get flash-sale-counter)
)

(define-read-only (get-loyalty-status (customer principal))
  (map-get? customer-loyalty { customer: customer })
)

(define-read-only (get-loyalty-tier (customer principal))
  (match (map-get? customer-loyalty { customer: customer })
    loyalty-data
    (get tier loyalty-data)
    "none"
  )
)

(define-read-only (get-loyalty-discount (customer principal))
  (match (map-get? customer-loyalty { customer: customer })
    loyalty-data
    (let ((total-spent (get total-spent loyalty-data)))
      (if (>= total-spent GOLD-THRESHOLD)
        GOLD-DISCOUNT
        (if (>= total-spent SILVER-THRESHOLD)
          SILVER-DISCOUNT
          (if (>= total-spent BRONZE-THRESHOLD)
            BRONZE-DISCOUNT
            u0
          )
        )
      )
    )
    u0
  )
)

(define-read-only (get-next-tier-progress (customer principal))
  (match (map-get? customer-loyalty { customer: customer })
    loyalty-data
    (let ((total-spent (get total-spent loyalty-data)))
      (if (>= total-spent GOLD-THRESHOLD)
        { next-tier: "max", spent: total-spent, required: GOLD-THRESHOLD, remaining: u0 }
        (if (>= total-spent SILVER-THRESHOLD)
          { next-tier: "gold", spent: total-spent, required: GOLD-THRESHOLD, remaining: (- GOLD-THRESHOLD total-spent) }
          (if (>= total-spent BRONZE-THRESHOLD)
            { next-tier: "silver", spent: total-spent, required: SILVER-THRESHOLD, remaining: (- SILVER-THRESHOLD total-spent) }
            { next-tier: "bronze", spent: total-spent, required: BRONZE-THRESHOLD, remaining: (- BRONZE-THRESHOLD total-spent) }
          )
        )
      )
    )
    { next-tier: "bronze", spent: u0, required: BRONZE-THRESHOLD, remaining: BRONZE-THRESHOLD }
  )
)

;; private functions
(define-private (update-loyalty (customer principal) (amount-spent uint))
  (let (
    (current-block stacks-block-height)
    (existing-loyalty (map-get? customer-loyalty { customer: customer }))
  )
    (match existing-loyalty
      loyalty-data
      (let (
        (new-total-spent (+ (get total-spent loyalty-data) amount-spent))
        (new-tier (calculate-tier new-total-spent))
      )
        (map-set customer-loyalty
          { customer: customer }
          (merge loyalty-data {
            total-spent: new-total-spent,
            tier: new-tier,
            orders-count: (+ (get orders-count loyalty-data) u1),
            last-order-block: current-block
          })
        )
        (ok true)
      )
      (begin
        (map-set customer-loyalty
          { customer: customer }
          {
            total-spent: amount-spent,
            tier: (calculate-tier amount-spent),
            orders-count: u1,
            joined-block: current-block,
            last-order-block: current-block
          }
        )
        (ok true)
      )
    )
  )
)

(define-private (calculate-tier (total-spent uint))
  (if (>= total-spent GOLD-THRESHOLD)
    "gold"
    (if (>= total-spent SILVER-THRESHOLD)
      "silver"
      (if (>= total-spent BRONZE-THRESHOLD)
        "bronze"
        "none"
      )
    )
  )
)

(define-private (calculate-price-with-multipliers (base-price uint) (multiplier1 uint) (multiplier2 uint) (multiplier3 uint))
  (/ (* (* (* base-price multiplier1) multiplier2) multiplier3) u1000000)
)

(define-private (record-bundle-item-demand (bundle-id uint))
  (let ((current-day (get-current-day)))
    (fold record-single-item-from-bundle (list u1 u2 u3 u4 u5) { bundle-id: bundle-id, day: current-day })
    (ok true)
  )
)

(define-private (record-single-item-from-bundle (item-id uint) (data { bundle-id: uint, day: uint }))
  (let (
    (bundle-id (get bundle-id data))
    (day (get day data))
    (bundle-item-data (map-get? bundle-items { bundle-id: bundle-id, item-id: item-id }))
  )
    (match bundle-item-data
      item-data
      (let (
        (quantity (get quantity item-data))
        (current-demand (default-to { orders: u0, total-revenue: u0 }
                                   (map-get? daily-demand { item-id: item-id, day: day })))
      )
        (map-set daily-demand
          { item-id: item-id, day: day }
          {
            orders: (+ (get orders current-demand) quantity),
            total-revenue: (get total-revenue current-demand)
          }
        )
        data
      )
      data
    )
  )
)

(define-private (calculate-bundle-item-price (item-id uint) (data { bundle-id: uint, total: uint }))
  (let (
    (bundle-id (get bundle-id data))
    (current-total (get total data))
    (bundle-item-data (map-get? bundle-items { bundle-id: bundle-id, item-id: item-id }))
  )
    (match bundle-item-data
      item-data
      (let (
        (quantity (get quantity item-data))
        (item-price (get-dynamic-price item-id))
      )
        { bundle-id: bundle-id, total: (+ current-total (* item-price quantity)) }
      )
      data
    )
  )
)

(define-private (get-single-bundle-item (item-id uint) (data { bundle-id: uint, items: (list 10 uint) }))
  (let (
    (bundle-id (get bundle-id data))
    (current-items (get items data))
    (bundle-item-data (map-get? bundle-items { bundle-id: bundle-id, item-id: item-id }))
  )
    (if (is-some bundle-item-data)
      { bundle-id: bundle-id, items: (unwrap-panic (as-max-len? (append current-items item-id) u10)) }
      data
    )
  )
)
