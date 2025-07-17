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

(define-constant PEAK-HOUR-START u11)
(define-constant PEAK-HOUR-END u14)
(define-constant EVENING-PEAK-START u18)
(define-constant EVENING-PEAK-END u21)
(define-constant BLOCKS-PER-HOUR u144)
(define-constant VOTING-PERIOD u1000)

;; data vars
(define-data-var contract-owner principal tx-sender)
(define-data-var base-surge-multiplier uint u150)
(define-data-var vote-threshold uint u10)
(define-data-var menu-item-counter uint u0)

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
    (current-price (get-dynamic-price item-id))
    (total-cost (* current-price quantity))
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

;; private functions
(define-private (calculate-price-with-multipliers (base-price uint) (multiplier1 uint) (multiplier2 uint) (multiplier3 uint))
  (/ (* (* (* base-price multiplier1) multiplier2) multiplier3) u1000000)
)
