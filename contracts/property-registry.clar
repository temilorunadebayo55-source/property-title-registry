
;; title: property-registry
;; version: 1.0.0
;; summary: Digital property title management system
;; description: Core registry for managing property titles, ownership, and transfers

;; constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPERTY-NOT-FOUND (err u101))
(define-constant ERR-PROPERTY-ALREADY-EXISTS (err u102))
(define-constant ERR-NOT-OWNER (err u103))
(define-constant ERR-INVALID-PROPERTY-ID (err u104))
(define-constant ERR-TRANSFER-FAILED (err u105))
(define-constant CONTRACT-OWNER tx-sender)

;; data vars
(define-data-var next-property-id uint u1)
(define-data-var registry-fee uint u1000000) ;; 1 STX in microSTX

;; data maps
(define-map properties
    { property-id: uint }
    {
        owner: principal,
        address: (string-ascii 200),
        legal-description: (string-ascii 500),
        property-type: (string-ascii 50),
        square-footage: uint,
        assessed-value: uint,
        registration-date: uint,
        last-transfer-date: uint,
        is-active: bool
    }
)

(define-map property-history
    { property-id: uint, transaction-id: uint }
    {
        from-owner: (optional principal),
        to-owner: principal,
        transfer-date: uint,
        transfer-price: uint,
        transaction-type: (string-ascii 20)
    }
)

(define-map owner-properties
    { owner: principal }
    { property-ids: (list 100 uint) }
)

(define-map property-transaction-count
    { property-id: uint }
    { count: uint }
)

;; public functions
(define-public (register-property
    (address (string-ascii 200))
    (legal-description (string-ascii 500))
    (property-type (string-ascii 50))
    (square-footage uint)
    (assessed-value uint)
    )
    (let
        (
            (property-id (var-get next-property-id))
        )
        ;; Collect registration fee
        (try! (stx-transfer? (var-get registry-fee) tx-sender CONTRACT-OWNER))
        
        ;; Register property
        (map-set properties
            { property-id: property-id }
            {
                owner: tx-sender,
                address: address,
                legal-description: legal-description,
                property-type: property-type,
                square-footage: square-footage,
                assessed-value: assessed-value,
                registration-date: stacks-block-height,
                last-transfer-date: stacks-block-height,
                is-active: true
            }
        )
        
        ;; Add to owner's property list
        (let ((current-properties (default-to { property-ids: (list) } 
                                    (map-get? owner-properties { owner: tx-sender }))))
            (map-set owner-properties
                { owner: tx-sender }
                { property-ids: (unwrap! (as-max-len? 
                    (append (get property-ids current-properties) property-id) u100) 
                    ERR-TRANSFER-FAILED) }
            )
        )
        
        ;; Record initial registration in history
        (map-set property-history
            { property-id: property-id, transaction-id: u1 }
            {
                from-owner: none,
                to-owner: tx-sender,
                transfer-date: stacks-block-height,
                transfer-price: u0,
                transaction-type: "REGISTRATION"
            }
        )
        
        ;; Initialize transaction count
        (map-set property-transaction-count
            { property-id: property-id }
            { count: u1 }
        )
        
        ;; Increment next property ID
        (var-set next-property-id (+ property-id u1))
        
        (ok property-id)
    )
)

(define-public (transfer-property
    (property-id uint)
    (new-owner principal)
    (transfer-price uint)
    )
    (let
        (
            (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
            (current-owner (get owner property))
            (transaction-count (default-to { count: u0 } 
                               (map-get? property-transaction-count { property-id: property-id })))
            (next-transaction-id (+ (get count transaction-count) u1))
        )
        ;; Verify ownership
        (asserts! (is-eq tx-sender current-owner) ERR-NOT-OWNER)
        
        ;; Update property ownership
        (map-set properties
            { property-id: property-id }
            (merge property {
                owner: new-owner,
                last-transfer-date: stacks-block-height
            })
        )
        
        ;; Remove from current owner's list
        (let ((current-owner-properties (unwrap! (map-get? owner-properties { owner: current-owner }) 
                                                 ERR-TRANSFER-FAILED)))
            (map-set owner-properties
                { owner: current-owner }
                { property-ids: (filter is-not-property-id (get property-ids current-owner-properties)) }
            )
        )
        
        ;; Add to new owner's list
        (let ((new-owner-properties (default-to { property-ids: (list) } 
                                    (map-get? owner-properties { owner: new-owner }))))
            (map-set owner-properties
                { owner: new-owner }
                { property-ids: (unwrap! (as-max-len? 
                    (append (get property-ids new-owner-properties) property-id) u100) 
                    ERR-TRANSFER-FAILED) }
            )
        )
        
        ;; Record transfer in history
        (map-set property-history
            { property-id: property-id, transaction-id: next-transaction-id }
            {
                from-owner: (some current-owner),
                to-owner: new-owner,
                transfer-date: stacks-block-height,
                transfer-price: transfer-price,
                transaction-type: "TRANSFER"
            }
        )
        
        ;; Update transaction count
        (map-set property-transaction-count
            { property-id: property-id }
            { count: next-transaction-id }
        )
        
        (ok true)
    )
)

(define-public (update-property-value
    (property-id uint)
    (new-assessed-value uint)
    )
    (let
        (
            (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
        )
        ;; Verify ownership
        (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-OWNER)
        
        ;; Update assessed value
        (map-set properties
            { property-id: property-id }
            (merge property { assessed-value: new-assessed-value })
        )
        
        (ok true)
    )
)

(define-public (deactivate-property (property-id uint))
    (let
        (
            (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
        )
        ;; Verify ownership
        (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-OWNER)
        
        ;; Deactivate property
        (map-set properties
            { property-id: property-id }
            (merge property { is-active: false })
        )
        
        (ok true)
    )
)

;; read only functions
(define-read-only (get-property (property-id uint))
    (map-get? properties { property-id: property-id })
)

(define-read-only (get-property-owner (property-id uint))
    (match (map-get? properties { property-id: property-id })
        property (some (get owner property))
        none
    )
)

(define-read-only (get-owner-properties (owner principal))
    (default-to { property-ids: (list) } (map-get? owner-properties { owner: owner }))
)

(define-read-only (get-property-history (property-id uint) (transaction-id uint))
    (map-get? property-history { property-id: property-id, transaction-id: transaction-id })
)

(define-read-only (get-property-transaction-count (property-id uint))
    (default-to { count: u0 } (map-get? property-transaction-count { property-id: property-id }))
)

(define-read-only (get-next-property-id)
    (var-get next-property-id)
)

(define-read-only (get-registry-fee)
    (var-get registry-fee)
)

;; private functions
(define-private (is-not-property-id (id uint))
    (not (is-eq id (var-get next-property-id)))
)
