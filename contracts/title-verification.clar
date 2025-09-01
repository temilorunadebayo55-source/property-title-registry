
;; title: title-verification
;; version: 1.0.0
;; summary: Property title verification and validation system
;; description: Provides verification services for property titles and ownership validation

;; constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-PROPERTY-NOT-FOUND (err u201))
(define-constant ERR-VERIFICATION-NOT-FOUND (err u202))
(define-constant ERR-ALREADY-VERIFIED (err u203))
(define-constant ERR-INVALID-VERIFIER (err u204))
(define-constant ERR-VERIFICATION-EXPIRED (err u205))
(define-constant CONTRACT-OWNER tx-sender)
(define-constant VERIFICATION-VALIDITY-PERIOD u1440) ;; ~10 days in blocks

;; data vars
(define-data-var next-verification-id uint u1)
(define-data-var verification-fee uint u500000) ;; 0.5 STX in microSTX

;; data maps
(define-map authorized-verifiers
    { verifier: principal }
    {
        is-authorized: bool,
        specialization: (string-ascii 100),
        verification-count: uint,
        authorization-date: uint
    }
)

(define-map property-verifications
    { property-id: uint, verification-id: uint }
    {
        verifier: principal,
        verification-type: (string-ascii 50),
        verification-date: uint,
        expiry-date: uint,
        status: (string-ascii 20),
        notes: (string-ascii 500),
        is-valid: bool
    }
)

(define-map verification-requests
    { request-id: uint }
    {
        requester: principal,
        property-id: uint,
        verification-type: (string-ascii 50),
        request-date: uint,
        assigned-verifier: (optional principal),
        status: (string-ascii 20)
    }
)

(define-map property-verification-count
    { property-id: uint }
    { count: uint }
)

;; public functions
(define-public (authorize-verifier (verifier principal) (specialization (string-ascii 100)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set authorized-verifiers
            { verifier: verifier }
            {
                is-authorized: true,
                specialization: specialization,
                verification-count: u0,
                authorization-date: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (request-verification
    (property-id uint)
    (verification-type (string-ascii 50))
    )
    (let
        (
            (request-id (var-get next-verification-id))
        )
        ;; Collect verification fee
        (try! (stx-transfer? (var-get verification-fee) tx-sender CONTRACT-OWNER))
        
        ;; Create verification request
        (map-set verification-requests
            { request-id: request-id }
            {
                requester: tx-sender,
                property-id: property-id,
                verification-type: verification-type,
                request-date: stacks-block-height,
                assigned-verifier: none,
                status: "PENDING"
            }
        )
        
        ;; Increment request ID
        (var-set next-verification-id (+ request-id u1))
        
        (ok request-id)
    )
)

(define-public (assign-verifier
    (request-id uint)
    (verifier principal)
    )
    (let
        (
            (request (unwrap! (map-get? verification-requests { request-id: request-id }) 
                              ERR-VERIFICATION-NOT-FOUND))
            (verifier-info (unwrap! (map-get? authorized-verifiers { verifier: verifier }) 
                                    ERR-INVALID-VERIFIER))
        )
        ;; Only contract owner can assign verifiers
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        ;; Verify the verifier is authorized
        (asserts! (get is-authorized verifier-info) ERR-INVALID-VERIFIER)
        
        ;; Update request with assigned verifier
        (map-set verification-requests
            { request-id: request-id }
            (merge request {
                assigned-verifier: (some verifier),
                status: "ASSIGNED"
            })
        )
        
        (ok true)
    )
)

(define-public (complete-verification
    (request-id uint)
    (property-id uint)
    (verification-result bool)
    (notes (string-ascii 500))
    )
    (let
        (
            (request (unwrap! (map-get? verification-requests { request-id: request-id }) 
                              ERR-VERIFICATION-NOT-FOUND))
            (verifier-info (unwrap! (map-get? authorized-verifiers { verifier: tx-sender }) 
                                    ERR-INVALID-VERIFIER))
            (verification-count (default-to { count: u0 } 
                                (map-get? property-verification-count { property-id: property-id })))
            (new-verification-id (+ (get count verification-count) u1))
        )
        ;; Verify the caller is the assigned verifier
        (asserts! (is-eq (some tx-sender) (get assigned-verifier request)) ERR-NOT-AUTHORIZED)
        
        ;; Verify property ID matches
        (asserts! (is-eq property-id (get property-id request)) ERR-PROPERTY-NOT-FOUND)
        
        ;; Create verification record
        (map-set property-verifications
            { property-id: property-id, verification-id: new-verification-id }
            {
                verifier: tx-sender,
                verification-type: (get verification-type request),
                verification-date: stacks-block-height,
                expiry-date: (+ stacks-block-height VERIFICATION-VALIDITY-PERIOD),
                status: (if verification-result "VERIFIED" "FAILED"),
                notes: notes,
                is-valid: verification-result
            }
        )
        
        ;; Update property verification count
        (map-set property-verification-count
            { property-id: property-id }
            { count: new-verification-id }
        )
        
        ;; Update request status
        (map-set verification-requests
            { request-id: request-id }
            (merge request { status: "COMPLETED" })
        )
        
        ;; Update verifier's completion count
        (map-set authorized-verifiers
            { verifier: tx-sender }
            (merge verifier-info {
                verification-count: (+ (get verification-count verifier-info) u1)
            })
        )
        
        (ok true)
    )
)

(define-public (revoke-verifier (verifier principal))
    (let
        (
            (verifier-info (unwrap! (map-get? authorized-verifiers { verifier: verifier }) 
                                    ERR-INVALID-VERIFIER))
        )
        ;; Only contract owner can revoke authorization
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        ;; Revoke authorization
        (map-set authorized-verifiers
            { verifier: verifier }
            (merge verifier-info { is-authorized: false })
        )
        
        (ok true)
    )
)

;; read only functions
(define-read-only (is-verifier-authorized (verifier principal))
    (match (map-get? authorized-verifiers { verifier: verifier })
        verifier-info (get is-authorized verifier-info)
        false
    )
)

(define-read-only (get-verifier-info (verifier principal))
    (map-get? authorized-verifiers { verifier: verifier })
)

(define-read-only (get-property-verification (property-id uint) (verification-id uint))
    (map-get? property-verifications { property-id: property-id, verification-id: verification-id })
)

(define-read-only (get-verification-request (request-id uint))
    (map-get? verification-requests { request-id: request-id })
)

(define-read-only (get-property-verification-count (property-id uint))
    (default-to { count: u0 } (map-get? property-verification-count { property-id: property-id }))
)

(define-read-only (is-property-verified (property-id uint))
    (let
        (
            (verification-count (get-property-verification-count property-id))
        )
        (if (> (get count verification-count) u0)
            (let
                (
                    (latest-verification (get-property-verification property-id (get count verification-count)))
                )
                (match latest-verification
                    verification (and 
                                   (get is-valid verification)
                                   (> (get expiry-date verification) stacks-block-height))
                    false
                )
            )
            false
        )
    )
)

(define-read-only (get-verification-fee)
    (var-get verification-fee)
)

(define-read-only (get-next-verification-id)
    (var-get next-verification-id)
)

;; private functions
(define-private (is-verification-expired (expiry-date uint))
    (<= expiry-date stacks-block-height)
)
