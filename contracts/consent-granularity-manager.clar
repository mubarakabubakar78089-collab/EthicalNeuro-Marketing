;; consent-granularity-manager.clar
;; Ultra-granular consent management for neurological data usage in marketing

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-INPUT (err u102))
(define-constant ERR-CONSENT-EXPIRED (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data types for consent granularity
(define-constant CONSENT-NEUROLOGICAL u1)
(define-constant CONSENT-BIOMETRIC u2)
(define-constant CONSENT-BEHAVIORAL u3)
(define-constant CONSENT-EMOTIONAL u4)
(define-constant CONSENT-COGNITIVE u5)
(define-constant CONSENT-PHYSIOLOGICAL u6)

;; Purpose types for data usage
(define-constant PURPOSE-RESEARCH u10)
(define-constant PURPOSE-ADVERTISING u11)
(define-constant PURPOSE-ANALYTICS u12)
(define-constant PURPOSE-PERSONALIZATION u13)
(define-constant PURPOSE-OPTIMIZATION u14)

;; Consent status types
(define-constant STATUS-GRANTED u20)
(define-constant STATUS-REVOKED u21)
(define-constant STATUS-EXPIRED u22)
(define-constant STATUS-PENDING u23)

;; Data structures
(define-map consent-records
  { user-id: principal, data-type: uint, purpose: uint }
  {
    status: uint,
    granted-at: uint,
    expires-at: uint,
    revoked-at: (optional uint),
    usage-count: uint,
    max-usage: uint
  }
)

(define-map user-preferences
  principal
  {
    default-expiry-days: uint,
    auto-renewal: bool,
    notification-enabled: bool,
    created-at: uint,
    updated-at: uint
  }
)

(define-map consent-audit-log
  { user-id: principal, timestamp: uint, action-id: uint }
  {
    action-type: (string-ascii 50),
    data-type: uint,
    purpose: uint,
    previous-status: (optional uint),
    new-status: uint,
    reason: (string-ascii 200)
  }
)

(define-map data-usage-log
  { user-id: principal, data-type: uint, purpose: uint, timestamp: uint }
  {
    usage-description: (string-ascii 200),
    processor: principal,
    compliance-verified: bool
  }
)

;; Counter for audit log entries
(define-data-var audit-counter uint u0)

;; Public functions

;; Initialize user preferences
(define-public (initialize-user-preferences (default-days uint) (auto-renew bool) (notifications bool))
  (begin
    (asserts! (is-none (map-get? user-preferences tx-sender)) ERR-ALREADY-EXISTS)
    (ok (map-set user-preferences tx-sender
      {
        default-expiry-days: default-days,
        auto-renewal: auto-renew,
        notification-enabled: notifications,
        created-at: stacks-block-height,
        updated-at: stacks-block-height
      }
    ))
  )
)

;; Grant consent for specific data type and purpose
(define-public (grant-consent (data-type uint) (purpose uint) (max-usage uint))
  (let (
    (user-prefs (default-to { default-expiry-days: u365, auto-renewal: false, notification-enabled: true, created-at: stacks-block-height, updated-at: stacks-block-height }
                   (map-get? user-preferences tx-sender)))
    (expiry-block (+ stacks-block-height (* (get default-expiry-days user-prefs) u144))) ;; Assuming ~144 blocks per day
    (audit-id (+ (var-get audit-counter) u1))
  )
    (asserts! (and (>= data-type CONSENT-NEUROLOGICAL) (<= data-type CONSENT-PHYSIOLOGICAL)) ERR-INVALID-INPUT)
    (asserts! (and (>= purpose PURPOSE-RESEARCH) (<= purpose PURPOSE-OPTIMIZATION)) ERR-INVALID-INPUT)
    (asserts! (> max-usage u0) ERR-INVALID-INPUT)
    
    ;; Set consent record
    (map-set consent-records
      { user-id: tx-sender, data-type: data-type, purpose: purpose }
      {
        status: STATUS-GRANTED,
        granted-at: stacks-block-height,
        expires-at: expiry-block,
        revoked-at: none,
        usage-count: u0,
        max-usage: max-usage
      }
    )
    
    ;; Log audit entry
    (map-set consent-audit-log
      { user-id: tx-sender, timestamp: stacks-block-height, action-id: audit-id }
      {
        action-type: "CONSENT_GRANTED",
        data-type: data-type,
        purpose: purpose,
        previous-status: none,
        new-status: STATUS-GRANTED,
        reason: "User granted consent"
      }
    )
    
    (var-set audit-counter audit-id)
    (ok audit-id)
  )
)

;; Revoke consent
(define-public (revoke-consent (data-type uint) (purpose uint) (reason (string-ascii 200)))
  (let (
    (consent-key { user-id: tx-sender, data-type: data-type, purpose: purpose })
    (current-consent (unwrap! (map-get? consent-records consent-key) ERR-NOT-FOUND))
    (audit-id (+ (var-get audit-counter) u1))
  )
    (asserts! (is-eq (get status current-consent) STATUS-GRANTED) ERR-INVALID-INPUT)
    
    ;; Update consent record
    (map-set consent-records consent-key
      (merge current-consent {
        status: STATUS-REVOKED,
        revoked-at: (some stacks-block-height)
      })
    )
    
    ;; Log audit entry
    (map-set consent-audit-log
      { user-id: tx-sender, timestamp: stacks-block-height, action-id: audit-id }
      {
        action-type: "CONSENT_REVOKED",
        data-type: data-type,
        purpose: purpose,
        previous-status: (some STATUS-GRANTED),
        new-status: STATUS-REVOKED,
        reason: reason
      }
    )
    
    (var-set audit-counter audit-id)
    (ok audit-id)
  )
)

;; Record data usage (called by authorized processors)
(define-public (record-data-usage (user-id principal) (data-type uint) (purpose uint) (description (string-ascii 200)))
  (let (
    (consent-key { user-id: user-id, data-type: data-type, purpose: purpose })
    (current-consent (unwrap! (map-get? consent-records consent-key) ERR-NOT-FOUND))
    (new-usage-count (+ (get usage-count current-consent) u1))
  )
    ;; Verify consent is valid
    (asserts! (is-eq (get status current-consent) STATUS-GRANTED) ERR-UNAUTHORIZED)
    (asserts! (< stacks-block-height (get expires-at current-consent)) ERR-CONSENT-EXPIRED)
    (asserts! (< (get usage-count current-consent) (get max-usage current-consent)) ERR-UNAUTHORIZED)
    
    ;; Update usage count
    (map-set consent-records consent-key
      (merge current-consent {
        usage-count: new-usage-count
      })
    )
    
    ;; Log data usage
    (map-set data-usage-log
      { user-id: user-id, data-type: data-type, purpose: purpose, timestamp: stacks-block-height }
      {
        usage-description: description,
        processor: tx-sender,
        compliance-verified: true
      }
    )
    
    (ok new-usage-count)
  )
)

;; Update user preferences
(define-public (update-preferences (default-days uint) (auto-renew bool) (notifications bool))
  (let (
    (current-prefs (unwrap! (map-get? user-preferences tx-sender) ERR-NOT-FOUND))
  )
    (ok (map-set user-preferences tx-sender
      (merge current-prefs {
        default-expiry-days: default-days,
        auto-renewal: auto-renew,
        notification-enabled: notifications,
        updated-at: stacks-block-height
      })
    ))
  )
)

;; Read-only functions

;; Check if consent is valid for specific data type and purpose
(define-read-only (is-consent-valid (user-id principal) (data-type uint) (purpose uint))
  (match (map-get? consent-records { user-id: user-id, data-type: data-type, purpose: purpose })
    consent-record
    (and
      (is-eq (get status consent-record) STATUS-GRANTED)
      (< stacks-block-height (get expires-at consent-record))
      (< (get usage-count consent-record) (get max-usage consent-record))
    )
    false
  )
)

;; Get consent details
(define-read-only (get-consent-details (user-id principal) (data-type uint) (purpose uint))
  (map-get? consent-records { user-id: user-id, data-type: data-type, purpose: purpose })
)

;; Get user preferences
(define-read-only (get-user-preferences (user-id principal))
  (map-get? user-preferences user-id)
)

;; Get consent audit log entry
(define-read-only (get-audit-entry (user-id principal) (timestamp uint) (action-id uint))
  (map-get? consent-audit-log { user-id: user-id, timestamp: timestamp, action-id: action-id })
)

;; Get data usage log entry
(define-read-only (get-usage-log (user-id principal) (data-type uint) (purpose uint) (timestamp uint))
  (map-get? data-usage-log { user-id: user-id, data-type: data-type, purpose: purpose, timestamp: timestamp })
)

;; Check if consent exists
(define-read-only (consent-exists (user-id principal) (data-type uint) (purpose uint))
  (is-some (map-get? consent-records { user-id: user-id, data-type: data-type, purpose: purpose }))
)

;; Get current audit counter
(define-read-only (get-audit-counter)
  (var-get audit-counter)
)

;; Get consent status
(define-read-only (get-consent-status (user-id principal) (data-type uint) (purpose uint))
  (match (map-get? consent-records { user-id: user-id, data-type: data-type, purpose: purpose })
    consent-record (some (get status consent-record))
    none
  )
)

;; Admin functions (only contract owner)

;; Emergency revoke (for compliance issues)
(define-public (emergency-revoke-consent (user-id principal) (data-type uint) (purpose uint) (reason (string-ascii 200)))
  (let (
    (consent-key { user-id: user-id, data-type: data-type, purpose: purpose })
    (current-consent (unwrap! (map-get? consent-records consent-key) ERR-NOT-FOUND))
    (audit-id (+ (var-get audit-counter) u1))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Update consent record
    (map-set consent-records consent-key
      (merge current-consent {
        status: STATUS-REVOKED,
        revoked-at: (some stacks-block-height)
      })
    )
    
    ;; Log audit entry
    (map-set consent-audit-log
      { user-id: user-id, timestamp: stacks-block-height, action-id: audit-id }
      {
        action-type: "EMERGENCY_REVOKE",
        data-type: data-type,
        purpose: purpose,
        previous-status: (some (get status current-consent)),
        new-status: STATUS-REVOKED,
        reason: reason
      }
    )
    
    (var-set audit-counter audit-id)
    (ok audit-id)
  )
)