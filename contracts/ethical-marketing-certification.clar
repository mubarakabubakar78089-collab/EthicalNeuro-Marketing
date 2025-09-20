;; ethical-marketing-certification.clar
;; Certify marketing campaigns as ethically compliant

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u300))
(define-constant ERR-NOT-FOUND (err u301))
(define-constant ERR-INVALID-INPUT (err u302))
(define-constant ERR-INSUFFICIENT-SCORE (err u305))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Certification levels
(define-constant CERT-BRONZE u1)
(define-constant CERT-SILVER u2)
(define-constant CERT-GOLD u3)
(define-constant CERT-PLATINUM u4)

;; Score thresholds
(define-constant BRONZE-MIN-SCORE u60)
(define-constant SILVER-MIN-SCORE u75)
(define-constant GOLD-MIN-SCORE u85)
(define-constant PLATINUM-MIN-SCORE u95)

;; Data structures
(define-map certifications
  { campaign-id: (string-ascii 50) }
  {
    organization: principal,
    certification-level: uint,
    overall-score: uint,
    issued-at: uint,
    issuer: principal
  }
)

(define-map auditors
  principal
  {
    auditor-name: (string-ascii 100),
    active: bool
  }
)

;; Public functions
(define-public (authorize-auditor (auditor principal) (name (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (ok (map-set auditors auditor
      {
        auditor-name: name,
        active: true
      }
    ))
  )
)

(define-public (issue-certification (campaign-id (string-ascii 50)) (organization principal) (overall-score uint))
  (let (
    (auditor-info (unwrap! (map-get? auditors tx-sender) ERR-UNAUTHORIZED))
    (cert-level (get-certification-level overall-score))
  )
    (asserts! (get active auditor-info) ERR-UNAUTHORIZED)
    (asserts! (>= overall-score BRONZE-MIN-SCORE) ERR-INSUFFICIENT-SCORE)
    
    (ok (map-set certifications
      { campaign-id: campaign-id }
      {
        organization: organization,
        certification-level: cert-level,
        overall-score: overall-score,
        issued-at: stacks-block-height,
        issuer: tx-sender
      }
    ))
  )
)

;; Private functions
(define-private (get-certification-level (score uint))
  (if (>= score PLATINUM-MIN-SCORE) CERT-PLATINUM
    (if (>= score GOLD-MIN-SCORE) CERT-GOLD
      (if (>= score SILVER-MIN-SCORE) CERT-SILVER
        CERT-BRONZE)))
)

;; Read-only functions
(define-read-only (get-certification (campaign-id (string-ascii 50)))
  (map-get? certifications { campaign-id: campaign-id })
)

(define-read-only (is-authorized-auditor (auditor principal))
  (match (map-get? auditors auditor)
    info (get active info)
    false
  )
)

(define-read-only (get-score-thresholds)
  {
    bronze: BRONZE-MIN-SCORE,
    silver: SILVER-MIN-SCORE,
    gold: GOLD-MIN-SCORE,
    platinum: PLATINUM-MIN-SCORE
  }
)