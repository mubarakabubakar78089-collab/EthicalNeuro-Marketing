;; manipulation-detection-oracle.clar
;; Detect and flag potentially manipulative neuro-marketing techniques

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u200))
(define-constant ERR-NOT-FOUND (err u201))
(define-constant ERR-INVALID-INPUT (err u202))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map campaign-analysis
  { campaign-id: (string-ascii 50), analyzer: principal }
  {
    overall-risk-score: uint,
    analysis-timestamp: uint,
    status: uint
  }
)

(define-map authorized-analyzers
  principal
  {
    analyzer-name: (string-ascii 100),
    active: bool
  }
)

;; Public functions
(define-public (authorize-analyzer (analyzer principal) (name (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (ok (map-set authorized-analyzers analyzer
      {
        analyzer-name: name,
        active: true
      }
    ))
  )
)

(define-public (analyze-campaign (campaign-id (string-ascii 50)) (risk-score uint))
  (let (
    (analyzer-info (unwrap! (map-get? authorized-analyzers tx-sender) ERR-UNAUTHORIZED))
  )
    (asserts! (get active analyzer-info) ERR-UNAUTHORIZED)
    (asserts! (<= risk-score u100) ERR-INVALID-INPUT)
    
    (ok (map-set campaign-analysis
      { campaign-id: campaign-id, analyzer: tx-sender }
      {
        overall-risk-score: risk-score,
        analysis-timestamp: stacks-block-height,
        status: u21
      }
    ))
  )
)

;; Read-only functions
(define-read-only (get-campaign-analysis (campaign-id (string-ascii 50)) (analyzer principal))
  (map-get? campaign-analysis { campaign-id: campaign-id, analyzer: analyzer })
)

(define-read-only (is-authorized-analyzer (analyzer principal))
  (match (map-get? authorized-analyzers analyzer)
    info (get active info)
    false
  )
)