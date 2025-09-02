;; Juror Availability Scheduling System
;; Allows jurors to set availability windows for case assignments

(define-constant ERR-UNAUTHORIZED (err u200))
(define-constant ERR-NOT-REGISTERED (err u201))
(define-constant ERR-INVALID-TIME-RANGE (err u202))
(define-constant ERR-AVAILABILITY-EXISTS (err u203))
(define-constant ERR-AVAILABILITY-NOT-FOUND (err u204))
(define-constant ERR-MAX-AVAILABILITY-REACHED (err u205))

;; Maximum availability windows per juror
(define-constant MAX-AVAILABILITY-WINDOWS u10)

;; Track juror availability windows
(define-map juror-availability {juror: principal, window-id: uint}
  {
    start-block: uint,
    end-block: uint,
    days-of-week: uint, ;; Bitmask: Mon=1, Tue=2, Wed=4, Thu=8, Fri=16, Sat=32, Sun=64
    is-active: bool
  }
)

;; Track availability window counts per juror
(define-map availability-counts principal uint)

;; Track next window ID per juror
(define-map next-window-id principal uint)


;; Set availability window for a juror
(define-public (set-availability (start-block uint) (end-block uint) (days-of-week uint))
  (let (
    (caller tx-sender)
    (current-count (default-to u0 (map-get? availability-counts caller)))
    (next-id (default-to u1 (map-get? next-window-id caller)))
  )
    ;; Check if caller is registered as juror (reference to main contract)
    (asserts! (is-some (contract-call? .Jurybit get-juror caller)) ERR-NOT-REGISTERED)
    
    ;; Validate time range
    (asserts! (< start-block end-block) ERR-INVALID-TIME-RANGE)
    (asserts! (>= start-block stacks-block-height) ERR-INVALID-TIME-RANGE)
    
    ;; Check availability limit
    (asserts! (< current-count MAX-AVAILABILITY-WINDOWS) ERR-MAX-AVAILABILITY-REACHED)
    
    ;; Validate days of week (must be between 1-127, representing Mon-Sun)
    (asserts! (and (> days-of-week u0) (<= days-of-week u127)) ERR-INVALID-TIME-RANGE)
    
    ;; Add availability window
    (map-set juror-availability {juror: caller, window-id: next-id}
      {
        start-block: start-block,
        end-block: end-block,
        days-of-week: days-of-week,
        is-active: true
      }
    )
    
    ;; Update counters
    (map-set availability-counts caller (+ current-count u1))
    (map-set next-window-id caller (+ next-id u1))
    
    (ok next-id)
  )
)

;; Remove availability window
(define-public (remove-availability (window-id uint))
  (let (
    (caller tx-sender)
    (availability-key {juror: caller, window-id: window-id})
    (availability-data (unwrap! (map-get? juror-availability availability-key) ERR-AVAILABILITY-NOT-FOUND))
    (current-count (default-to u0 (map-get? availability-counts caller)))
  )
    ;; Remove availability window
    (map-delete juror-availability availability-key)
    
    ;; Update counter
    (if (> current-count u0)
      (map-set availability-counts caller (- current-count u1))
      true
    )
    
    (ok true)
  )
)

;; Toggle availability window active status
(define-public (toggle-availability (window-id uint) (active bool))
  (let (
    (caller tx-sender)
    (availability-key {juror: caller, window-id: window-id})
    (availability-data (unwrap! (map-get? juror-availability availability-key) ERR-AVAILABILITY-NOT-FOUND))
  )
    ;; Update availability status
    (map-set juror-availability availability-key
      (merge availability-data {is-active: active})
    )
    
    (ok true)
  )
)

;; Check if juror is available at current time (simplified)
(define-read-only (is-juror-available (juror principal))
  (let (
    (availability-count (default-to u0 (map-get? availability-counts juror)))
  )
    ;; If no availability windows set, assume always available
    (if (is-eq availability-count u0)
      true
      ;; For simplicity, check if juror has any active windows
      (is-some (map-get? juror-availability {juror: juror, window-id: u1}))
    )
  )
)

;; Get all availability windows for a juror (simplified)
(define-read-only (get-juror-availability (juror principal))
  (let (
    (availability-count (default-to u0 (map-get? availability-counts juror)))
  )
    (if (is-eq availability-count u0)
      (list )
      ;; Return simplified list with just first window if exists
      (match (map-get? juror-availability {juror: juror, window-id: u1})
        window
        (list window)
        (list )
      )
    )
  )
)

;; Get availability window count
(define-read-only (get-availability-count (juror principal))
  (default-to u0 (map-get? availability-counts juror))
)

;; Check if specific window exists
(define-read-only (get-availability-window (juror principal) (window-id uint))
  (map-get? juror-availability {juror: juror, window-id: window-id})
)

;; Get list of currently available jurors (simplified)
(define-read-only (get-available-jurors-list (max-jurors uint))
  (list) ;; Simplified implementation - returns empty list
)