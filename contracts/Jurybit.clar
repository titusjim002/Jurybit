(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ALREADY_REGISTERED (err u101))
(define-constant ERR_NOT_REGISTERED (err u102))
(define-constant ERR_CASE_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_NOT_JURY_MEMBER (err u105))
(define-constant ERR_CASE_CLOSED (err u106))
(define-constant ERR_INSUFFICIENT_STAKE (err u107))
(define-constant ERR_CASE_STILL_ACTIVE (err u108))
(define-constant ERR_INVALID_VOTE (err u109))

(define-constant MIN_STAKE u1000000)
(define-constant JURY_SIZE u5)
(define-constant VOTING_PERIOD u144)
(define-constant CASE_FEE u500000)

(define-data-var next-case-id uint u1)
(define-data-var total-jurors uint u0)

(define-map jurors principal 
  {
    stake: uint,
    cases-served: uint,
    reputation: uint,
    active: bool
  }
)

(define-map cases uint
  {
    plaintiff: principal,
    defendant: principal,
    description: (string-ascii 500),
    stake-amount: uint,
    created-at: uint,
    voting-ends: uint,
    status: (string-ascii 20),
    votes-for-plaintiff: uint,
    votes-for-defendant: uint,
    total-votes: uint
  }
)

(define-map case-jury uint (list 5 principal))

(define-map jury-votes {case-id: uint, juror: principal}
  {
    vote: (string-ascii 10),
    voted-at: uint
  }
)

(define-map juror-list uint principal)

(define-public (register-as-juror (stake-amount uint))
  (let
    (
      (caller tx-sender)
    )
    (asserts! (>= stake-amount MIN_STAKE) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none (map-get? jurors caller)) ERR_ALREADY_REGISTERED)
    (try! (stx-transfer? stake-amount caller (as-contract tx-sender)))
    (map-set jurors caller
      {
        stake: stake-amount,
        cases-served: u0,
        reputation: u100,
        active: true
      }
    )
    (map-set juror-list (var-get total-jurors) caller)
    (var-set total-jurors (+ (var-get total-jurors) u1))
    (ok true)
  )
)

(define-public (create-case (defendant principal) (description (string-ascii 500)) (stake-amount uint))
  (let
    (
      (case-id (var-get next-case-id))
      (caller tx-sender)
      (current-block stacks-block-height)
    )
    (asserts! (>= stake-amount CASE_FEE) ERR_INSUFFICIENT_STAKE)
    (try! (stx-transfer? stake-amount caller (as-contract tx-sender)))
    (map-set cases case-id
      {
        plaintiff: caller,
        defendant: defendant,
        description: description,
        stake-amount: stake-amount,
        created-at: current-block,
        voting-ends: (+ current-block VOTING_PERIOD),
        status: "active",
        votes-for-plaintiff: u0,
        votes-for-defendant: u0,
        total-votes: u0
      }
    )
    (try! (select-jury case-id))
    (var-set next-case-id (+ case-id u1))
    (ok case-id)
  )
)

(define-private (select-jury (case-id uint))
  (let
    (
      (total-jurors-count (var-get total-jurors))
      (seed (+ case-id stacks-block-height))
    )
    (asserts! (>= total-jurors-count JURY_SIZE) ERR_NOT_REGISTERED)
    (let
      (
        (selected-jury (generate-jury-list seed total-jurors-count))
      )
      (map-set case-jury case-id selected-jury)
      (ok true)
    )
  )
)

(define-private (generate-jury-list (seed uint) (total-count uint))
  (let
    (
      (juror1 (get-juror-by-index (mod (+ seed u1) total-count)))
      (juror2 (get-juror-by-index (mod (+ seed u17) total-count)))
      (juror3 (get-juror-by-index (mod (+ seed u37) total-count)))
      (juror4 (get-juror-by-index (mod (+ seed u53) total-count)))
      (juror5 (get-juror-by-index (mod (+ seed u71) total-count)))
    )
    (list juror1 juror2 juror3 juror4 juror5)
  )
)

(define-private (get-juror-by-index (index uint))
  (default-to CONTRACT_OWNER (map-get? juror-list index))
)

(define-public (vote-on-case (case-id uint) (vote (string-ascii 10)))
  (let
    (
      (caller tx-sender)
      (case-data (unwrap! (map-get? cases case-id) ERR_CASE_NOT_FOUND))
      (jury-members (unwrap! (map-get? case-jury case-id) ERR_CASE_NOT_FOUND))
    )
    (asserts! (is-eq (get status case-data) "active") ERR_CASE_CLOSED)
    (asserts! (<= stacks-block-height (get voting-ends case-data)) ERR_CASE_CLOSED)
    (asserts! (is-some (index-of jury-members caller)) ERR_NOT_JURY_MEMBER)
    (asserts! (is-none (map-get? jury-votes {case-id: case-id, juror: caller})) ERR_ALREADY_VOTED)
    (asserts! (or (is-eq vote "plaintiff") (is-eq vote "defendant")) ERR_INVALID_VOTE)
    
    (map-set jury-votes {case-id: case-id, juror: caller}
      {
        vote: vote,
        voted-at: stacks-block-height
      }
    )
    
    (let
      (
        (updated-case (if (is-eq vote "plaintiff")
          (merge case-data {
            votes-for-plaintiff: (+ (get votes-for-plaintiff case-data) u1),
            total-votes: (+ (get total-votes case-data) u1)
          })
          (merge case-data {
            votes-for-defendant: (+ (get votes-for-defendant case-data) u1),
            total-votes: (+ (get total-votes case-data) u1)
          })
        ))
      )
      (map-set cases case-id updated-case)
      (try! (update-juror-stats caller))
      (ok true)
    )
  )
)

(define-private (update-juror-stats (juror principal))
  (let
    (
      (juror-data (unwrap! (map-get? jurors juror) ERR_NOT_REGISTERED))
    )
    (map-set jurors juror
      (merge juror-data {
        cases-served: (+ (get cases-served juror-data) u1),
        reputation: (+ (get reputation juror-data) u10)
      })
    )
    (ok true)
  )
)

(define-public (finalize-case (case-id uint))
  (let
    (
      (case-data (unwrap! (map-get? cases case-id) ERR_CASE_NOT_FOUND))
    )
    (asserts! (is-eq (get status case-data) "active") ERR_CASE_CLOSED)
    (asserts! (> stacks-block-height (get voting-ends case-data)) ERR_CASE_STILL_ACTIVE)
    
    (let
      (
        (winner (if (> (get votes-for-plaintiff case-data) (get votes-for-defendant case-data))
          (get plaintiff case-data)
          (get defendant case-data)
        ))
        (final-status (if (> (get votes-for-plaintiff case-data) (get votes-for-defendant case-data))
          "plaintiff-wins"
          "defendant-wins"
        ))
      )
      (map-set cases case-id (merge case-data {status: final-status}))
      (try! (distribute-rewards case-id winner))
      (ok winner)
    )
  )
)

(define-private (distribute-rewards (case-id uint) (winner principal))
  (let
    (
      (case-data (unwrap! (map-get? cases case-id) ERR_CASE_NOT_FOUND))
      (reward-amount (/ (get stake-amount case-data) u2))
    )
    (try! (as-contract (stx-transfer? reward-amount tx-sender winner)))
    (try! (distribute-jury-rewards case-id (/ reward-amount JURY_SIZE)))
    (ok true)
  )
)

(define-private (distribute-jury-rewards (case-id uint) (reward-per-juror uint))
  (let
    (
      (jury-members (unwrap! (map-get? case-jury case-id) ERR_CASE_NOT_FOUND))
    )
    (fold distribute-single-reward jury-members (ok reward-per-juror))
  )
)

(define-private (distribute-single-reward (juror principal) (reward-result (response uint uint)))
  (match reward-result
    reward-amount
      (match (as-contract (stx-transfer? reward-amount tx-sender juror))
        success (ok reward-amount)
        error-val (err error-val)
      )
    error-val (err error-val)
  )
)

(define-public (withdraw-stake)
  (let
    (
      (caller tx-sender)
      (juror-data (unwrap! (map-get? jurors caller) ERR_NOT_REGISTERED))
    )
    (try! (as-contract (stx-transfer? (get stake juror-data) tx-sender caller)))
    (map-delete jurors caller)
    (ok true)
  )
)

(define-read-only (get-case (case-id uint))
  (map-get? cases case-id)
)

(define-read-only (get-juror (juror principal))
  (map-get? jurors juror)
)

(define-read-only (get-case-jury (case-id uint))
  (map-get? case-jury case-id)
)

(define-read-only (get-jury-vote (case-id uint) (juror principal))
  (map-get? jury-votes {case-id: case-id, juror: juror})
)

(define-read-only (get-total-jurors)
  (var-get total-jurors)
)

(define-read-only (get-next-case-id)
  (var-get next-case-id)
)

(define-read-only (is-case-active (case-id uint))
  (match (map-get? cases case-id)
    case-data (and 
      (is-eq (get status case-data) "active")
      (<= stacks-block-height (get voting-ends case-data))
    )
    false
  )
)