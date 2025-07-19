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
(define-constant ERR_NOT_APPEALABLE (err u110))
(define-constant ERR_APPEAL_PERIOD_EXPIRED (err u111))
(define-constant ERR_ALREADY_APPEALED (err u112))
(define-constant ERR_INSUFFICIENT_APPEAL_STAKE (err u113))
(define-constant ERR_APPEAL_NOT_FOUND (err u114))
(define-constant ERR_APPEAL_STILL_ACTIVE (err u115))

(define-constant MIN_STAKE u1000000)
(define-constant JURY_SIZE u5)
(define-constant VOTING_PERIOD u144)
(define-constant CASE_FEE u500000)
(define-constant APPEAL_JURY_SIZE u7)
(define-constant APPEAL_PERIOD u72)
(define-constant APPEAL_VOTING_PERIOD u216)
(define-constant APPEAL_MULTIPLIER u3)

(define-data-var next-case-id uint u1)
(define-data-var next-appeal-id uint u1)
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

(define-map appeals uint
  {
    original-case-id: uint,
    appellant: principal,
    appeal-stake: uint,
    created-at: uint,
    voting-ends: uint,
    status: (string-ascii 20),
    votes-for-original: uint,
    votes-for-reverse: uint,
    total-votes: uint,
    final-decision: (string-ascii 20)
  }
)

(define-map appeal-jury uint (list 7 principal))

(define-map appeal-votes {appeal-id: uint, juror: principal}
  {
    vote: (string-ascii 10),
    voted-at: uint
  }
)

(define-map case-appeals uint uint)

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

(define-public (appeal-case (case-id uint))
  (let
    (
      (caller tx-sender)
      (case-data (unwrap! (map-get? cases case-id) ERR_CASE_NOT_FOUND))
      (appeal-id (var-get next-appeal-id))
      (appeal-stake (* (get stake-amount case-data) APPEAL_MULTIPLIER))
      (current-block stacks-block-height)
    )
    (asserts! (not (is-eq (get status case-data) "active")) ERR_CASE_STILL_ACTIVE)
    (asserts! (or (is-eq (get plaintiff case-data) caller) (is-eq (get defendant case-data) caller)) ERR_UNAUTHORIZED)
    (asserts! (<= current-block (+ (get voting-ends case-data) APPEAL_PERIOD)) ERR_APPEAL_PERIOD_EXPIRED)
    (asserts! (is-none (map-get? case-appeals case-id)) ERR_ALREADY_APPEALED)
    (asserts! (>= (stx-get-balance caller) appeal-stake) ERR_INSUFFICIENT_APPEAL_STAKE)
    
    (try! (stx-transfer? appeal-stake caller (as-contract tx-sender)))
    
    (map-set appeals appeal-id
      {
        original-case-id: case-id,
        appellant: caller,
        appeal-stake: appeal-stake,
        created-at: current-block,
        voting-ends: (+ current-block APPEAL_VOTING_PERIOD),
        status: "active",
        votes-for-original: u0,
        votes-for-reverse: u0,
        total-votes: u0,
        final-decision: "pending"
      }
    )
    
    (map-set case-appeals case-id appeal-id)
    (try! (select-appeal-jury appeal-id))
    (var-set next-appeal-id (+ appeal-id u1))
    (ok appeal-id)
  )
)

(define-private (select-appeal-jury (appeal-id uint))
  (let
    (
      (total-jurors-count (var-get total-jurors))
      (seed (+ appeal-id stacks-block-height u789))
    )
    (asserts! (>= total-jurors-count APPEAL_JURY_SIZE) ERR_NOT_REGISTERED)
    (let
      (
        (selected-jury (generate-appeal-jury-list seed total-jurors-count))
      )
      (map-set appeal-jury appeal-id selected-jury)
      (ok true)
    )
  )
)

(define-private (generate-appeal-jury-list (seed uint) (total-count uint))
  (let
    (
      (juror1 (get-juror-by-index (mod (+ seed u11) total-count)))
      (juror2 (get-juror-by-index (mod (+ seed u23) total-count)))
      (juror3 (get-juror-by-index (mod (+ seed u41) total-count)))
      (juror4 (get-juror-by-index (mod (+ seed u67) total-count)))
      (juror5 (get-juror-by-index (mod (+ seed u89) total-count)))
      (juror6 (get-juror-by-index (mod (+ seed u101) total-count)))
      (juror7 (get-juror-by-index (mod (+ seed u127) total-count)))
    )
    (list juror1 juror2 juror3 juror4 juror5 juror6 juror7)
  )
)

(define-public (vote-on-appeal (appeal-id uint) (vote (string-ascii 10)))
  (let
    (
      (caller tx-sender)
      (appeal-data (unwrap! (map-get? appeals appeal-id) ERR_APPEAL_NOT_FOUND))
      (jury-members (unwrap! (map-get? appeal-jury appeal-id) ERR_APPEAL_NOT_FOUND))
    )
    (asserts! (is-eq (get status appeal-data) "active") ERR_CASE_CLOSED)
    (asserts! (<= stacks-block-height (get voting-ends appeal-data)) ERR_CASE_CLOSED)
    (asserts! (is-some (index-of jury-members caller)) ERR_NOT_JURY_MEMBER)
    (asserts! (is-none (map-get? appeal-votes {appeal-id: appeal-id, juror: caller})) ERR_ALREADY_VOTED)
    (asserts! (or (is-eq vote "original") (is-eq vote "reverse")) ERR_INVALID_VOTE)
    
    (map-set appeal-votes {appeal-id: appeal-id, juror: caller}
      {
        vote: vote,
        voted-at: stacks-block-height
      }
    )
    
    (let
      (
        (updated-appeal (if (is-eq vote "original")
          (merge appeal-data {
            votes-for-original: (+ (get votes-for-original appeal-data) u1),
            total-votes: (+ (get total-votes appeal-data) u1)
          })
          (merge appeal-data {
            votes-for-reverse: (+ (get votes-for-reverse appeal-data) u1),
            total-votes: (+ (get total-votes appeal-data) u1)
          })
        ))
      )
      (map-set appeals appeal-id updated-appeal)
      (try! (update-juror-appeal-stats caller))
      (ok true)
    )
  )
)

(define-private (update-juror-appeal-stats (juror principal))
  (let
    (
      (juror-data (unwrap! (map-get? jurors juror) ERR_NOT_REGISTERED))
    )
    (map-set jurors juror
      (merge juror-data {
        cases-served: (+ (get cases-served juror-data) u1),
        reputation: (+ (get reputation juror-data) u15)
      })
    )
    (ok true)
  )
)

(define-public (finalize-appeal (appeal-id uint))
  (let
    (
      (appeal-data (unwrap! (map-get? appeals appeal-id) ERR_APPEAL_NOT_FOUND))
      (original-case-id (get original-case-id appeal-data))
      (case-data (unwrap! (map-get? cases original-case-id) ERR_CASE_NOT_FOUND))
    )
    (asserts! (is-eq (get status appeal-data) "active") ERR_CASE_CLOSED)
    (asserts! (> stacks-block-height (get voting-ends appeal-data)) ERR_APPEAL_STILL_ACTIVE)
    
    (let
      (
        (appeal-decision (if (> (get votes-for-reverse appeal-data) (get votes-for-original appeal-data))
          "reversed"
          "upheld"
        ))
        (new-winner (if (> (get votes-for-reverse appeal-data) (get votes-for-original appeal-data))
          (if (is-eq (get plaintiff case-data) (get appellant appeal-data))
            (get defendant case-data)
            (get plaintiff case-data)
          )
          (if (is-eq (get status case-data) "plaintiff-wins")
            (get plaintiff case-data)
            (get defendant case-data)
          )
        ))
        (final-status (if (> (get votes-for-reverse appeal-data) (get votes-for-original appeal-data))
          (if (is-eq (get status case-data) "plaintiff-wins") "defendant-wins" "plaintiff-wins")
          (get status case-data)
        ))
      )
      (map-set appeals appeal-id (merge appeal-data {
        status: "finalized",
        final-decision: appeal-decision
      }))
      (map-set cases original-case-id (merge case-data {status: final-status}))
      (try! (distribute-appeal-rewards appeal-id new-winner appeal-decision))
      (ok new-winner)
    )
  )
)

(define-private (distribute-appeal-rewards (appeal-id uint) (winner principal) (decision (string-ascii 20)))
  (let
    (
      (appeal-data (unwrap! (map-get? appeals appeal-id) ERR_APPEAL_NOT_FOUND))
      (total-stake (get appeal-stake appeal-data))
      (winner-reward (/ (* total-stake u6) u10))
      (jury-reward-pool (/ (* total-stake u3) u10))
      (platform-fee (/ total-stake u10))
    )
    (try! (as-contract (stx-transfer? winner-reward tx-sender winner)))
    (try! (distribute-appeal-jury-rewards appeal-id (/ jury-reward-pool APPEAL_JURY_SIZE)))
    (ok true)
  )
)

(define-private (distribute-appeal-jury-rewards (appeal-id uint) (reward-per-juror uint))
  (let
    (
      (jury-members (unwrap! (map-get? appeal-jury appeal-id) ERR_APPEAL_NOT_FOUND))
    )
    (fold distribute-single-appeal-reward jury-members (ok reward-per-juror))
  )
)

(define-private (distribute-single-appeal-reward (juror principal) (reward-result (response uint uint)))
  (match reward-result
    reward-amount
      (match (as-contract (stx-transfer? reward-amount tx-sender juror))
        success (ok reward-amount)
        error-val (err error-val)
      )
    error-val (err error-val)
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

(define-read-only (get-appeal (appeal-id uint))
  (map-get? appeals appeal-id)
)

(define-read-only (get-appeal-jury (appeal-id uint))
  (map-get? appeal-jury appeal-id)
)

(define-read-only (get-appeal-vote (appeal-id uint) (juror principal))
  (map-get? appeal-votes {appeal-id: appeal-id, juror: juror})
)

(define-read-only (get-case-appeal (case-id uint))
  (map-get? case-appeals case-id)
)

(define-read-only (get-next-appeal-id)
  (var-get next-appeal-id)
)

(define-read-only (is-appeal-active (appeal-id uint))
  (match (map-get? appeals appeal-id)
    appeal-data (and 
      (is-eq (get status appeal-data) "active")
      (<= stacks-block-height (get voting-ends appeal-data))
    )
    false
  )
)

(define-read-only (can-appeal-case (case-id uint))
  (match (map-get? cases case-id)
    case-data (and
      (not (is-eq (get status case-data) "active"))
      (<= stacks-block-height (+ (get voting-ends case-data) APPEAL_PERIOD))
      (is-none (map-get? case-appeals case-id))
    )
    false
  )
)