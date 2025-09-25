;; FitnessChain: Fitness Activity Tracking and Reward System
;; Version: 1.0.0

;; Constants
(define-constant FITNESS_INCENTIVE_CAPACITY u2800000)
(define-constant BASE_FITNESS_REWARD u20)
(define-constant ATHLETE_BONUS u7)
(define-constant MAX_ATHLETE_LEVEL u10)
(define-constant ERR_INVALID_WORKOUT_USAGE u1)
(define-constant ERR_NO_FITNESS_POINTS u2)
(define-constant ERR_INCENTIVE_EXCEEDED u3)
(define-constant BLOCKS_PER_FITNESS_CYCLE u1440)
(define-constant TRAINING_OPTIMIZATION_MULTIPLIER u3)
(define-constant MIN_OPTIMIZATION_PERIOD u720)
(define-constant EARLY_OPTIMIZATION_PENALTY u12)

;; Data Variables
(define-data-var total-fitness-points-awarded uint u0)
(define-data-var total-workout-sessions uint u0)
(define-data-var fitness-coordinator principal tx-sender)

;; Data Maps
(define-map athlete-workouts principal uint)
(define-map athlete-fitness-points principal uint)
(define-map workout-start-time principal uint)
(define-map athlete-level principal uint)
(define-map athlete-last-workout principal uint)
(define-map athlete-optimized-training principal uint)
(define-map athlete-optimization-start-block principal uint)

;; Public Functions
(define-public (start-workout-session (workout-intensity uint))
  (let
    (
      (athlete tx-sender)
    )
    (asserts! (> workout-intensity u0) (err ERR_INVALID_WORKOUT_USAGE))
    (map-set workout-start-time athlete burn-block-height)
    (ok true)
  ))

(define-public (complete-workout-session (workout-intensity uint))
  (let
    (
      (athlete tx-sender)
      (start-block (default-to u0 (map-get? workout-start-time athlete)))
      (blocks-exercising (- burn-block-height start-block))
      (last-workout-block (default-to u0 (map-get? athlete-last-workout athlete)))
      (athlete-tier (default-to u0 (map-get? athlete-level athlete)))
      (capped-tier (if (<= athlete-tier MAX_ATHLETE_LEVEL) athlete-tier MAX_ATHLETE_LEVEL))
      (fitness-reward (+ BASE_FITNESS_REWARD (* capped-tier ATHLETE_BONUS)))
    )
    (asserts! (and (> start-block u0) (>= blocks-exercising workout-intensity)) (err ERR_INVALID_WORKOUT_USAGE))
    
    (map-set athlete-workouts athlete (+ (default-to u0 (map-get? athlete-workouts athlete)) u1))
    (map-set athlete-fitness-points athlete (+ (default-to u0 (map-get? athlete-fitness-points athlete)) fitness-reward))
    
    (if (< (- burn-block-height last-workout-block) BLOCKS_PER_FITNESS_CYCLE)
      (map-set athlete-level athlete (+ athlete-tier u1))
      (map-set athlete-level athlete u1)
    )
    
    (map-set athlete-last-workout athlete burn-block-height)
    (var-set total-workout-sessions (+ (var-get total-workout-sessions) u1))
    (var-set total-fitness-points-awarded (+ (var-get total-fitness-points-awarded) fitness-reward))
    
    (asserts! (<= (var-get total-fitness-points-awarded) FITNESS_INCENTIVE_CAPACITY) (err ERR_INCENTIVE_EXCEEDED))
    (ok fitness-reward)
  ))

(define-public (claim-fitness-rewards)
  (let
    (
      (athlete tx-sender)
      (point-balance (default-to u0 (map-get? athlete-fitness-points athlete)))
    )
    (asserts! (> point-balance u0) (err ERR_NO_FITNESS_POINTS))
    (map-set athlete-fitness-points athlete u0)
    (ok point-balance)
  ))

;; Training Optimization Features
(define-public (optimize-training-plan (amount uint))
  (let
    (
      (athlete tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_WORKOUT_USAGE))
    (asserts! (>= (var-get total-fitness-points-awarded) amount) (err ERR_INCENTIVE_EXCEEDED))
    
    (map-set athlete-optimized-training athlete amount)
    (map-set athlete-optimization-start-block athlete burn-block-height)
    (var-set total-fitness-points-awarded (- (var-get total-fitness-points-awarded) amount))
    (ok amount)
  ))

(define-public (complete-training-optimization)
  (let
    (
      (athlete tx-sender)
      (optimized-amount (default-to u0 (map-get? athlete-optimized-training athlete)))
      (optimization-start-block (default-to u0 (map-get? athlete-optimization-start-block athlete)))
      (blocks-optimized (- burn-block-height optimization-start-block))
      (penalty (if (< blocks-optimized MIN_OPTIMIZATION_PERIOD) (/ (* optimized-amount EARLY_OPTIMIZATION_PENALTY) u100) u0))
      (final-amount (- optimized-amount penalty))
    )
    (asserts! (> optimized-amount u0) (err ERR_NO_FITNESS_POINTS))
    
    (map-set athlete-optimized-training athlete u0)
    (map-set athlete-optimization-start-block athlete u0)
    (var-set total-fitness-points-awarded (+ (var-get total-fitness-points-awarded) final-amount))
    (ok final-amount)
  ))

;; Read-Only Functions
(define-read-only (get-workout-count (user principal))
  (default-to u0 (map-get? athlete-workouts user)))

(define-read-only (get-fitness-point-balance (user principal))
  (default-to u0 (map-get? athlete-fitness-points user)))

(define-read-only (get-athlete-level (user principal))
  (default-to u0 (map-get? athlete-level user)))

(define-read-only (get-fitness-program-stats)
  {
    total-workout-sessions: (var-get total-workout-sessions),
    total-fitness-points-awarded: (var-get total-fitness-points-awarded)
  })

;; Private Functions
(define-private (is-fitness-coordinator)
  (is-eq tx-sender (var-get fitness-coordinator)))