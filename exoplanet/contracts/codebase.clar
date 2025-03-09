;; Interplanetary Exoplanet Research Archive
;; First version of project

;; Constants and Error Codes
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INVALID-PARAMS (err u400))

;; Data Validation Constants
(define-constant MAX-DISTANCE-FACTOR u50000)  ;; 50,000 light years in decimals

;; Data Structures
(define-map exoplanets
    { planet-id: uint }
    {
        astronomer: principal,
        details: {
            name: (string-utf8 100),
            stellar-category: (string-utf8 500)
        },
        environment: {
            orbital-min: uint,
            orbital-max: uint
        },
        metadata: {
            discovered-at: uint
        }
    }
)

;; State Variables
(define-data-var planet-counter uint u0)

;; Private Helper Functions

;; Validates orbital range
(define-private (validate-orbital-range (min uint) (max uint))
    (and 
        (>= min u0)
        (>= max min)
        (<= max MAX-DISTANCE-FACTOR)
    )
)

;; Verifies planet ownership
(define-private (is-planet-owner (planet-id uint))
    (match (map-get? exoplanets { planet-id: planet-id })
        data (is-eq tx-sender (get astronomer data))
        false
    )
)

;; Public Functions

;; Registers a new exoplanet discovery
(define-public (register-exoplanet 
        (name (string-utf8 100))
        (stellar-category (string-utf8 500))
        (orbital-min uint)
        (orbital-max uint))
    (let (
        (planet-id (+ (var-get planet-counter) u1))
        (current-time (unwrap-panic (get-block-info? time u0)))
    )
        ;; Input validation
        (asserts! (validate-orbital-range orbital-min orbital-max) ERR-INVALID-PARAMS)
        
        ;; Create exoplanet record
        (map-set exoplanets
            { planet-id: planet-id }
            {
                astronomer: tx-sender,
                details: {
                    name: name,
                    stellar-category: stellar-category
                },
                environment: {
                    orbital-min: orbital-min,
                    orbital-max: orbital-max
                },
                metadata: {
                    discovered-at: current-time
                }
            }
        )
        
        ;; Update counter and return
        (var-set planet-counter planet-id)
        (ok planet-id)
    )
)

;; Updates exoplanet details
(define-public (update-exoplanet-details
        (planet-id uint)
        (name (string-utf8 100))
        (stellar-category (string-utf8 500)))
    (let ((planet (unwrap! (map-get? exoplanets { planet-id: planet-id }) ERR-NOT-FOUND)))
        (asserts! (is-planet-owner planet-id) ERR-NOT-AUTHORIZED)
        
        (map-set exoplanets
            { planet-id: planet-id }
            (merge planet {
                details: {
                    name: name,
                    stellar-category: stellar-category
                }
            })
        )
        (ok true)
    )
)

;; Read-Only Functions

;; Gets exoplanet information
(define-read-only (get-exoplanet (planet-id uint))
    (map-get? exoplanets { planet-id: planet-id })
)

;; Gets total number of registered exoplanets
(define-read-only (get-exoplanet-count)
    (ok (var-get planet-counter))
)