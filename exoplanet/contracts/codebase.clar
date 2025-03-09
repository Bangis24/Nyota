;; Interplanetary Exoplanet Research Archive
;; Version: 3.0
;; Full implementation with data verification system and comprehensive error handling

;; Constants and Error Codes
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INVALID-PARAMS (err u400))
(define-constant ERR-PLANET-VERIFIED (err u403))
(define-constant ERR-LIST-FULL (err u429))

;; Data Validation Constants
(define-constant MAX-DISTANCE-FACTOR u50000)  ;; 50,000 light years in decimals
(define-constant MAX-RADIATION-RANGE 8000)    ;; Radiation in kilojansens (int)
(define-constant MAX-LIST-SIZE u1000)

;; Data Structures
(define-map exoplanets
    { planet-id: uint }
    {
        astronomer: principal,
        details: {
            name: (string-utf8 100),
            stellar-category: (string-utf8 500),
            composition-type: (string-utf8 50)
        },
        environment: {
            orbital-min: uint,
            orbital-max: uint,
            radiation: int,
            atmosphere-density: int
        },
        metadata: {
            spectral-signature: (string-ascii 100),
            discovered-at: uint
        },
        settings: {
            is-public: bool,
            planet-verified: bool
        }
    }
)

;; Astronomer planet tracking
(define-map planets-by-astronomer
    { astronomer: principal }
    { planet-ids: (list 1000 uint) }
)

;; State Variables
(define-data-var planet-counter uint u0)

;; Private Helper Functions

;; Validates environmental parameters
(define-private (validate-environment-params (radiation int) (atmosphere-density int))
    (and 
        (and (>= radiation (- MAX-RADIATION-RANGE)) (<= radiation MAX-RADIATION-RANGE))
        (and (>= atmosphere-density 0) (<= atmosphere-density 100000))
    )
)

;; Validates orbital range
(define-private (validate-orbital-range (min uint) (max uint))
    (and 
        (>= min u0)
        (>= max min)
        (<= max MAX-DISTANCE-FACTOR)
    )
)

;; Updates astronomer's planet list safely
(define-private (update-astronomer-planet-list (astronomer principal) (planet-id uint) (is-add bool))
    (let (
        (current-data (default-to { planet-ids: (list) } 
                      (map-get? planets-by-astronomer { astronomer: astronomer })))
        (current-ids (get planet-ids current-data))
    )
        (if is-add
            ;; Adding planet
            (if (>= (len current-ids) u1000)
                ERR-LIST-FULL
                (ok (map-set planets-by-astronomer
                    { astronomer: astronomer }
                    { planet-ids: (unwrap! (as-max-len? 
                        (append current-ids planet-id) u1000)
                        ERR-LIST-FULL) }
                )))
            ;; Removing planet
            (ok (map-set planets-by-astronomer
                { astronomer: astronomer }
                { planet-ids: (filter remove-planet-id current-ids) }
            ))
        )
    )
)

;; Helper for filtering planet IDs
(define-private (remove-planet-id (id uint)) 
    (not (is-eq id id))
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
        (composition-type (string-utf8 50))
        (orbital-min uint)
        (orbital-max uint)
        (radiation int)
        (atmosphere-density int)
        (spectral-signature (string-ascii 100))
        (is-public bool))
    (let (
        (planet-id (+ (var-get planet-counter) u1))
        (current-time (unwrap-panic (get-block-info? time u0)))
    )
        ;; Input validation
        (asserts! (validate-orbital-range orbital-min orbital-max) ERR-INVALID-PARAMS)
        (asserts! (validate-environment-params radiation atmosphere-density) ERR-INVALID-PARAMS)
        
        ;; Create exoplanet record
        (map-set exoplanets
            { planet-id: planet-id }
            {
                astronomer: tx-sender,
                details: {
                    name: name,
                    stellar-category: stellar-category,
                    composition-type: composition-type
                },
                environment: {
                    orbital-min: orbital-min,
                    orbital-max: orbital-max,
                    radiation: radiation,
                    atmosphere-density: atmosphere-density
                },
                metadata: {
                    spectral-signature: spectral-signature,
                    discovered-at: current-time
                },
                settings: {
                    is-public: is-public,
                    planet-verified: false
                }
            }
        )
        
        ;; Update astronomer's planet list
        (try! (update-astronomer-planet-list tx-sender planet-id true))
        
        ;; Update counter and return
        (var-set planet-counter planet-id)
        (ok planet-id)
    )
)

;; Updates exoplanet details
(define-public (update-exoplanet-details
        (planet-id uint)
        (name (string-utf8 100))
        (stellar-category (string-utf8 500))
        (composition-type (string-utf8 50))
        (is-public bool))
    (let ((planet (unwrap! (map-get? exoplanets { planet-id: planet-id }) ERR-NOT-FOUND)))
        (asserts! (is-planet-owner planet-id) ERR-NOT-AUTHORIZED)
        (asserts! (not (get planet-verified (get settings planet))) ERR-PLANET-VERIFIED)
        
        (map-set exoplanets
            { planet-id: planet-id }
            (merge planet {
                details: {
                    name: name,
                    stellar-category: stellar-category,
                    composition-type: composition-type
                },
                settings: (merge (get settings planet) {
                    is-public: is-public
                })
            })
        )
        (ok true)
    )
)

;; Verifies exoplanet data (making it immutable)
(define-public (verify-exoplanet-data (planet-id uint))
    (let ((planet (unwrap! (map-get? exoplanets { planet-id: planet-id }) ERR-NOT-FOUND)))
        (asserts! (is-planet-owner planet-id) ERR-NOT-AUTHORIZED)
        
        (ok (map-set exoplanets
            { planet-id: planet-id }
            (merge planet {
                settings: (merge (get settings planet) {
                    planet-verified: true
                })
            })
        ))
    )
)

;; Transfers exoplanet research rights
(define-public (transfer-exoplanet (planet-id uint) (new-astronomer principal))
    (let ((planet (unwrap! (map-get? exoplanets { planet-id: planet-id }) ERR-NOT-FOUND)))
        ;; Verify ownership
        (asserts! (is-planet-owner planet-id) ERR-NOT-AUTHORIZED)
        
        ;; Remove from current astronomer's list
        (try! (update-astronomer-planet-list tx-sender planet-id false))
        
        ;; Add to new astronomer's list
        (try! (update-astronomer-planet-list new-astronomer planet-id true))
        
        ;; Update planet ownership
        (map-set exoplanets
            { planet-id: planet-id }
            (merge planet { astronomer: new-astronomer })
        )
        (ok true)
    )
)

;; Read-Only Functions

;; Gets exoplanet information
(define-read-only (get-exoplanet (planet-id uint))
    (map-get? exoplanets { planet-id: planet-id })
)

;; Gets all exoplanets cataloged by an astronomer
(define-read-only (get-exoplanets-by-astronomer (astronomer principal))
    (default-to { planet-ids: (list) }
        (map-get? planets-by-astronomer { astronomer: astronomer }))
)

;; Gets total number of registered exoplanets
(define-read-only (get-exoplanet-count)
    (ok (var-get planet-counter))
)

;; Checks if an exoplanet's data is publicly accessible
(define-read-only (is-exoplanet-public (planet-id uint))
    (match (map-get? exoplanets { planet-id: planet-id })
        data (ok (get is-public (get settings data)))
        (err ERR-NOT-FOUND)
    )
)