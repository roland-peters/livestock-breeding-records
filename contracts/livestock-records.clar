;; Livestock Breeding Records Smart Contract
;; This contract manages animal husbandry data including genealogy, breeding schedules, and health records

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_DATA (err u400))

;; Data Variables
(define-data-var animal-id-counter uint u1)
(define-data-var breeding-id-counter uint u1)
(define-data-var health-record-counter uint u1)

;; Data Maps
(define-map animals
  { animal-id: uint }
  {
    owner: principal,
    species: (string-ascii 32),
    breed: (string-ascii 64),
    gender: (string-ascii 8),
    birth-date: uint,
    parent-sire: (optional uint),
    parent-dam: (optional uint),
    registration-number: (string-ascii 32),
    active: bool
  }
)

(define-map breeding-records
  { breeding-id: uint }
  {
    sire-id: uint,
    dam-id: uint,
    breeding-date: uint,
    expected-delivery: uint,
    actual-delivery: (optional uint),
    offspring-count: uint,
    breeding-method: (string-ascii 32),
    success: bool,
    notes: (string-ascii 256)
  }
)

(define-map health-records
  { health-record-id: uint }
  {
    animal-id: uint,
    record-date: uint,
    record-type: (string-ascii 32),
    veterinarian: (string-ascii 64),
    diagnosis: (string-ascii 256),
    treatment: (string-ascii 256),
    medication: (string-ascii 128),
    follow-up-date: (optional uint),
    cost: uint
  }
)

(define-map owner-animals
  { owner: principal, animal-id: uint }
  { registered: bool }
)

;; Read-Only Functions
(define-read-only (get-animal (animal-id uint))
  (map-get? animals { animal-id: animal-id })
)

(define-read-only (get-breeding-record (breeding-id uint))
  (map-get? breeding-records { breeding-id: breeding-id })
)

(define-read-only (get-health-record (health-record-id uint))
  (map-get? health-records { health-record-id: health-record-id })
)

(define-read-only (get-animal-lineage (animal-id uint))
  (match (get-animal animal-id)
    animal (ok {
      animal-id: animal-id,
      sire: (get parent-sire animal),
      dam: (get parent-dam animal),
      breed: (get breed animal)
    })
    ERR_NOT_FOUND
  )
)

(define-read-only (is-owner (animal-id uint) (owner principal))
  (match (get-animal animal-id)
    animal (is-eq (get owner animal) owner)
    false
  )
)

(define-read-only (get-current-counters)
  {
    next-animal-id: (var-get animal-id-counter),
    next-breeding-id: (var-get breeding-id-counter),
    next-health-record-id: (var-get health-record-counter)
  }
)

;; Public Functions
(define-public (register-animal
  (species (string-ascii 32))
  (breed (string-ascii 64))
  (gender (string-ascii 8))
  (birth-date uint)
  (parent-sire (optional uint))
  (parent-dam (optional uint))
  (registration-number (string-ascii 32))
)
  (let ((animal-id (var-get animal-id-counter)))
    (asserts! (> (len species) u0) ERR_INVALID_DATA)
    (asserts! (> (len breed) u0) ERR_INVALID_DATA)
    (asserts! (or (is-eq gender "male") (is-eq gender "female")) ERR_INVALID_DATA)
    (asserts! (> birth-date u0) ERR_INVALID_DATA)
    
    ;; Validate parent animals exist if provided
    (match parent-sire
      sire-id (asserts! (is-some (get-animal sire-id)) ERR_NOT_FOUND)
      true
    )
    (match parent-dam
      dam-id (asserts! (is-some (get-animal dam-id)) ERR_NOT_FOUND)
      true
    )
    
    (map-set animals
      { animal-id: animal-id }
      {
        owner: tx-sender,
        species: species,
        breed: breed,
        gender: gender,
        birth-date: birth-date,
        parent-sire: parent-sire,
        parent-dam: parent-dam,
        registration-number: registration-number,
        active: true
      }
    )
    
    (map-set owner-animals
      { owner: tx-sender, animal-id: animal-id }
      { registered: true }
    )
    
    (var-set animal-id-counter (+ animal-id u1))
    (ok animal-id)
  )
)

(define-public (record-breeding
  (sire-id uint)
  (dam-id uint)
  (breeding-date uint)
  (expected-delivery uint)
  (breeding-method (string-ascii 32))
  (notes (string-ascii 256))
)
  (let ((breeding-id (var-get breeding-id-counter)))
    (asserts! (is-owner sire-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-owner dam-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq sire-id dam-id)) ERR_INVALID_DATA)
    (asserts! (> breeding-date u0) ERR_INVALID_DATA)
    (asserts! (> expected-delivery breeding-date) ERR_INVALID_DATA)
    
    ;; Validate animals exist
    (asserts! (is-some (get-animal sire-id)) ERR_NOT_FOUND)
    (asserts! (is-some (get-animal dam-id)) ERR_NOT_FOUND)
    
    (map-set breeding-records
      { breeding-id: breeding-id }
      {
        sire-id: sire-id,
        dam-id: dam-id,
        breeding-date: breeding-date,
        expected-delivery: expected-delivery,
        actual-delivery: none,
        offspring-count: u0,
        breeding-method: breeding-method,
        success: false,
        notes: notes
      }
    )
    
    (var-set breeding-id-counter (+ breeding-id u1))
    (ok breeding-id)
  )
)

(define-public (update-breeding-outcome
  (breeding-id uint)
  (actual-delivery uint)
  (offspring-count uint)
  (success bool)
)
  (let ((breeding (unwrap! (get-breeding-record breeding-id) ERR_NOT_FOUND)))
    (asserts! (is-owner (get sire-id breeding) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> actual-delivery u0) ERR_INVALID_DATA)
    
    (map-set breeding-records
      { breeding-id: breeding-id }
      (merge breeding {
        actual-delivery: (some actual-delivery),
        offspring-count: offspring-count,
        success: success
      })
    )
    (ok true)
  )
)

(define-public (add-health-record
  (animal-id uint)
  (record-date uint)
  (record-type (string-ascii 32))
  (veterinarian (string-ascii 64))
  (diagnosis (string-ascii 256))
  (treatment (string-ascii 256))
  (medication (string-ascii 128))
  (follow-up-date (optional uint))
  (cost uint)
)
  (let ((health-record-id (var-get health-record-counter)))
    (asserts! (is-owner animal-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> record-date u0) ERR_INVALID_DATA)
    (asserts! (> (len record-type) u0) ERR_INVALID_DATA)
    
    (map-set health-records
      { health-record-id: health-record-id }
      {
        animal-id: animal-id,
        record-date: record-date,
        record-type: record-type,
        veterinarian: veterinarian,
        diagnosis: diagnosis,
        treatment: treatment,
        medication: medication,
        follow-up-date: follow-up-date,
        cost: cost
      }
    )
    
    (var-set health-record-counter (+ health-record-id u1))
    (ok health-record-id)
  )
)

(define-public (deactivate-animal (animal-id uint))
  (let ((animal (unwrap! (get-animal animal-id) ERR_NOT_FOUND)))
    (asserts! (is-owner animal-id tx-sender) ERR_UNAUTHORIZED)
    
    (map-set animals
      { animal-id: animal-id }
      (merge animal { active: false })
    )
    (ok true)
  )
)

(define-public (transfer-animal (animal-id uint) (new-owner principal))
  (let ((animal (unwrap! (get-animal animal-id) ERR_NOT_FOUND)))
    (asserts! (is-owner animal-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq tx-sender new-owner)) ERR_INVALID_DATA)
    
    ;; Remove old owner mapping
    (map-delete owner-animals { owner: tx-sender, animal-id: animal-id })
    
    ;; Update animal owner
    (map-set animals
      { animal-id: animal-id }
      (merge animal { owner: new-owner })
    )
    
    ;; Add new owner mapping
    (map-set owner-animals
      { owner: new-owner, animal-id: animal-id }
      { registered: true }
    )
    
    (ok true)
  )
)


;; title: livestock-records
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

