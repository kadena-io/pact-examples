(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

(module critters 'admin-keyset
  "Collectible Crypto Critters"
  (defschema critter
    "Data defining a critter"
    genes:integer
    matron-id:integer
    sire-id:integer
    generation:integer
    owner:keyset
    transfer:bool
    transfer-to:keyset
  )

  (defschema countSchema
    count:integer
  )

  (deftable critters:{critter})
  (deftable countTable:{countSchema})

  (defun get-inc-count:string (k:string)
    "Incremenent row K in the count table"
    (with-read countTable k {"count" := count}
     (write countTable k {"count": (+ count 1)})
     (format "{}" [count])
    )
  )

  (defun create-critter:integer (genes:integer)
    "Create a gen0 critter using GENES"
    (enforce-keyset 'admin-keyset)
    (let ((id (get-inc-count "critters")))
      (insert critters id
        { "matron-id": 0,
          "sire-id": 0,
          "generation": 0,
          "genes": genes,
          "owner": (read-keyset "admin-keyset"),
          "transfer": false,
          "transfer-to": (read-keyset "admin-keyset")
        }
      )
      id
    )
  )

  (defun show-critter:string (suffix:string critter:object)
    "String representation of CRITTER appending SUFFIX"
    (bind critter { "matron-id" := m,
              "sire-id" := s,
              "generation" := gen,
              "genes" := genes,
              "owner" := o,
              "transfer" := t,
              "transfer-to" := tto
            }
      (+ (format "gen: {} matron: {} sire: {} owner: {} {} {} {}\n"
          [gen m s o t tto genes]) suffix)
    )
  )

  (defun show-generation:string (gen:integer)
    "Get all the critters in GEN generation"
    (let ((cs (select critters (where 'generation (= gen)))))
         (fold (show-critter) "" cs)
    )
  )

  (defun owner (critter-id:string)
    "Get the owner of a critter CRITTER-ID"
    (with-read critters critter-id {"owner":= o} o)
  )

  (defun transfer-critter (new-owner:keyset critter-id:string)
    "Transfer critter CRITTER-ID ownership to NEW-OWNER (Note: UNSAFE!)"
    (let ((c (read critters critter-id)))
        (enforce-keyset (at "owner" c))
        (update critters critter-id
          (+ {"owner": new-owner} c)
        )
    )
  )

  (defun set-transfer (critter-id:string transfer:bool to:keyset)
    "Set critter CRITTER-ID TRANSFER flag to TO keyset"
    ;; NOTE: This is a private helper function
    (let ((c (read critters critter-id)))
        (enforce-keyset (at "owner" c))
        (update critters critter-id
          (+ {"transfer": transfer, "transfer-to": to} c)
        )
    )
  )

  (defun initiate-transfer (new-owner:keyset critter-id:string)
    "Transfer critter CRITTER-ID ownership to NEW-OWNER safely without \
    \the possibility of the critter getting lost"
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (set-transfer critter-id true new-owner)
    )
  )

  (defun complete-transfer (critter-id:string)
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "transfer-to" c))
      ;; We don't call transferCritter because we're not the owner
      (update critters critter-id
        (+ {"owner": (at "transfer-to" c)} c)
      )
      (set-transfer critter-id false (read-keyset "admin-keyset"))
    )
  )

  (defun cancel-transfer (critter-id:string)
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (set-transfer critter-id false (read-keyset "admin-keyset"))
    )
  )
)

(create-table critters)
(create-table countTable)
(insert countTable "critters" {"count":0})
