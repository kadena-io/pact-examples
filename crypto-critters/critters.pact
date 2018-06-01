(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

(module critters 'admin-keyset
  "Collectible Crypto Critters"
  (defschema critter
    "Data defining a critter"
    genes:integer
    matronId:integer
    sireId:integer
    generation:integer
    owner:keyset
    transfer:bool
    transferTo:keyset
  )

  (defschema countSchema
    count:integer
  )

  (deftable critters:{critter})
  (deftable countTable:{countSchema})

  (defun getAndIncCount:integer (k:string)
    "Incremenent a row in the count table"
    (with-read countTable k {"count" := count}
     (write countTable k {"count": (+ count 1)})
     count
    )
  )

  (defun createCritter:integer (genes:integer)
    "Create a gen0 critter"
    (enforce-keyset 'admin-keyset)
    (let ((i (getAndIncCount "critters")))
      (insert critters (format "{}" [i])
        { "matronId": 0,
          "sireId": 0,
          "generation": 0,
          "genes": genes,
          "owner": (read-keyset "admin-keyset"),
          "transfer": false,
          "transferTo": (read-keyset "admin-keyset")
        }
      )
      i
    )
  )

  (defun showCritter:string (str:string c:object)
    "String representation of a critter"
    (bind c { "matronId" := m,
              "sireId" := s,
              "generation" := gen,
              "genes" := genes,
              "owner" := o,
              "transfer" := t,
              "transferTo" := tto
            }
      (+ (format "gen: {} matron: {} sire: {} owner: {} {} {} {}\n" [gen m s o t tto genes]) str)
    )
  )

  (defun showGeneration:string (gen:integer)
    "Get all the critters in the specified generation"
    (let ((cs (select critters (where 'generation (= 0)))))
         (fold (showCritter) "" cs)
    )
  )

  (defun owner (critterId:string)
    "Get the owner of a critter"
    (let ((c (read critters critterId)))
      (bind c {"owner" := o} o)
    )
  )

  (defun transferCritter (newOwner:keyset critterId:string)
    "Transfer critter ownership to another party (Note: UNSAFE!)"
    (let ((c (read critters critterId)))
        (enforce-keyset (at "owner" c))
        (update critters critterId
          (+ {"owner": newOwner} c)
        )
    )
  )

  (defun setTransfer (critterId:string flag:bool to:keyset)
    "Set a critter's transfer flag"
    ;; NOTE: This is a private helper function
    (let ((c (read critters critterId)))
        (enforce-keyset (at "owner" c))
        (update critters critterId
          (+ {"transfer": flag, "transferTo": to} c)
        )
    )
  )

  (defun initiate-transfer (newOwner:keyset critterId:string)
    "Transfer critter ownership to another party safely without the possibility of the critter getting lost"
    (let ((c (read critters critterId)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (setTransfer critterId true newOwner)
    )
  )

  (defun complete-transfer (critterId:string)
    (let ((c (read critters critterId)))
      (enforce-keyset (at "transferTo" c))
      ;; We don't call transferCritter because we're not the owner
      (update critters critterId
        (+ {"owner": (at "transferTo" c)} c)
      )
      (setTransfer critterId false (read-keyset "admin-keyset"))
    )
  )

  (defun cancel-transfer (critterId:string)
    (let ((c (read critters critterId)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (setTransfer critterId false (read-keyset "admin-keyset"))
    )
  )
)

(create-table critters)
(create-table countTable)
(insert countTable "critters" {"count":0})
