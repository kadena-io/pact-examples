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
          "owner": (read-keyset "admin-keyset")
        }
      )
      i
    )
  )

  (defun showCritter:string (str:string c:object)
    "String representation of a critter"
    (bind c {"matronId" := m,
             "sireId" := s,
             "generation" := gen,
             "genes" := genes,
             "owner" := o}
      (+ (format "gen: {} matron: {} sire: {} owner: {} {}\n" [gen m s o genes]) str)
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
    "Transfer critter ownership to another party"
    (let ((c (read critters critterId)))
      (bind c {"matronId" := m,
               "sireId" := s,
               "generation" := gen,
               "genes" := genes,
               "owner" := o}
        (update critters critterId
          {"matronId": m,
           "sireId": s,
           "generation": gen,
           "genes": genes,
           "owner": newOwner
          })
      )
    )
  )
)

(create-table critters)
(create-table countTable)
(insert countTable "critters" {"count":0})
