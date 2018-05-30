(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

(module critters 'admin-keyset
  "Collectible Crypto Critters"
  (defschema critter
    "Data defining a critter"
    genes:integer
    matronId:integer
    sireId:integer
    generation:integer
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
          "genes": genes
        }
      )
      i
    )
  )

  (defun showCritter:string (str:string c:object)
    "String representation of a critter"
    (bind c {"matronId" := m, "sireId" := s, "generation" := gen, "genes" := genes}
      (+ (format "gen: {} matron: {} sire: {} {}\n" [gen m s genes]) str)
    )
  )

  (defun showGeneration:string (gen:integer)
    "Get all the critters in the specified generation"
    (let ((cs (select critters (where 'generation (= 0)))))
         (fold (showCritter) "" cs)
    )
  )

)

(create-table critters)
(create-table countTable)
(insert countTable "critters" {"count":0})
