(define-keyset 'admin-keyset (read-keyset 'admin-keyset))
; (namespace 'user)
(module n-key-wallet 'admin-keyset

  ;;Create a smart wallet interface.
  (defschema user
    id:string
    limitLogic:string
    guards:list)

  (deftable user-table:{user})

  (defun smart-guard:guard ()
    (create-module-guard 'wallet-guard))

  (defun create-smart-user (id:string guards:[guard])
    @doc "Create a coin account managed by customized smart wallet"
    (insert user-table id {
      "id": id,
      "limitLogic": "default",
      "guards": guards
      })
    (coin.create-account id (smart-guard))
  )

  (defun smart-transfer (account receiver amount)
    @doc "Runs coin.transfer with customized rules."
    (enforce-signed account amount)
    (coin.transfer account receiver amount))

  (defun enforce-signed:bool (id amount)
    @doc "Enforced customized rule to transfer."
    (let* (
      (needed (numKeysNeeded id amount))
      (signed (numKeysSigned id)))
      (enforce (<= needed signed)
        (format "{} more keys need to be signed"
        [(- needed signed)]))))

 ;;Utility functions
 (defun numKeysNeeded:integer (user:string amount:decimal)
    @doc "Number of keys required to transfer amount"
    (let ((
      balance (total-balance user)
      ))
    (enforce (<= amount balance) "Insufficient balance")
    (logic (numKeys user) amount balance)))

  (defun numKeysSigned:integer (id:string)
    @doc "Number of keys signed"
    (length
      (filter
        (= true)
          (map (try-enforce-guard) (guards id)))))

  (defun try-enforce-guard:bool (guard:guard)
    @doc "Try enforcing a guard for transfer and returns result"
    (try false (enforce-guard guard)))

  (defun logic:integer (numKeys:integer amount:decimal total:decimal)
    @doc "Default logic to calculate number of keys needed to transfer amount"
    (ceiling (* numKeys (/ amount total))))

  (defun numKeys (user:string)
    (length (guards user)))

  (defun guards (user:string)
    (at 'guards (read user-table user)))

  (defun total-balance (id:string)
    (at 'balance (coin.details id)))
)

(create-table user-table)
