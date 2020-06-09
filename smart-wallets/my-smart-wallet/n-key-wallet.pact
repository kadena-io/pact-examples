(define-keyset 'admin-keyset (read-keyset 'admin-keyset))
; (namespace 'user)
(module n-key-wallet 'admin-keyset

  ;;Create a smart wallet interface .
  (defschema user
    user:string
    limitLogic:string
    guards:list
    transfer-guard:guard)

  (deftable user-table:{user})

  (defun enforce-smart-guard:bool (tguard:guard)
    @doc "Enforce module guard and transfer guard"
    (enforce-guard (create-module-guard 'wallet-guard))
    (enforce-guard tguard)
    )

  (defun smart-guard:guard (tguard:guard)
    @doc "Create a smart guard that guards against direct coin.transfer of the account"
    (create-user-guard (enforce-smart-guard tguard))
    )

  (defun create-smart-user (user:string guards:[guard] tguard:guard)
    @doc "Create a coin account managed by customized smart wallet"
    (insert user-table user {
      "user": user,
      "limitLogic": "default",
      "guards": guards,
      "transfer-guard":tguard
      })
    (coin.create-account user (smart-guard tguard))
  )

  (defun smart-transfer (account receiver amount)
    @doc "Runs coin.transfer with customized rules."
    (enforce-signed account amount)
    (coin.transfer account receiver amount))

  (defun enforce-signed:bool (user amount)
    @doc "Enforced customized rule to transfer."
    (let* (
      (needed (numKeysNeeded user amount))
      (signed (numKeysSigned user)))
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

  (defun numKeysSigned:integer (user:string)
    @doc "Number of keys signed"
    (length
      (filter
        (= true)
          (map (try-enforce-guard) (guards user)))))

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

  (defun total-balance (user:string)
    (at 'balance (coin.details user)))
)

(create-table user-table)
