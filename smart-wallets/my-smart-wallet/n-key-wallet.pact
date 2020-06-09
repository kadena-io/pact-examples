(define-keyset 'admin-keyset (read-keyset 'admin-keyset))

(module n-key-wallet 'admin-keyset
  @doc"'n-key-wallet' provides safe transactions of Kadena coins.                \
  \ The accounts in the wallet are guarded by a list of keys and a guardian key. \
  \ Each transfer will require n number of keys to be signed depending on the    \
  \ ratio of (amount to transfer) : (total balance). For example, if an account  \
  \ has a balance of 10.0 coins and guard list with 2 keys, then the transfer    \
  \ of 0-5 coins will require 1 key to be signed, and the transfer of 5-10 coins \
  \ will require 2 keys to be signed. Guardian key will allow the rotation of    \
  \ guard list in case some keys are lost."

  (defschema user
    user:string
    guards:list
    guardian:guard)

  (deftable user-table:{user})

  (defun smart-guard:bool ()
    @doc "Module guard"
    (create-module-guard 'wallet-guard)
    )

  (defun create-smart-user (user:string guards:[guard] guardian:guard)
    @doc "Create a coin account managed by customized smart wallet"
    (enforce-guard guardian)
    (insert user-table user {
      "user": user,
      "guards": guards,
      "guardian": guardian
      })
    (coin.create-account user (smart-guard))
  )

  (defun smart-transfer (account receiver amount)
    @doc "Runs coin.transfer with customized rules."
    (enforce-signed account amount)
    (coin.transfer account receiver amount))

  (defun rotate-guards (user:string guards:[guard])
    @doc "Rotate guards of the user"
    (enforce-guardian user)
    (update user-table user {
      "guards": guards
      }))

  (defun rotate-guardian-guard (user:string guardian:guard)
    @doc "Rotate guards of the user"
    (enforce-guardian user)
    (update user-table user {
      "guardian": guardian
      }))

 ;;Utility functions
 (defun enforce-guardian:bool (user:string)
  @doc "Enforced the guardian guard"
   (enforce-guard (guardian user))
 )

 (defun enforce-signed:bool (user amount)
   @doc "Enforced customized rule to find what guards need to be signed."
   (let* (
     (needed (numKeysNeeded user amount))
     (signed (numKeysSigned user)))
     (enforce (<= needed signed)
       (format "{} more keys need to be signed"
       [(- needed signed)]))))

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

  (defun guardian (user:string)
    (at 'guardian (read user-table user)))

  (defun total-balance (user:string)
    (at 'balance (coin.details user)))
)

(create-table user-table)
