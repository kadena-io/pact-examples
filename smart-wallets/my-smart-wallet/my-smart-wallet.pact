(define-keyset 'master-keyset (read-keyset 'master-keyset))
;(namespace 'user)
(module my-smart-wallet GOVERNANCE
  @doc"'smart-wallet' provides safe transactions of Kadena coins. \
  \The accounts are guarded by the module guard, there fore GOVERNANCE \
  \of the contract will be the master key of all accounts with complete control.\
  \Accounts will each have guards that can make transfers under certain rules. \
  \When using the master key, none of these restrictions apply. "

  (defcap GOVERNANCE()
    (enforce-keyset (keyset-ref-guard 'master-keyset)))

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema wallet
    @doc "schema of accounts and customized guards"
    guard:guard
    max-withdrawal:decimal
    min-balance:decimal
    time-between:decimal
    last-withdrawal-time:time
    )

  (deftable wallet-table:{wallet})

  ; --------------------------------------------------------------------------
  ; Constants

  (defconst DEFAULT_MAX_WITHDRAWAL:decimal 20.0)
  (defconst DEFAULT_MIN_BALANCE:decimal 20.0)
  (defconst DEFAULT_TIME_BETWEEN 1800.0)
  (defconst EPOCH (time "1970-01-01T00:00:00Z"))

  ; --------------------------------------------------------------------------
  ; Module Guards

  (defun wallet-module-guard:guard()
    @doc "module guard of the wallet."

    (create-module-guard 'wallet-guard))

  ;;Control with master key

  (defun rotate-guard:string (account:string new-guard:guard)
    @doc "rotates the account guard"

    (enforce-guard new-guard)
    (with-capability (GOVERNANCE)
      (update wallet-table account {
        "guard": new-guard
        })
    )
  )


  (defun update-min-balance:string (account:string amount:decimal)
    @doc "updates the minimum balance an account needs to hold"
    (with-capability (GOVERNANCE)
      (update wallet-table account {
        "min-balance": amount
      })
    )
  )

  (defun update-max-withdrawal:string (account:string amount:decimal)
    @doc "updates the maximum balance an account can transfer"
    (with-capability (GOVERNANCE)
      (update wallet-table account {
        "max-withdrawal": amount
      })
    )
  )


  (defun update-time-between:string (account:string time-between:time)
    @doc "updates the minimum time an account has to wait between two transfers."
    (with-capability (GOVERNANCE)
      (update wallet-table account {
        "time-between": time-between
      })
    )
  )

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap TRANSFER:bool (account:string amount:decimal)
    @doc "enforces the customized rules of the account"
    (with-read wallet-table account {
        "guard":= guard,
        "max-withdrawal":= max-amount,
        "min-balance":= min-bal,
        "time-between":= time-between,
        "last-withdrawal-time":= last-withdrawal-time
      }
      (enforce ( >= max-amount amount) "Transfer amount is too big")
      (enforce ( >= (diff-time (curr-time) last-withdrawal-time) time-between)
        (format "You're transferring too many times {} {}" [time-between (diff-time (curr-time) last-withdrawal-time)]))
      (let (( coin-balance:decimal (coin.get-balance account) ))
        (enforce ( <= min-bal (- coin-balance amount)) (format "min bal {} and this left {}" [min-bal (- coin-balance amount)]))
      )
      (enforce-guard guard)
      (update wallet-table account {
        "last-withdrawal-time": (curr-time)
        })
    ))

  (defun transfer (account receiver amount)
    @doc "Runs coin.transfer with customized rules."
    (with-capability (TRANSFER account amount)
      (coin.transfer account receiver amount)
    )
  )

  (defun transfer-create (account receiver receiver-guard amount)
    @doc "Runs coin.transfer-create with customized rules."
    (with-capability (TRANSFER account amount)
      (coin.transfer-create account receiver receiver-guard amount)
    )
  )

  (defun safe-transfer (sender:string receiver:string receiver-guard:guard amount:decimal extra:decimal)
    @doc "Transfer between two known accounts. The sender has to be the account from the wallet-table."
    (with-capability (TRANSFER sender amount)
      (coin.transfer-create sender receiver receiver-guard (+ amount extra))
      (coin.transfer receiver sender extra))
  )

  ; --------------------------------------------------------------------------
  ; Smart Wallet Functions

  (defun create-wallet-account:string (
      account:string
      guard:guard
    )
    @doc "Inserts an account into the wallet table safely. The account will have default customization"

    (enforce-guard guard)
    (insert wallet-table account {
      "guard": guard,
      "max-withdrawal": DEFAULT_MAX_WITHDRAWAL,
      "min-balance": DEFAULT_MIN_BALANCE,
      "time-between": DEFAULT_TIME_BETWEEN,
      "last-withdrawal-time": EPOCH
    })
    (coin.create-account account (wallet-module-guard))
    )

  (defun curr-time ()
    (at 'block-time (chain-data))
  )
)


(create-table wallet-table)
