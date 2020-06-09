(module max-limit-smart-wallet GOVERNANCE
  @doc"'smart-wallet' provides safe transactions of Kadena coins. \
  \Users will have low-security and high-security transactions with this wallet with two keysets. \
  \Low-security keyset will be encrypted and saved by the software, while high-security keysets  \
  \are entirely managed by the user. For convenient usage, users can set maximum amount of    \
  \ low-security transactions and allow small transactions with low-seucrity keys. For bigger       \
  \transactions, users are required to provide high-security keys"

  (use coin)

  (defcap GOVERNANCE()
    (enforce false "Enforce non-upgradeability except in the case of a hard fork"))

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema wallet
    low-security-guard:guard ;keyset managed by software
    high-security-guard:guard ;;keyset managed by the user
    max-low-security-amount:decimal;Maximum amount of transfer allowed with low-security keyset
    low-security-withdrawal-time-limit:decimal
    last-low-security-withdrawal-time:time
    )

  (deftable wallet-table:{wallet})

  ; --------------------------------------------------------------------------
  ; Module Guards

  (defun wallet-module-guard:guard()
    (create-module-guard 'wallet-guard))

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap LOW_SECURITY:bool (account)
    @doc "Lookup and enforce low-security guards associated with an account"
    (with-read wallet-table account {
        "low-security-guard":= guard
      }
      (enforce-guard guard)
      ))

  (defcap HIGH_SECURITY:bool (account)
    @doc "Lookup and enforce high-security guards associated with an account"
    (with-read wallet-table account {
        "high-security-guard":= guard
      }
      (enforce-guard guard)))

  (defcap AMOUNT_CHECK:bool (account:string amount:decimal)
    @doc "Lookup and enforce that the tx amount is in the range of allowed low-security tx amount"

    (with-read wallet-table account {
      "max-low-security-amount":= max
      }
      (enforce (> max amount) "Amount is bigger than maximum")))

  (defcap TIME_CHECK:bool (account:string)
    @doc "Lookup and enforce low-security guards associated with an account"
    (with-read wallet-table account {
        "low-security-withdrawal-time-limit":= time-limit,
        "last-low-security-withdrawal-time":= last-withdrawal-time}
    (enforce (>=
      (diff-time (curr-time) last-withdrawal-time)
      time-limit)
      "Withdrawal time limit has not met" )))

  ; --------------------------------------------------------------------------
  ; Constants

  (defconst EPOCH (time "1970-01-01T00:00:00Z"))

  ; --------------------------------------------------------------------------
  ; Smart Wallet Functions

  (defun create-wallet-account:string (
    account:string
    low-security-guard:guard
    high-security-guard:guard
    max-low-security-amount:decimal
    low-security-withdrawal-time-limit:decimal)
    @doc "Create a coin account with ACCOUNT and WALLET_MODULE_GUARD in coin-table,     \
    \and create a wallet account with ACCOUNT, LOW_SECURITY_GUARD, and HIGH_SECURITY_GUARD,\
    \ and MAX_LOW_SECURITY_AMOUNT "

    (enforce-guard low-security-guard)
    (enforce-guard high-security-guard)

    ;;Create Account in Kadena Coin Table with Wallet Module Guard.
    ;;This Prevents other contracts from tx functions of accounts created by the wallet.
    (create-account account (wallet-module-guard))
    (insert wallet-table account {
        "low-security-guard": low-security-guard,
        "high-security-guard": high-security-guard,
        "max-low-security-amount": max-low-security-amount,
        "low-security-withdrawal-time-limit": low-security-withdrawal-time-limit,
        "last-low-security-withdrawal-time": EPOCH
       }))

  (defun low-security-transfer:string (sender:string receiver:string amount:decimal)
    @doc "This function enforces that tx amount is smaller than SENDER's    \
    \MAX_LOW_SECURITY_AMOUNT, and then enforces the SENDER's HIGH_SECURITY_GUARD. \
    \ If the conditions qualify, transfers AMOUNT of Kadena Coin from SENDER \
    \account to RECEIVER account."

    (with-capability (LOW_SECURITY sender)
      (with-capability (TIME_CHECK sender)
        (with-capability (AMOUNT_CHECK sender amount)
          (transfer sender receiver amount)
          (update wallet-table sender {
            "last-low-security-withdrawal-time":(curr-time)
            })))))

  (defun high-security-transfer:string (sender:string receiver:string amount:decimal)
    @doc "This function enforces SENDER's HIGH_SECURITY_GUARD and transfers \
    \ AMOUNT of Kadena coins from SENDER account to RECEIVER account without \
    \limit."

    (with-capability (HIGH_SECURITY sender)
      (transfer sender receiver amount)))

  (defun update-max-low-security-amount:string (account:string amount:decimal)
    @doc "This function enforces ACCOUNT's high-security keyset and updates \
    \ACCOUNT's max-low-security-amount to AMOUNT"

    (with-capability (HIGH_SECURITY account)
      (update wallet-table account {
        "max-low-security-amount": amount
        })))

  (defun update-low-security-withdrawal-time-limit:string (account:string withdrawal-time-limit:decimal)
    @doc "This function enforces ACCOUNT's high-security keyset and updates \
    \ACCOUNT's low-security-withdrawal-time-limit to WITHDRAWAL_TIME_LIMIT"

    (with-capability (HIGH_SECURITY account)
      (update wallet-table account {
        "low-security-withdrawal-time-limit": withdrawal-time-limit
        })))

  (defun rotate-low-security-guard:string (account:string guard:guard)
    @doc "This function enforces ACCOUNT's low-security-guard and presented GUARD\
    \ and replaces ACCOUNT's low-security-guard with GUARD"

    (enforce-guard guard)
    (with-capability (LOW_SECURITY account)
      (update wallet-table account {
        "low-security-guard": guard
        })))

  (defun rotate-high-security-guard:string (account:string guard:guard)
    @doc "This function enforces ACCOUNT's high-security-guard and presented GUARD \
    \ and replaces ACCOUNT's high-security-guard with GUARD"

    (enforce-guard guard)
    (with-capability (HIGH_SECURITY account)
      (update wallet-table account {
        "high-security-guard": guard
        })))

  (defun curr-time ()
    (at 'block-time (chain-data))
  )
)

(create-table wallet-table)
