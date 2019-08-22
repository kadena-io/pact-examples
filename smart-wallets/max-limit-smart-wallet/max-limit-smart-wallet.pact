(define-keyset 'smart-wallet-admin
  (read-keyset 'admin-keyset))

(module max-limit-smart-wallet 'smart-wallet-admin
  @doc"'smart-wallet' provides safe transactions of Kadena coins. \
  \Users will have custodial or non-custodial transactions with this wallet with two keysets. \
  \Custodial keyset will be encrypted and saved by the software, while non-custodial keysets  \
  \are entirely managed by the user. For convenient usage, users can set maximum amount of    \
  \ custodial transactions and allow small transactions with custodial keys. For bigger       \
  \transactions, users are required to provide non-custodial keys"

  (use coin)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema wallet
    custodial-guard:guard;keyset managed by software
    non-custodial-guard:guard;;keyset managed by the user
    max-custodial-amount:decimal;Maximum amount of transfer allowed with custodial keyset
    )

  (deftable wallet-table:{wallet})

  ; --------------------------------------------------------------------------
  ; Module Guards

  (defun wallet-module-guard:guard()
    (create-module-guard 'wallet-guard))

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap CUSTODIAL:bool (account)
    @doc "Lookup and enforce custodial guards associated with an account"
    (with-read wallet-table account {
        "custodial-guard":= guard
      }
      (enforce-guard guard)))

  (defcap NON_CUSTODIAL:bool (account)
    @doc "Lookup and enforce non-custodial guards associated with an account"
    (with-read wallet-table account {
        "non-custodial-guard":= guard
      }
      (enforce-guard guard)))

  (defcap AMOUNT_CHECK:bool (account:string amount:decimal)
    @doc "Lookup and enforce that the tx amount is in the range of allowed custodial tx amount"

    (with-read wallet-table account {
      "max-custodial-amount":= max
      }
      (enforce (> max amount) "Amount is bigger than maximum")))

  ; --------------------------------------------------------------------------
  ; Smart Wallet Functions

  (defun create-wallet-account:string (account:string custodial-guard:guard non-custodial-guard:guard max-custodial-amount:decimal)
    @doc "Create a coin account with ACCOUNT and WALLET_MODULE_GUARD in coin-table,     \
    \and create a wallet account with ACCOUNT, CUSTODIAL_GUARD, and NON_CUSTODIAL_GUARD,\
    \ and MAX_CUSTODIAL_AMOUNT "

    (enforce-guard custodial-guard)
    (enforce-guard non-custodial-guard)

    ;;Create Account in Kadena Coin Table with Wallet Module Guard.
    ;;This Prevents other contracts from tx functions of accounts created by the wallet.
    (create-account account (wallet-module-guard))
    (insert wallet-table account {
        "custodial-guard": custodial-guard,
        "non-custodial-guard": non-custodial-guard,
        "max-custodial-amount": max-custodial-amount
       }))

  (defun custodial-transfer (sender:string receiver:string amount:decimal)
    @doc "This function enforces that tx amount is smaller than SENDER's    \
    \MAX_CUSTODIAL_AMOUNT, and then enforces the SENDER's CUSTODIAL_GUARD. \
    \ If the conditions qualify, transfers AMOUNT of Kadena Coin from SENDER \
    \account to RECEIVER account."

    (with-capability (AMOUNT_CHECK sender amount)
      (with-capability (CUSTODIAL sender)
        (transfer sender receiver amount))))

  (defun non-custodial-transfer (sender:string receiver:string amount:decimal)
    @doc "This function enforces SENDER's NON_CUSTODIAL_GUARD and transfers \
    \ AMOUNT of Kadena coins from SENDER account to RECEIVER account without \
    \limit."

    (with-capability (NON_CUSTODIAL sender)
      (transfer sender receiver amount)))
)

(create-table wallet-table)
