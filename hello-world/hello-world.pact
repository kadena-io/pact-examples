(namespace "free")

(module hello "free.hello-keyset"
  "Pact hello-world with database example"

  (defschema hello-schema
    "VALUE stores greeting recipient"
    value:string)

  (deftable hellos:{hello-schema})

  (defun hello (value)
    "Store VALUE to say hello with."
    (write hellos "hello" { 'value: value }))

  (defun greet ()
    "Say hello to stored value."
    (with-read hellos "hello" { "value" := value }
      (format "Hello, {}!" [value])))
)

(create-table free.hellos)

(free.hello "world") ;; store "hello"
(free.greet)         ;; say hello!