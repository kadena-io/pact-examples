; define keysets by name in global namespace
(define-keyset 'keyset-carol
  (read-keyset "keyset-single-data"))

(define-keyset 'keyset-multi
  (read-keyset "keyset-multi-data"))

(define-keyset 'keyset-real
  (read-keyset "keyset-real-data"))

(enforce-keyset 'keyset-multi)
