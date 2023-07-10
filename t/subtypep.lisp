(prove:plan 4)

;;;; <https://github.com/armedbear/abcl/issues/433>
(prove:is-values
 (subtypep '(or null cons) 'list)
 '(t t))

(prove:is-values
 (subtypep 'list '(or null cons))
 '(t t))

(prove:is-values
 (subtypep 'null '(and symbol list))
 '(t t))

(prove:is-values
 (subtypep '(and symbol list) 'null)
 '(t t))

(prove:finalize)
