(prove:plan 2)

(prove:ok
 (uiop/run-program:run-program "ls" :output t))

(prove:ok
 (uiop/run-program:run-program "ls" :output :string))

(prove:finalize)

