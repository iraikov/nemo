
(require-extension ersatz-lib)

(make-lexer 
 begin-expand: "{{" end-expand: "}}" 
 begin-logic: "{%" end-logic: "%}" 
 lexer-proc: 'nemo-ersatz-lexer
 compile: #t)

