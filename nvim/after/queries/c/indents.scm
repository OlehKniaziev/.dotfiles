;; inherits: c
((switch_statement
   condition: (_)
   body: (compound_statement
           (case_statement (compound_statement)? @indent.dedent)+ @indent.dedent @indent.branch)))

