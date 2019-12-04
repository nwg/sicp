- `(f 'x 'y)`
  - env for operator: no
  - env for operands:
    - arg1: no
  - argl for operands:
    - arg1. no
    - arg2: no
  - proc for operand sequence: no
- `((f) 'x 'y)`
  - env for operator: yes
  - env for operands:
    -arg1: no
  - argl for operands:
    -arg1: no
    -arg2: no
  - proc for operand sequence: no
- `(f (g 'x) y)`
  - env for operator: no
  - env for operands:
    -arg1: yes
  - argl for operands:
    arg1: yes
    arg2: no
  - proc for operand sequence: yes
- `(f (g 'x) 'y)`
  - env for operator: no
  - env for operands:
    arg1: no
  - argl for operands:
    arg1: yes
    arg2: no
  - proc for operand sequence: yes
