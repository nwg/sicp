- `(f 'x 'y)`
  - env for operator: no
  - env for operands:
    1. no
  - argl for operands:
    1. no
    2. no
  - proc for operand sequence: no
- `((f) 'x 'y)`
  - env for operator: yes
  - env for operands:
    1. no
  - argl for operands:
    1. no
    2. no
  - proc for operand sequence: no
- `(f (g 'x) y)`
  - env for operator: no
  - env for operands:
    1. yes
  - argl for operands:
    1. yes
    2. no
  - proc for operand sequence: yes
- `(f (g 'x) 'y)`
  - env for operator: no
  - env for operands:
    1. no
  - argl for operands:
    1. yes
    2. no
  - proc for operand sequence: yes
