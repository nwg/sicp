The ratio over interpreted performancce is computed as r in the below table:

|                           | Maximum Depth  (n=10) | Pushes (n=10) | Maximum Depth (n=15) | Pushes (n=15) | Maximum Depth (n=20) |  Pushes (n=20) |
|---------------------------|:---------------------:|:-------------:|:--------------------:|:-------------:|:--------------------:|:--------------:|
| Interpreted               |           53          |      4944     |          78          |     55232     |          103         |     612936     |
| Special-purpose           |       18 (r=.34)      |  352 (r=.07)  |      28 (r=.36)      |  3944 (r=.07) |      38 (r=.37)      |  43780 (r=.07) |
| Compiled (w/ open coding) |       21 (r=.39)      |  974 (r=.20)  |      31 (r=.40)      | 10852 (r=.20) |      41 (r=.40)      | 120401 (r=.20) |

The special-purpose machine heavily outperforms the compiled program in this case.

The compiled procedure has:
* pushes: pushes(n-1) + pushes(n-2) + 5, pushes(0) = 6, pushes(1) = 6
* depth: 2n + 1

The specal-purpose procedure has:
* pushes: pushes(n-1) + pushes(n-2) + 40, pushes(0) = 16, pushes(1) = 16
* depth: 5*n + 3
