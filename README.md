# Advent2018
My attempts at the [Advent of Code 2018](https://adventofcode.com/2018).

## Benchmarking

Written in [Code::Blocks for Fortran](http://http://cbfortran.sourceforge.net/).
Compiled using `GNU Fortran (GCC) 8.2.1 20181127`.

<table>
<tr><th>Problem</th><th colspan=3>ThinkPad T430<br>i7-3520M CPU @ 2.90GHz</th><th colspan=3>ThinkPad X230<br>i5-3320M CPU @ 2.60GHz</th><th colspan=3>ODROID-XU4 (big.LITTLE)<br>ARM Cortex-A15 @ 2.10GHz<br>ARM Cortex-A7 @ 1.50GHz</th></tr>
<tr></tr>
<tr><th></th>   <th>Input</th>  <th>Part 1</th>  <th>Part 2</th> <th>Input</th>  <th>Part 1</th> <th>Part 2</th>  <th>Input</th>  <th>Part 1</th> <th>Part 2</th></tr>
<tr><th>01</th> <td>0.011 s</td><td>0.000 s</td><td>0.009 s</td> <td>0.010 s</td><td>0.000 s</td><td>0.009 s</td> <td>0.032 s</td><td>0.000 s</td><td>0.019 s</td></tr>
<tr><th>02</th> <td>0.013 s</td><td>0.000 s</td><td>0.004 s</td> <td>0.011 s</td><td>0.000 s</td><td>0.004 s</td> <td>0.031 s</td><td>0.001 s</td><td>0.009 s</td></tr>
<tr><th>03</th> <td>0.014 s</td><td>0.003 s</td><td>0.002 s</td> <td>0.013 s</td><td>0.003 s</td><td>0.003 s</td> <td>0.046 s</td><td>0.021 s</td><td>0.005 s</td></tr>
<tr><th>04</th> <td>0.032 s</td><td>0.000 s</td><td>0.000 s</td> <td>0.028 s</td><td>0.000 s</td><td>0.000 s</td> <td>0.077 s</td><td>0.000 s</td><td>0.001 s</td></tr>
<tr><th>05</th> <td>0.016 s</td><td>0.003 s</td><td>0.028 s</td> <td>0.016 s</td><td>0.004 s</td><td>0.035 s</td> <td>0.047 s</td><td>0.011 s</td><td>0.083 s</td></tr>
<tr><th>06</th> <td>0.011 s</td><td>0.011 s</td><td>0.002 s</td> <td>0.011 s</td><td>0.012 s</td><td>0.003 s</td> <td>0.036 s</td><td>0.034 s</td><td>0.017 s</td></tr>
<tr><th>07</th> <td>0.011 s</td><td>0.000 s</td><td>0.000 s</td> <td>0.012 s</td><td>0.000 s</td><td>0.000 s</td> <td>0.035 s</td><td>0.000 s</td><td>0.000 s</td></tr>
<tr><th>08</th> <td>0.015 s</td><td>0.000 s</td><td>0.000 s</td> <td>0.017 s</td><td>0.000 s</td><td>0.000 s</td> <td>0.047 s</td><td>0.001 s</td><td>0.000 s</td></tr>
<tr><th>09</th> <td>0.000 s</td><td>0.003 s</td><td>0.277 s</td> <td>0.000 s</td><td>0.003 s</td><td>0.369 s</td> <td>0.000 s</td><td>0.008 s</td><td>0.994 s</td></tr>
<tr><th>10</th> <td>0.015 s</td><td>0.000 s</td><td>0.000 s</td> <td>0.013 s</td><td>0.000 s</td><td>0.000 s</td> <td>0.041 s</td><td>0.000 s</td><td>0.000 s</td></tr>
<tr><th>11</th> <td>0.000 s</td><td>0.003 s</td><td>0.039 s</td> <td>0.000 s</td><td>0.004 s</td><td>0.041 s</td> <td>0.000 s</td><td>0.013 s</td><td>0.097 s</td></tr>
<tr></tr>
<tr><th>Total</th><th colspan=3>0.536 s</th><th colspan=3>0.643 s</th><th colspan=3>1.666 s</th></tr>
</table>

Note: Final routine time is taken *before* any explicit destructor calls, some of which take upwards of 0.100s to complete.
