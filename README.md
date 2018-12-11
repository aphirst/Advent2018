# Advent2018
My attempts at the [Advent of Code 2018](https://adventofcode.com/2018).

## Benchmarking

Written in [Code::Blocks for Fortran](http://http://cbfortran.sourceforge.net/).
Compiled using `GNU Fortran (GCC) 8.2.1 20181127`.

<table>
<tr><th>Problem</th><th colspan=2>ThinkPad T430<br>i7-3520M CPU @ 2.90GHz</th><th colspan=2>ThinkPad X230<br>i5-3320M CPU @ 2.60GHz</th><th colspan=2>ODROID-XU4<br>ARM Cortex-A7 @ 2.00GHz</th></tr>
<tr></tr>
<tr><th>01</th><td>0.003 s</td><td>0.008 s</td><td>0.001 s</td><td>0.009 s</td><td>0.004 s</td><td>0.016 s</td></tr>
<tr><th>02</th><td>0.000 s</td><td>0.004 s</td><td>0.000 s</td><td>0.005 s</td><td>0.003 s</td><td>0.009 s</td></tr>
<tr><th>03</th><td>0.013 s</td><td>0.007 s</td><td>0.009 s</td><td>0.007 s</td><td>0.040 s</td><td>0.012 s</td></tr>
<tr><th>04</th><td>0.027 s</td><td>0.000 s</td><td>0.027 s</td><td>0.000 s</td><td>0.083 s</td><td>0.000 s</td></tr>
<tr><th>05</th><td>0.019 s</td><td>0.029 s</td><td>0.020 s</td><td>0.034 s</td><td>0.054 s</td><td>0.077 s</td></tr>
<tr><th>06</th><td>0.020 s</td><td>0.003 s</td><td>0.023 s</td><td>0.002 s</td><td>0.066 s</td><td>0.018 s</td></tr>
<tr><th>07</th><td>0.012 s</td><td> -     </td><td>0.011 s</td><td> -     </td><td>0.032 s</td><td> -     </td></tr>
<tr><th>08</th><td>-      </td><td> -     </td><td> -     </td><td> -     </td><td> -     </td><td> -     </td></tr>
<tr><th>09</th><td>0.002 s</td><td>0.277 s</td><td>0.003 s</td><td>0.366 s</td><td>0.007 s</td><td>0.830 s</td></tr>
<tr><th>10</th><td>0.010 s</td><td>0.000 s</td><td>0.011 s</td><td>0.000 s</td><td>0.038 s</td><td>0.000 s</td></tr>
<tr><th>11</th><td>0.003 s</td><td>0.038 s</td><td>0.003 s</td><td>0.043 s</td><td>0.010 s</td><td>0.088 s</td></tr>
<tr></tr>
<tr><th>Total</th><th colspan=2 align="center">0.486 s</th><th colspan=2>0.566 s</th><th colspan=2>1.406 s</th></tr>
</table>

Note: Final routine time is taken *before* any explicit destructor calls, some of which take upwards of 0.100s to complete.
