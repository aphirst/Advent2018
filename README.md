# Advent2018
My attempts at the [Advent of Code 2018](https://adventofcode.com/2018).

## Benchmarking

Written in [Code::Blocks for Fortran](http://http://cbfortran.sourceforge.net/).
Compiled using `GNU Fortran (GCC) 8.2.1 20181127`.

<table>
<tr><th>Problem</th><th colspan=2>ThinkPad T430<br>i7-3520M CPU @ 2.90GHz</th><th colspan=2>ThinkPad X230<br>i5-3320M CPU @ 2.60GHz</th><th colspan=2>ODROID-XU4<br>ARM Cortex-A7 @ 2.00GHz</th></tr>
<tr></tr>
<tr><th>01</th><td>0.002 s</td><td>0.009 s</td><td>0.001 s</td><td>0.009 s</td><td>0.004 s</td><td>0.018 s</td></tr>
<tr><th>02</th><td>0.001 s</td><td>0.003 s</td><td>0.001 s</td><td>0.004 s</td><td>0.003 s</td><td>0.008 s</td></tr>
<tr><th>03</th><td>0.010 s</td><td>0.008 s</td><td>0.011 s</td><td>0.007 s</td><td>0.043 s</td><td>0.011 s</td></tr>
<tr><th>04</th><td>0.026 s</td><td>0.000 s</td><td>0.028 s</td><td>0.000 s</td><td>0.071 s</td><td>0.001 s</td></tr>
<tr><th>05</th><td>0.019 s</td><td>0.031 s</td><td>0.019 s</td><td>0.033 s</td><td>0.051 s</td><td>0.072 s</td></tr>
<tr><th>06</th><td>0.020 s</td><td>0.003 s</td><td>0.024 s</td><td>0.002 s</td><td>0.057 s</td><td>0.017 s</td></tr>
<tr><th>07</th><td>0.010 s</td><td> -     </td><td>0.011 s</td><td> -     </td><td>0.031 s</td><td> -     </td></tr>
<tr><th>08</th><td>-      </td><td> -     </td><td> -     </td><td> -     </td><td> -     </td><td> -     </td></tr>
<tr><th>09</th><td> -     </td><td> -     </td><td>0.003 s</td><td>0.368 s</td><td>0.007 s</td><td>0.868 s</td></tr>
<tr></tr>
<tr><th>Total</th><th colspan=2 align="center">0.738 s</th><th colspan=2>0.173 s</th><th colspan=2>2.144 s</th></tr>
</table>
