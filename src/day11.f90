! This file is part of Advent2018.
! Copyright (C) 2018 Adam Hirst <adam@aphirst.karoo.co.uk>
!
! Advent2018 is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Advent2018 is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Advent2018. If not, see <http://www.gnu.org/licenses/>.

module Day11
  implicit none

  integer, parameter :: N = 300, serial = 9424

  private
  public :: Problem11

contains

  elemental integer function Fuel(i, j, serial)
    integer, intent(in) :: i, j, serial
    integer             :: rack

    rack = i + 10
    ! MODULO seems slightly faster than MOD in this application, with no difference in the result
    fuel = modulo( abs( ((rack*j+serial)*rack)/100 ), 10 ) - 5
  end function

  pure function SummedAreaTable(cells) result(sat)
    integer, intent(in) :: cells(:,:)
    integer             :: sat(size(cells,1), size(cells,2)), Ni, Nj, i, j

    Ni = size(cells,1)
    Nj = size(cells,2)
    if ( (Ni < 1) .or. (Nj < 1) ) error stop "Logikfehler: Datenfeld muss mindestens 1 Element enthalten."
    ! populate initial row and column
    sat(1,1) = cells(1,1)

    ! handle the initial row and column separately
    if (Ni > 1) then
      do i = 2, Ni
        sat(i,1) = cells(i,1) + sat(i-1,1)
      end do
    end if
    if (Nj > 1) then
      do j = 2, Nj
        sat(1,j) = cells(1,j) + sat(1,j-1)
      end do
    end if
    ! populate rest of summed-area table
    if ((Ni > 1) .and. (Nj > 1)) then
      do i = 2, Nj
        do j = 2, Nj
          sat(i,j) = cells(i,j) + sat(i,j-1) + sat(i-1,j) - sat(i-1,j-1)
        end do
      end do
    end if
  end function

  pure integer function EvaluateSum(sat, i1, i2, j1, j2) result(mysum)
    ! uses a pre-evaluated summed area table to compute the sum for the INCLUSIVE range [i1:i2, j1:j2]
    ! some effort was required to work out the correct offsets, as most implementations seem implicitly right-EXCLUSIVE
    integer, intent(in) :: sat(:,:), i1, i2, j1, j2
    integer             :: Ni, Nj

    Ni = size(sat,1)
    Nj = size(sat,2)
    if ( (i1-1 < 1) .and. (j1-1 < 1) ) then
      mysum = sat(min(i2,Ni),min(j2,Nj))
    else if (i1-1 < 1) then
      mysum = sat(min(i2,Ni),min(j2,Nj)) - sat(min(i2,Ni),j1-1)
    else if (j1-1 < 1) then
      mysum = sat(min(i2,Ni),min(j2,Nj)) - sat(i1-1,min(j2,Nj))
    else
      mysum = sat(i1-1,j1-1) + sat(min(i2,Ni),min(j2,Nj)) - sat(i1-1,min(j2,Nj)) - sat(min(i2,Ni),j1-1)
    end if
  end function

  pure function PowerArray(sat, k)
    integer, intent(in) :: sat(:,:), k
    integer             :: powerarray(size(sat,1)-k+1, size(sat,2)-k+1), i, j

    powerarray = reshape( [( [( EvaluateSum(sat, i, i+k-1, j, j+k-1), i = 1, N-k+1 )], j = 1, N-k+1 )], [N-k+1,N-k+1] )
  end function

  subroutine Problem11(c)
    integer, intent(out) :: c(3)
    integer              :: i, j, k, cells(N,N), sat(N,N), power(N,N), best_power, coord(2), best_coord(2), best_k

    ! no input file for this problem
    call system_clock(c(1))

    cells = reshape( [( [( Fuel(i, j, serial), i=1,N )], j=1,N )], [N,N] )

    ! Part 1: "What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power?"
    sat = SummedAreaTable(cells)

    best_power = minval(cells) ! less arbitrary than assuming that solution power > 0
    do k = 1, N ! size of square used to evaluate "fuel power" sum
      ! use `power` essentially like a scratch space, restricting indices appropriately
      power(:N-k+1,:N-k+1) = PowerArray(sat, k)
      coord = maxloc( power(:N-k+1,:N-k+1) )
      if (k == 3) then
        print "(2(a,i0))", "Ergebnis 1: ", coord(1), ",", coord(2)
        print "(2(a,i0))", "Richtig:    ", 243, ",", 72
        call system_clock(c(2))
      end if
      if (power(coord(1), coord(2)) > best_power) then
        best_coord = coord
        best_power = power(coord(1), coord(2))
        best_k = k
      end if
    end do

    ! Part 2: "What is the X,Y,size identifier of the square with the largest total power?"
    print "(3(a,i0))", "Ergebnis 2: ", best_coord(1), ",", best_coord(2), ",", best_k
    print "(3(a,i0))", "Richtig:    ", 229, ",", 192, ",", 11
    call system_clock(c(3))
  end subroutine

end module
