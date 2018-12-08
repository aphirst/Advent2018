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

program Advent2018
  use Day01
  use Day02
  use Day03
  use Day04
  use Day05
  use Day06
  use Day07

  implicit none

  integer :: i, c(3), cr, cm
  real    :: rate

  print *, "Grüß Gott!"
  print *, ""

  call system_clock(count_rate=cr)
  call system_clock(count_max=cm)
  rate = real(cr)

  do i = 1, 25
    call system_clock(c(1))
    select case (i)
    case (1)
      print *, "1. Tag:"
      call Problem01(c(2:3))
    case (2)
      print *, "2. Tag:"
      call Problem02(c(2:3))
    case (3)
      print *, "3. Tag:"
      call Problem03(c(2:3))
    case (4)
      print *, "4. Tag:"
      call Problem04(c(2:3))
    case (5)
      print *, "5. Tag:"
      call Problem05(c(2:3))
    case (6)
      print *, "6. Tag:"
      call Problem06(c(2:3))
    case (7)
      print *, "7. Tag:"
      call Problem07(c(2:3))
    case default
      cycle
    end select
    print "(3(a,f0.3))", "Laufzeiten: ", (c(2)-c(1))/rate, " ", (c(3)-c(2))/rate, " ", (c(3)-c(1))/rate
    print *, ""
  end do

  print *, "Frohe Weihnachten! Tschüss!"

end program

