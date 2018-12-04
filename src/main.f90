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

  implicit none

  integer :: i
  real    :: t(2)

  print *, "Grüß Gott!"
  print *, ""

  do i = 1, 60
    call cpu_time(t(1))
    select case (i)
    case (1)
      print *, "1. Tag"
      call Problem01a()
    case (2)
      !call Problem01b()
      !call Problem01b_smart()
      call Problem01b_smarter()
    case (3)
      print *, "2. Tag"
      call Problem02a()
    case (4)
      call Problem02b()
    case (5)
      print *, "3. Tag"
      call Problem03a()
    case (6)
      call Problem03b()
    case (7)
      print *, "4. Tag"
      call Problem04a()
    case (8)
      call Problem04b()
    case default
      exit
    end select
    call cpu_time(t(2))
    print "(a,f8.3)", "Programmlaufzeit:", t(2)-t(1)
    print *, ""
  end do

  print *, "Frohe Weihnachten! Tschüss!"

end program

