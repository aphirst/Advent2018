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

  implicit none

  integer :: day

  print *, "Grüß Gott!"
  print *, "Gib doch mal das Datum ein: (Tagesnummer in Dezember)"
  read (*,*) day
  select case (day)
  case (1)
    call Problem01a()
    call Problem01b()
  case (2)
    call Problem02a()
    call Problem02b()
  case default
    stop "Kein gültiger Dezembertag!"
  end select
  print *, "Frohe Weihnachten! Tschüss!"

end program

