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

module Day01
  implicit none


  contains

  subroutine Problem01a()
    ! read file
    ! sum up integers
    ! spit out output
    integer       :: unit, iostat
    integer       :: total, nextint

    total = 0
    nextint = 0

    open(newunit=unit, file="day01.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    do while (iostat == 0)
      total = total + nextint
      read(unit, *, iostat=iostat) nextint
    end do
    close(unit)
    print "(a,i0)", "Ergebnis: ", total
  end subroutine

  logical function IsEndDuplicate(array)
    integer, intent(in) :: array(:)
    integer             :: i

    do i = lbound(array,1), ubound(array,1)-1
      if (array(ubound(array,1)) == array(i)) then
        isendduplicate = .true.
        return
      end if
    end do
    isendduplicate = .false.
  end function

  subroutine Problem01b()
    ! read file
    ! sum integers and keep all intermediates
    ! check each new intermediate against all old intermediates
    ! terminate at first duplicate
    integer              :: unit, iostat, i
    integer              :: nextint
    integer, allocatable :: total(:)

    allocate(total(1))
    total = [0]

    outer: do
      open(newunit=unit, file="day01.txt", iostat=iostat, status="old")
      if (iostat /= 0) stop "Datenfehler."
      read(unit, *, iostat=iostat) nextint
      inner: do while (iostat == 0)
        total = [ total, total(ubound(total,1)) + nextint ]
        if (IsEndDuplicate(total)) exit outer
        read(unit, *, iostat=iostat) nextint
      end do inner
      close(unit)
    end do outer
    print "(a,i0)", "Ergebnis: ", total(ubound(total,1))
    open(newunit=unit, file="day01_naive.log", status="new")
    do i = 1, size(total)
      write(unit,*) total(i)
    end do
    close(unit)
  end subroutine

  subroutine Problem01b_smart()
    ! TODO: Smart version of Problem01b
    ! keep the list of intermediates sorted, use binary search/insertion
    ! can also represent as a tree
  end subroutine

end module
