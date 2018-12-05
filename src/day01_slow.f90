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

module Day01_Slow

  implicit none

contains

  logical pure function IsEndDuplicate(array)
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

  subroutine Problem01b_bad()
    ! read file
    ! sum integers and keep all intermediates
    ! check each new intermediate against all old intermediates
    ! terminate at first duplicate
    integer              :: unit, iostat
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
  end subroutine

  integer recursive function BinarySearch(item, list) result(bestindex)
    integer, intent(in) :: item, list(:)
    integer             :: guess

    select case (size(list))
    case (0)
      bestindex = 1
    case (1)
      if (item <= list(1)) then
        bestindex = 1
      else
        bestindex = 2
      end if
    case default
      if (item > list(size(list))) then
        bestindex = size(list)+1
      else if (item < list(1)) then
        bestindex = 1
      end if
      guess = size(list)/2
      if (item == list(guess)) then
        bestindex = guess
      else if (item < list(guess)) then
        bestindex = BinarySearch(item, list(:guess-1))
      else
        bestindex = guess + BinarySearch(item, list(guess+1:))
      end if
    end select
  end function

  subroutine ListInsert(item, index, list)
    integer,              intent(in)     :: item, index
    integer, allocatable, intent(in out) :: list(:)

    select case (size(list))
    case (0)
      list = [ item ]
    case default
      if (index == 1) then
        list = [ item, list ]
      else if (index == size(list)+1) then
        list = [ list, item ]
      else
        list = [ list(1:index-1), item, list(index:size(list)) ]
      end if
    end select
  end subroutine

  subroutine TestSearch()
    integer, parameter :: list(8) = [-10, -8, -7, -3, 0, 12, 23, 24]
    integer            :: i

    do i = -11, 25
      print *, i, BinarySearch(i, list)
    end do
    stop
  end subroutine

  subroutine TestInsert()
    integer, allocatable :: list(:)
    integer              :: item, i

    allocate(list(0))
    do
      print *, "Neuer Eintrag:"
      read *, item
      i = BinarySearch(item, list)
      if (list(i) == item) then
        print "(*(i3))", list
        print *, "Gefunden an der Stelle:", i
      else
        call ListInsert(item, i, list)
        print "(*(i3))", list
        print *, ""
      end if
    end do
    stop
  end subroutine

  subroutine Problem01b_better()
    ! TODO: Smart version of Problem01b
    ! keep the list of intermediates sorted, use binary search/insertion
    integer              :: unit, iostat
    integer              :: nextint, total, i, index
    integer, allocatable :: diffs(:), totals(:)

    total = 0
    allocate(diffs(0))
    allocate(totals(0))

    open(newunit=unit, file="day01.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit, *, iostat=iostat) nextint
    do while (iostat == 0)
      diffs = [ diffs, nextint ]
      read(unit, *, iostat=iostat) nextint
    end do
    close(unit)

    outer: do
      do i = 1, size(diffs)
        total = total + diffs(i)
        index = BinarySearch(total, totals)
        if (totals(index) == total) then
          exit outer
        else
          call ListInsert(total, index, totals)
        end if
      end do
      print *, total
    end do outer
    print "(a,i0)", "Ergebnis: ", total
  end subroutine
end module
