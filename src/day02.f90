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

module Day02
  implicit none

  private
  public :: Problem02a, Problem02b

contains

  function CountInstances(string)
    ! take a single string (character variable of nonzero length)
    ! return an array where each element is the # of times a given letter appears
    ! all input is lower case
    character(26), intent(in) :: string
    integer                   :: i, j, countinstances(iachar("a"):iachar("z"))

    do i = iachar("a"), iachar("z")
      countinstances(i) = count( [( string(j:j) == achar(i), j = 1, len(string) )] )
    end do
  end function

  subroutine UpdateChecksumCache(instances, twice, thrice)
    ! for each letter which occured exactly twice or thrice, update the counters
    integer, intent(in)     :: instances(26)
    integer, intent(in out) :: twice, thrice

    if (any(instances == 2)) then
      twice = twice + 1
    end if
    if (any(instances == 3)) then
      thrice = thrice + 1
    end if
  end subroutine

  subroutine Problem02a()
    ! read list of strings (assume all strings same length)
    ! count instances of each of 26 possible letters (all lowercase)
    ! save whether any letter occurred exactly twice
    ! same for exactly thrice
    ! repeat for all strings
    ! checksum is (# of strings with a 2-duplicate) * (# with a 3-duplicate)
    integer       :: unit, iostat, twice, thrice, checksum
    character(26) :: string

    twice = 0
    thrice = 0

    open(newunit=unit, file="input/day02.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit, *, iostat=iostat) string
    do while (iostat == 0)
      call UpdateChecksumCache(CountInstances(string), twice, thrice)
      read(unit, *, iostat=iostat) string
    end do
    close(unit)
    checksum = twice * thrice
    print "(a,i0)", "Ergebnis: ", checksum
  end subroutine

  subroutine Problem02b()
    ! read entire array of strings
    ! for each string, compare with all other strings
    ! get the indices of different characters
    ! if list has only 1 index, output string with the different element sliced out
    integer                    :: unit, iostat, i, j, k
    character(26)              :: stringtemp
    character(26), allocatable :: string(:)
    integer,       allocatable :: diff(:)

    allocate(string(0))

    open(newunit=unit, file="input/day02.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit, *, iostat=iostat) stringtemp
    do while (iostat == 0)
      string = [string, stringtemp]
      read(unit, *, iostat=iostat) stringtemp
    end do
    close(unit)

    outer: do i = 1, size(string)
      inner: do j = i, size(string)
        if (i == j) cycle inner
        if (allocated(diff)) deallocate(diff)
        allocate(diff(0))
        do k = 1, 26
          if (size(diff) > 1) cycle inner
          if (string(i)(k:k) /= string(j)(k:k)) then
            diff = [diff, k]
          end if
        end do
        if (size(diff) == 1) then
          print "(3a)", "Ergebnis: ", string(i)(1:diff(1)-1), string(i)(diff(1)+1:26)
          return
        end if
      end do inner
    end do outer
  end subroutine
end module
