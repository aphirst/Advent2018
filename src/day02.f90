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
  public :: Problem02

contains

  subroutine ReadStrings(strings)
    character(26), allocatable :: strings(:)
    integer                    :: unit, iostat, num_strings, i

    call execute_command_line("rm input/day02_length.txt")
    call execute_command_line("expr `wc -l < input/day02.txt` > input/day02_length.txt")
    open(newunit=unit, file="input/day02_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) num_strings
    close(unit)
    allocate(strings(num_strings))

    open(newunit=unit, file="input/day02.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    do i = 1, num_strings
      read(unit, *) strings(i)
    end do
    close(unit)
  end subroutine

  pure function CountInstances(string)
    ! take a single string (character variable of nonzero length)
    ! return an array where each element is the # of times a given letter appears
    ! all input is lower case
    character(26), intent(in) :: string
    integer                   :: i, j, countinstances(iachar("a"):iachar("z"))

    do i = iachar("a"), iachar("z")
      countinstances(i) = count( [( string(j:j) == achar(i), j = 1, len(string) )] )
    end do
  end function

  pure subroutine UpdateChecksumCache(instances, twice, thrice)
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

  subroutine Problem02(c)
    integer,       intent(out)              :: c(2)
    character(26),              allocatable :: strings(:)
    integer                                 :: i, j, k, twice, thrice, checksum
    integer,                    allocatable :: diff(:)
    character(26)                           :: string_common

    call ReadStrings(strings)

    ! Part 1: "Count the number of boxes with an ID containing exactly two of
    ! any letter and then separately with exactly three of any letter.
    ! Multiply those two counts together to get a rudimentary checksum.
    ! What is the checksum for your list of box IDs?"
    twice = 0
    thrice = 0

    do i = 1, size(strings)
      call UpdateChecksumCache(CountInstances(strings(i)), twice, thrice)
    end do
    checksum = twice * thrice

    print "(a,i0)", "Ergebnis 1: ", checksum
    print "(a,i0)", "Richtig:    ", 7808
    call system_clock(c(1))

    ! Part 2: "What letters are common between the two correct box IDs?
    ! These will differ by exactly one character at the same position in both strings."

    ! read entire array of strings
    ! for each string, compare with all other strings
    ! get the indices of different characters
    ! if list has only 1 index, output string with the different element sliced out
    outer: do i = 1, size(strings)
      inner: do j = i+1, size(strings)
        if (allocated(diff)) deallocate(diff)
        allocate(diff(0))
        do k = 1, 26
          if (size(diff) > 1) cycle inner
          if (strings(i)(k:k) /= strings(j)(k:k)) then
            diff = [diff, k]
          end if
        end do
        if (size(diff) == 1) then
          string_common = strings(i)(1:diff(1)-1)//strings(i)(diff(1)+1:)
          exit outer
        end if
      end do inner
    end do outer

    print "(3a)", "Ergebnis 2: ", string_common
    print "(2a)", "Richtig:    ", "efmyhuckqldtwjyvisipargno"
    call system_clock(c(2))
  end subroutine

end module
