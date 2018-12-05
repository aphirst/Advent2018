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

module Day05_Slow
  implicit none

  character(26), parameter :: large = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  character(26), parameter :: small = 'abcdefghijklmnopqrstuvwxyz'

  integer,       parameter :: caps_offset = iachar("a") - iachar("A")

  private
  public :: Problem05a_bad, Problem05b_bad

contains

  subroutine ReadPolymer(polymer)
    character(:), allocatable, intent(out) :: polymer
    integer                                :: unit, iostat, N

    call execute_command_line("rm day05_length.txt")
    call execute_command_line("expr `wc -m < day05.txt` - `wc -l < day05.txt` > day05_length.txt")
    open(newunit=unit, file="day05_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit,*) N
    close(unit)
    allocate(character(len=N) :: polymer)

    open(newunit=unit, file="day05.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit,"(a)") polymer
    close(unit)
  end subroutine

  pure function StringToArray(mystring) result(myarray)
    character(*), intent(in) :: mystring
    character(1)             :: myarray(len(mystring))
    integer                  :: i

    myarray = [( mystring(i:i), i = 1, len(mystring) )]
  end function

  logical function EqualCaps(string)
    ! a = A + caps_offset
    character(1), intent(in) :: string(2)
    integer                  :: i(2)

    i = iachar(string)
    equalcaps = .false.
    ! case 1: Aa
    if ((iachar("A")<=i(1)) .and. (i(1)<=iachar("Z"))) then
      if (i(1) == i(2) - caps_offset) then
        equalcaps = .true.
      end if
    ! case 2: aA
    else if ((iachar("A")<=i(2)) .and. (i(2)<=iachar("Z"))) then
      if (i(1) == i(2) + caps_offset) then
        equalcaps = .true.
      end if
    end if
  end function

  function FullCollapse(polymer_in) result(collapsed)
    character(1), intent(in)              :: polymer_in(:)
    character(1),             allocatable :: polymer(:), collapsed(:)
    integer                               :: i

    polymer = polymer_in
    do
      ! eliminate dupes
      do i = 1, size(polymer)-1
        if (polymer(i) == " ") then
          cycle
        else if ( EqualCaps(polymer(i:i+1)) ) then
          polymer(i:i+1) = " "
        end if
      end do
      ! collapse
      collapsed = pack(polymer, polymer /= " ")
      if (size(collapsed) == size(polymer)) return
      call move_alloc(collapsed, polymer)
    end do
  end function

  subroutine Problem05a_bad()
    character(:), allocatable :: polymer
    character(1), allocatable :: polymer_array(:), collapsed_array(:)

    call ReadPolymer(polymer)
    polymer_array = StringToArray(polymer)
    collapsed_array = FullCollapse(polymer_array)
    print "(a,i0)", "Ergebnis: ", size(collapsed_array)
  end subroutine

  function RemoveLetter(polymer, i)
    character(1), intent(in)  :: polymer(:)
    character(1), allocatable :: removeletter(:)
    integer,      intent(in)  :: i

    removeletter = pack(polymer, ( (polymer/=large(i:i)) .and. (polymer /= small(i:i)) ))
  end function

  subroutine Problem05b_bad()
    character(:), allocatable :: polymer
    character(1), allocatable :: polymer_array(:), polymer_trim(:), collapsed_array(:)
    integer                   :: i, length(26)

    call ReadPolymer(polymer)
    polymer_array = StringToArray(polymer)
    do i = 1, 26
      polymer_trim = RemoveLetter(polymer_array, i)
      collapsed_array = FullCollapse(polymer_trim)
      length(i) = size(collapsed_array)
    end do
    print "(a,i0)", "Ergebnis: ", minval(length)
  end subroutine

end module
