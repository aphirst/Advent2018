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

module Day05
  implicit none

  character(26), parameter :: large = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  character(26), parameter :: small = 'abcdefghijklmnopqrstuvwxyz'

  private
  public :: Problem05a, Problem05b

contains

  subroutine ReadPolymer(polymer)
    integer                   :: unit, iostat, N
    character(:), allocatable :: polymer

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

  pure subroutine Alchemy(pair, is_blanked)

    character(2), intent(in out) :: pair
    logical,      intent(out)    :: is_blanked
    integer                      :: i_large(2), i_small(2)

    i_large(1) = index(large, pair(1:1))
    i_large(2) = index(large, pair(2:2))
    i_small(1) = index(small, pair(1:1))
    i_small(2) = index(small, pair(2:2))
    if ( ((i_large(1) == i_small(2)) .and. (i_large(1) /= 0)) .or. ((i_large(2) == i_small(1)) .and. (i_large(2) /= 0)) ) then
      pair = "  "
      is_blanked = .true.
    else
      is_blanked = .false.
    end if
  end subroutine

  pure recursive function Collapse(polymer) result(collapsed)
    ! collapse all regions with spaces
    character(*), intent(in)              :: polymer
    character(:),             allocatable :: collapsed
    integer                               :: i

    i = index(polymer, ' ')
    if (i == 0) then
      collapsed = polymer
    else
      collapsed = polymer(1:i-1) // Collapse(polymer(i+1:))
    end if
  end function

  pure function StringToArray(mystring) result(myarray)
    character(*), intent(in) :: mystring
    character(1)             :: myarray(len(mystring))
    integer                  :: i

    do i = 1, len(mystring)
      myarray(i) = mystring(i:i)
    end do
  end function

  pure function ArrayToString(myarray) result(mystring)
    character(1),             intent(in) :: myarray(:)
    character(size(myarray))             :: mystring
    integer                              :: i

    do i = 1, size(myarray)
      mystring(i:i) = myarray(i)
    end do
  end function

  pure function Collapse_smart(polymer) result(collapsed)
    character(*), intent(in)              :: polymer
    character(:),             allocatable :: collapsed
    character(1),             allocatable :: polymer_array(:), collapsed_array(:)

    polymer_array = StringToArray(polymer)
    collapsed_array = pack(polymer_array, polymer_array /= " ")
    collapsed = ArrayToString(collapsed_array)
  end function

  function FullCollapse(polymer_in) result(collapsed)
    character(*), intent(in)              :: polymer_in
    character(:),             allocatable :: polymer, collapsed
    logical                               :: is_blanked
    integer                               :: i

    polymer = polymer_in
    do
      is_blanked = .false.
      do i = 1, len(polymer)-1
        if (is_blanked) then
          is_blanked = .false.
          cycle
        else
          call Alchemy(polymer(i:i+1), is_blanked)
          if (is_blanked) cycle
        end if
      end do
      collapsed = Collapse(polymer)
      !collapsed = Collapse_smart(polymer)
      if (len(polymer) == len(collapsed)) then
        return
      else
        call move_alloc(collapsed, polymer)
      end if
    end do
  end function

  subroutine Problem05a()
    ! read string in
    ! conditions to delete: aA, Aa
    character(:), allocatable :: polymer, collapsed

    call ReadPolymer(polymer)
    collapsed = FullCollapse(polymer)
    print "(a,i0)", "Ergebnis: ", len(collapsed)
  end subroutine

  pure function RemoveLetter(polymer_in, i)
    character(*), intent(in)              :: polymer_in
    integer,      intent(in)              :: i
    character(:),             allocatable :: polymer, removeletter
    integer                               :: j

    polymer = polymer_in
    do concurrent (j = 1:len(polymer))
      if ( (polymer(j:j) == large(i:i)) .or. (polymer(j:j) == small(i:i)) ) then
        polymer(j:j) = " "
      end if
    end do
    removeletter = Collapse(polymer)
    !removeletter = Collapse_smart(polymer)
  end function

  function RemoveLetter_smart(polymer_in, i) result(removeletter)
    character(*), intent(in)              :: polymer_in
    integer,      intent(in)              :: i
    character(:),             allocatable :: removeletter
    character(1),             allocatable :: polymer(:), removeletter_array(:)

    polymer = StringToArray(polymer_in)
    removeletter_array = pack(polymer, ( (polymer/=large(i:i)) .and. (polymer /= small(i:i)) ))
    removeletter = ArrayToString(removeletter_array)
  end function

  subroutine Problem05b()
    character(:), allocatable :: polymer, polymer_trim, collapsed
    integer :: i, length(26)

    call ReadPolymer(polymer)
    do i = 1, 26
      print "(a,i0,a)", "i: ", i, "/26"
      polymer_trim = RemoveLetter(polymer, i)
      !polymer_trim = RemoveLetter_smart(polymer, i)
      collapsed = FullCollapse(polymer_trim)
      length(i) = len(collapsed)
    end do
    print "(a,i0)", "Ergebnis: ", minval(length)
  end subroutine
end module
