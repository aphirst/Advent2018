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

  pure function StringToASCII(mystring) result(myints)
    character(*), intent(in) :: mystring
    integer                  :: myints(len(mystring))
    integer                  :: i

    myints = [( iachar(mystring(i:i)), i = 1, len(mystring) )]
  end function

  subroutine ReadPolymer(polymer)
    integer,      allocatable, intent(out) :: polymer(:)
    integer                                :: unit, iostat, N
    character(:), allocatable              :: polymer_str

    call execute_command_line("rm input/day05_length.txt")
    call execute_command_line("expr `wc -m < input/day05.txt` - `wc -l < input/day05.txt` > input/day05_length.txt")
    open(newunit=unit, file="input/day05_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit,*) N
    close(unit)
    allocate(character(len=N) :: polymer_str)

    open(newunit=unit, file="input/day05.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit,"(a)") polymer_str
    close(unit)

    polymer = StringToASCII(polymer_str)
  end subroutine

  logical pure function SameLetterDifferentCase(pair) result(sldc)
    ! assumes all input either A-Z, a-z or (space)
    integer, intent(in)            :: pair(2)
    integer,             parameter :: ascii_offset = iachar("a") - iachar("A")

    sldc = (abs(pair(1)-pair(2)) == ascii_offset)
  end function

  pure function FullCollapse(polymer_in) result(collapsed)
    integer, intent(in)              :: polymer_in(:)
    integer,             allocatable :: polymer(:), collapsed(:)
    integer                          :: i

    allocate(polymer(size(polymer_in)))
    polymer(:) = polymer_in
    do
      ! convert dupes to junk (spaces)
      do i = 1, size(polymer)-1
        if ( polymer(i) == iachar(" ") ) then
          cycle
        else if ( SameLetterDifferentCase(polymer(i:i+1)) ) then
          polymer(i:i+1) = iachar(" ")
        end if
      end do
      ! collapse using a mask around the junk (spaces)
      collapsed = pack(polymer, polymer /= iachar(" "))
      if (size(collapsed) == size(polymer)) return
      call move_alloc(collapsed, polymer)
    end do
    deallocate(polymer)
  end function

  subroutine Problem05a_bad()
    integer, allocatable :: polymer(:), collapsed(:)

    call ReadPolymer(polymer)
    collapsed = FullCollapse(polymer)
    print "(a,i0)", "Ergebnis: ", size(collapsed)
  end subroutine

  pure function RemoveLetters(polymer, ascii)
    ! remove all instances of the specified ascii codes from the integer input
    integer, intent(in)              :: polymer(:), ascii(:)
    integer,             allocatable :: removeletters(:)
    logical                          :: mymask(size(polymer))
    integer                          :: i

    do i = 1, size(polymer)
      mymask(i) = all(polymer(i) /= ascii)
    end do
    removeletters = pack(polymer, mymask)
  end function

  subroutine Problem05b_bad()
    integer, allocatable :: polymer(:), polymer_trim(:), collapsed(:)
    integer              :: i, sizes(0:25)

    call ReadPolymer(polymer)
    polymer = FullCollapse(polymer)
    do i = 0, 25
      polymer_trim = RemoveLetters(polymer, [iachar("A")+i, iachar("a")+i])
      collapsed = FullCollapse(polymer_trim)
      sizes(i) = size(collapsed)
    end do
    print "(a,i0)", "Ergebnis: ", minval(sizes)
  end subroutine

end module
