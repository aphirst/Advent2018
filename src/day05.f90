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

  type Node
    type(Node), pointer :: next => NULL()
    integer             :: key
  end type

  type Stack
    type(Node), pointer :: head => NULL()
    integer             :: num_nodes
  contains
    procedure :: IsEmpty
  end type

  private
  public :: Problem05a, Problem05b

contains

  logical pure function IsEmpty(mystack)
    class(Stack), intent(in) :: mystack

    isempty = (mystack%num_nodes == 0)
  end function

  type(Stack) function NewStack()
    newstack = Stack(NULL(), 0)
  end function

  subroutine Init(mystack, key)
    ! make sure to only call when stack is not initialised
    type(Stack), intent(in out) :: mystack
    integer,     intent(in)     :: key

    allocate(mystack%head)
    mystack%head%key = key
    mystack%head%next => NULL()
    mystack%num_nodes = 1
  end subroutine

  subroutine Push(mystack, key)
    type(Stack), intent(in out) :: mystack
    integer,     intent(in)     :: key
    type(Node),  pointer        :: mynode

    ! if stack empty
    if (mystack%IsEmpty()) then
      call Init(mystack, key)
    else
      mynode => mystack%head
      allocate(mystack%head)
      mystack%head%key = key
      mystack%head%next => mynode
      mystack%num_nodes = mystack%num_nodes + 1
    end if
  end subroutine

  subroutine Pop(mystack, key)
    ! make sure to only call when stack is initialised
    type(Stack), intent(in out)           :: mystack
    integer,     intent(out)              :: key
    type(Node),                  pointer  :: mynode

    if (mystack%IsEmpty()) then
      error stop "Stapel ist bereits leer. Logikfehler."
    end if
    key = mystack%head%key
    mynode => mystack%head%next
    deallocate(mystack%head)
    mystack%head => mynode
    ! end state, mystack%head points to what used to be mystack%head%next
    mystack%num_nodes = mystack%num_nodes - 1
  end subroutine

  integer pure function Peek(mystack)
    type(Stack), intent(in) :: mystack

    peek = mystack%head%key
  end function

  function Traverse(mystack)
    type(Stack), intent(in)          :: mystack
    integer                          :: i, traverse(mystack%num_nodes)
    type(Node),              pointer :: mynode

    mynode => mystack%head
    do i = 1, mystack%num_nodes
      traverse(i) = mynode%key
      mynode => mynode%next
    end do
  end function

  subroutine ReadPolymer(polymer)
    type(Stack),               intent(out) :: polymer
    integer                                :: unit, iostat, N, i
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

    polymer = NewStack()
    do i = 1, len(polymer_str)
     call Push(polymer, iachar(polymer_str(i:i)))
    end do
  end subroutine

  logical pure function SameLetterDifferentCase(pair) result(sldc)
    ! assumes all input either A-Z, a-z or (space)
    integer, intent(in)            :: pair(2)
    integer,             parameter :: ascii_offset = iachar("a") - iachar("A")

    sldc = (abs(pair(1)-pair(2)) == ascii_offset)
  end function

  function FullCollapse(polymer) result(collapsed)
    ! parse the string starting with everything in the left stack
    ! process things into the right stack and compare-as-we-go
    ! once the left stack is empty, we must have a fully-collapsed stack on the right
    type(Stack), intent(in) :: polymer
    type(Stack)             :: left, right, collapsed
    integer                 :: i, j

    left = polymer
    right = NewStack()

    do
      if (left%IsEmpty()) then
        exit
      else if (right%IsEmpty()) then
        call Pop(left, i)
        call Push(right, i)
      else
        i = Peek(left)
        j = Peek(right)
        if (SameLetterDifferentCase([i,j])) then
          call Pop(left, i)
          call Pop(right, j)
        else
          call Pop(left, i)
          call Push(right, i)
        end if
      end if
    end do
    collapsed = right
  end function

  subroutine Problem05a()
    type(Stack)          :: polymer, collapsed

    call ReadPolymer(polymer)
    collapsed = FullCollapse(polymer)
    print "(a,i0)", "Ergebnis: ", collapsed%num_nodes
  end subroutine

  function RemoveLetters(polymer, ascii)
    ! remove all instances of the specified ascii codes from the integer input
    type(Stack), intent(in) :: polymer
    integer,     intent(in) :: ascii(:)
    type(Stack)             :: removeletters
    integer                 :: polymer_int(polymer%num_nodes), i
    logical                 :: mymask(polymer%num_nodes)
    integer,    allocatable :: polymer_new(:)

    polymer_int = Traverse(polymer)
    do concurrent (i = 1:size(polymer_int))
      mymask(i) = all(polymer_int(i) /= ascii)
    end do
    polymer_new = pack(polymer_int, mymask)

    removeletters = NewStack()
    do i = 1, size(polymer_new)
      call Push(removeletters, polymer_new(i))
    end do
  end function

  subroutine Problem05b()
    type(Stack)          :: polymer, collapsed, collapsed_new, polymer_trim
    integer              :: i, sizes(25)

    call ReadPolymer(polymer)
    collapsed = FullCollapse(polymer)

    do i = 0, 25
      polymer_trim = RemoveLetters(collapsed, [iachar("A")+i, iachar("a")+i])
      collapsed_new = FullCollapse(polymer_trim)
      sizes(i) = collapsed_new%num_nodes
    end do
    print "(a,i0)", "Ergebnis: ", minval(sizes)
  end subroutine

end module
