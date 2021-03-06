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

  ! TODO: refactor Stack to be similar to Tree
  ! compute length on-the-fly
  ! recursive Stack_Destroy
  ! recursive Stack_InOrder
  ! recursive Stack_Clone without having to copy out entire contents

  type Node
    type(Node), pointer :: next => NULL()
    integer             :: key
  end type

  type Stack
    type(Node), pointer :: head => NULL()
    integer             :: num_nodes
  contains
    procedure :: IsEmpty
    procedure :: Init
    procedure :: Peek
    procedure :: Push, Pop
  end type

  private
  public :: Problem05

contains

  logical pure function IsEmpty(this)
    class(Stack), intent(in) :: this

    isempty = (this%num_nodes == 0)
  end function

  type(Stack) function NewStack()
    newstack = Stack(NULL(), 0)
  end function

  subroutine Init(this, key)
    ! make sure to only call when stack is not initialised
    class(Stack), intent(in out) :: this
    integer,      intent(in)     :: key

    allocate(this%head)
    this%head%key = key
    this%head%next => NULL()
    this%num_nodes = 1
  end subroutine

  subroutine Push(this, key)
    class(Stack), intent(in out)          :: this
    integer,      intent(in)              :: key
    type(Node),                   pointer :: mynode

    ! if stack empty
    if (this%IsEmpty()) then
      call this%Init(key)
    else
      ! use temporary node to "keep" the reference to the list's old head
      mynode => this%head
      ! clear the old head
      allocate(this%head)
      ! place the new value directly into the new head
      this%head%key = key
      ! stitch the list back together by point to the "kept" reference
      this%head%next => mynode
      this%num_nodes = this%num_nodes + 1
    end if
  end subroutine

  subroutine Pop(this, key)
    ! make sure to only call when stack is initialised
    class(Stack), intent(in out)           :: this
    integer,      intent(out)              :: key
    type(Node),                   pointer  :: mynode

    if (this%IsEmpty()) then
      error stop "Logikfehler: Stapel ist bereits leer."
    end if
    key = this%head%key
    mynode => this%head%next
    deallocate(this%head)
    this%head => mynode
    ! end state, this%head points to what used to be this%head%next
    this%num_nodes = this%num_nodes - 1
  end subroutine

  integer pure function Peek(this)
    class(Stack), intent(in) :: this

    peek = this%head%key
  end function

  function InOrder(mystack) result(int_array)
    type(Stack), intent(in)          :: mystack
    integer                          :: i, int_array(mystack%num_nodes)
    type(Node),              pointer :: mynode

    mynode => mystack%head
    do i = 1, mystack%num_nodes
      int_array(i) = mynode%key
      mynode => mynode%next
    end do
  end function

  pure recursive function ReadNodes(mynode) result(myread)
    type(Node), intent(in)              :: mynode
    integer,                allocatable :: myread(:)

    if (.not. associated(mynode%next)) then
      myread = mynode%key
    else
      myread = [ mynode%key, ReadNodes(mynode%next) ]
    end if
  end function

  function Clone(mystack)
    type(Stack), intent(in) :: mystack
    type(Stack)             :: clone
    integer                 :: keys(mystack%num_nodes), i

    keys = InOrder(mystack)
    clone = NewStack()
    do i = 1, size(keys)
      call Push(clone, keys(i))
    end do
  end function

  elemental impure subroutine Clear(mystack)
    type(Stack), intent(in out) :: mystack
    integer                     :: i

    do while (mystack%num_nodes > 0)
      call Pop(mystack, i)
    end do
  end subroutine

  subroutine ReadPolymer(polymer)
    type(Stack),  intent(out)              :: polymer
    integer                                :: unit, iostat, N, i
    character(:),              allocatable :: polymer_str

    call execute_command_line("rm input/day05_length.txt")
    call execute_command_line("expr `wc -m < input/day05.txt` - `wc -l < input/day05.txt` > input/day05_length.txt")
    open(newunit=unit, file="input/day05_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) N
    close(unit)
    allocate(character(len=N) :: polymer_str)

    open(newunit=unit, file="input/day05.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
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

  function FullCollapse(polymer) result(right)
    ! parse the string starting with everything in the left stack
    ! process things into the right stack and compare-as-we-go
    ! once the left stack is empty, we must have a fully-collapsed stack on the right
    type(Stack), intent(in) :: polymer
    type(Stack)             :: left, right
    integer                 :: i, j

    left = Clone(polymer)
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

    call Clear(left)
  end function

  function RemoveLetters(polymer, ascii)
    ! remove all instances of the specified ascii codes from the integer input
    type(Stack), intent(in)              :: polymer
    integer,     intent(in)              :: ascii(:)
    type(Stack)                          :: removeletters
    integer                              :: polymer_int(polymer%num_nodes), i
    logical                              :: mymask(polymer%num_nodes)
    integer,                 allocatable :: polymer_new(:)

    polymer_int = InOrder(polymer)
    do concurrent (i = 1:size(polymer_int))
      mymask(i) = all(polymer_int(i) /= ascii)
    end do
    polymer_new = pack(polymer_int, mymask)

    removeletters = NewStack()
    do i = 1, size(polymer_new)
      call Push(removeletters, polymer_new(i))
    end do
  end function

  subroutine Problem05(c)
    integer,     intent(out) :: c(3)
    ! TODO: account for the 0 offset via (i+1) in loop instead
    type(Stack)              :: polymer, collapsed, collapsed_new(0:25), polymer_trim(0:25)
    integer                  :: i, sizes(0:25)

    call ReadPolymer(polymer)
    call system_clock(c(1))

    ! Part 1: "How many units remain after fully reacting the polymer you scanned?"
    collapsed = FullCollapse(polymer)

    print "(a,i0)", "Ergebnis 1: ", collapsed%num_nodes
    print "(a,i0)", "Richtig:    ", 10598
    call system_clock(c(2))

    ! Part 2: "What is the length of the shortest polymer you can produce by
    ! removing all units of exactly one type and fully reacting the result?"
    do i = 0, 25
      polymer_trim(i) = RemoveLetters(collapsed, [iachar("A")+i, iachar("a")+i])
      collapsed_new(i) = FullCollapse(polymer_trim(i))
      sizes(i) = collapsed_new(i)%num_nodes
    end do

    print "(a,i0)", "Ergebnis 2: ", minval(sizes)
    print "(a,i0)", "Richtig:    ", 5312
    call system_clock(c(3))

    call Clear(polymer)
    call Clear(collapsed)
    call Clear(polymer_trim)
    call Clear(collapsed_new)
  end subroutine

end module
