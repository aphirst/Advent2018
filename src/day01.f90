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

  type NodeInteger
    integer                        :: key
    type(NodeInteger), allocatable :: left, right
  contains
    procedure :: Create => NodeIntegerCreate
    procedure :: InsertUnique => NodeIntegerInsertUnique
    procedure :: PrintTree => NodeIntegerPrintTree
    procedure :: Destroy => NodeIntegerDestroy
  end type

  public

contains

  subroutine Problem01a()
    ! read file
    ! sum up integers
    ! spit out output
    integer :: unit, iostat
    integer :: total, nextint

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

  subroutine NodeIntegerCreate(this, key)
    class(NodeInteger)             :: this
    integer,            intent(in) :: key

    call this%Destroy()
    this%key = key
  end subroutine

  recursive subroutine NodeIntegerInsertUnique(this, key, is_duplicate)
    ! node insertion with no dupes
    class(NodeInteger), intent(in out) :: this
    integer,            intent(in)     :: key
    logical,            intent(out)    :: is_duplicate

    is_duplicate = .false.
    if (key == this%key) then
      is_duplicate = .true.
      return
    else if (key < this%key) then
      if (allocated(this%left)) then
        call this%left%InsertUnique(key, is_duplicate)
      else
        allocate(this%left)
        this%left%key = key
      end if
    else
      if (allocated(this%right)) then
        call this%right%InsertUnique(key, is_duplicate)
      else
        allocate(this%right)
        this%right%key = key
      end if
    end if
  end subroutine

  recursive subroutine NodeIntegerPrintTree(this)
    class(NodeInteger), intent(in) :: this

    if (allocated(this%left)) call this%left%PrintTree()
    print *, this%key
    if (allocated(this%right)) call this%right%PrintTree()
  end subroutine

  recursive subroutine NodeIntegerDestroy(this)
    class(NodeInteger) :: this

    if (allocated(this%left)) then
      call this%left%Destroy()
      deallocate(this%left)
    end if
    if (allocated(this%right)) then
      call this%right%Destroy()
      deallocate(this%right)
    end if
    this%key = 0
  end subroutine

  subroutine Problem01b()
    ! use a binary search tree
    integer                        :: unit, iostat
    integer                        :: nextint, total, i
    integer,           allocatable :: diffs(:)
    type(NodeInteger), allocatable :: mytree
    logical                        :: is_duplicate

    total = 0
    allocate(diffs(0))
    is_duplicate = .false.

    open(newunit=unit, file="day01.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit, *, iostat=iostat) nextint
    do while (iostat == 0)
      diffs = [ diffs, nextint ]
      read(unit, *, iostat=iostat) nextint
    end do
    close(unit)

    outer: do
      inner: do i = 1, size(diffs)
        total = total + diffs(i)
        if (.not. allocated(mytree)) then
          allocate(mytree)
          call mytree%Create(total)
        else
          call mytree%InsertUnique(total, is_duplicate)
          if (is_duplicate) exit outer
        end if
      end do inner
    end do outer
    print "(a,i0)", "Ergebnis: ", total
  end subroutine

end module
