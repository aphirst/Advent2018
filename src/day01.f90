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

  subroutine Problem01b_smart()
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

    open(newunit=unit, file="day01_smart.log", status="replace")
    do i = 1, size(totals)
      write(unit,*) totals(i)
    end do
    close(unit)
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

  subroutine Problem01b_smarter()
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
