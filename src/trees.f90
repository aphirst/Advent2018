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

module Trees
  implicit none

  type Tree
    type(Tree), pointer :: left => NULL(), right => NULL()
    integer             :: key
  end type

  public

contains

  subroutine Tree_Init(this, key)
    type(Tree),             pointer :: this
    integer,    intent(in)          :: key

    if (associated(this)) error stop "Logikfehler: Baum ist bereits zugeordnet."
    allocate(this)
    this%left => NULL()
    this%right => NULL()
    this%key = key
  end subroutine

  recursive subroutine Tree_Destroy(this)
    type(Tree), pointer :: this

    if (associated(this)) then
      call Tree_Destroy(this%left)
      call Tree_Destroy(this%right)
    end if
  end subroutine

  integer recursive function Tree_CountNodes(this) result(total)
    type(Tree), pointer :: this

    if (associated(this)) then
      total = Tree_CountNodes(this%left) + 1 + Tree_CountNodes(this%right)
    else
      total = 0
    end if
  end function

  recursive function Tree_InOrder(this) result(inorder)
    type(Tree), pointer     :: this
    integer,    allocatable :: inorder(:)

    if (associated(this)) then
      inorder = [ Tree_InOrder(this%left), this%key, Tree_InOrder(this%right) ]
    else
      allocate(inorder(0))
    end if
  end function

  logical function Tree_HasNoChildren(this) result(hnc)
    type(Tree), pointer :: this

    hnc = .not. (associated(this%left) .or. associated(this%right))
  end function

  logical recursive function Tree_Contains(this, key) result(tc)
    type(Tree),             pointer :: this
    integer,    intent(in)          :: key

    if (.not. associated(this)) then
      tc = .false.
    else
      if (key == this%key) then
        tc = .true.
      else if (key < this%key) then
        tc = Tree_Contains(this%left, key)
      else
        tc = Tree_Contains(this%right, key)
      end if
    end if
  end function

  recursive subroutine Tree_Insert(this, key, is_duplicate)
    type(Tree),              pointer             :: this
    integer,    intent(in)                       :: key
    logical,    intent(out),            optional :: is_duplicate

    if (present(is_duplicate)) is_duplicate = .false.
    if (associated(this)) then
      if (key < this%key) then
        call Tree_Insert(this%left, key, is_duplicate)
      else if (key > this%key) then
        call Tree_Insert(this%right, key, is_duplicate)
      else
        ! TODO: work out where to insert duplicates
        ! Should duplicates even be inserted?
        if (present(is_duplicate)) is_duplicate = .true.
      end if
    else
      call Tree_Init(this, key)
    end if
  end subroutine

  recursive subroutine Tree_CreateBalanced(this, sorted_keys)
    ! keys must be pre-sorted
    type(Tree),             pointer :: this
    integer,    intent(in)          :: sorted_keys(:)
    integer                         :: N, mid

    N = size(sorted_keys)
    select case (size(sorted_keys))
    case(0)
      return
    case(1)
      call Tree_Init(this, sorted_keys(1))
    case default
      mid = (1+N)/2
      call Tree_Init(this, sorted_keys(mid))
      call Tree_CreateBalanced(this%left, sorted_keys(:mid-1))
      call Tree_CreateBalanced(this%right, sorted_keys(mid+1:))
    end select
  end subroutine

  recursive subroutine Tree_PopSmallest(this, key)
    type(Tree),              pointer :: this, temp
    integer,    intent(out)          :: key

    if (.not. associated(this)) error stop "Logikfehler: Baum ist bereits leer."
    if (associated(this%left)) then
      call Tree_PopSmallest(this%left, key)
    else
      key = this%key
      if (associated(this%right)) then
        temp => this%right
        deallocate(this)
        this => temp
      else
        deallocate(this)
      end if
    end if
  end subroutine

  recursive subroutine Tree_Append(this, key)
    type(Tree),             pointer :: this
    integer,    intent(in)          :: key

    if (associated(this)) then
      call Tree_Append(this%right, key)
    else
      call Tree_Init(this, key)
    end if
  end subroutine

end module
