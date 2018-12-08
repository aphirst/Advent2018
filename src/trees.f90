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

  type TreeNode
    type(TreeNode), pointer :: left => NULL(), right => NULL()
    integer                 :: key
  end type

  public

contains

  subroutine Tree_InitNode(this, key)
    type(TreeNode),             pointer :: this
    integer,        intent(in)          :: key

    if (associated(this)) stop "Logikfehler: Baum ist bereits zugeordnet."
    allocate(this)
    this%left => NULL()
    this%right => NULL()
    this%key = key
  end subroutine

  recursive subroutine Tree_Destroy(this)
    type(TreeNode), pointer :: this

    if (associated(this)) then
      call Tree_Destroy(this%left)
      call Tree_Destroy(this%right)
    end if
  end subroutine

  integer recursive function Tree_CountNodes(this) result(total)
    type(TreeNode), pointer :: this

    if (associated(this)) then
      total = Tree_CountNodes(this%left) + 1 + Tree_CountNodes(this%right)
    else
      total = 0
    end if
  end function

  recursive function Tree_InOrder(this) result(inorder)
    type(TreeNode), pointer     :: this
    integer,        allocatable :: inorder(:)

    if (associated(this)) then
      inorder = [ Tree_InOrder(this%left), this%key, Tree_InOrder(this%right) ]
    else
      allocate(inorder(0))
    end if
  end function

  logical function Tree_HasNoChildren(this) result(hnc)
    class(TreeNode), pointer :: this

    hnc = .not. (associated(this%left) .or. associated(this%right))
  end function

  recursive subroutine Tree_InsertNode(this, key, is_duplicate)
    type(TreeNode),              pointer :: this
    integer,        intent(in)           :: key
    logical,        intent(out)          :: is_duplicate
    type(TreeNode),              pointer :: mynode => NULL()

    is_duplicate = .false.
    if (associated(this)) then
      if (key < this%key) then
        call Tree_InsertNode(this%left, key, is_duplicate)
      else if (key > this%key) then
        call Tree_InsertNode(this%right, key, is_duplicate)
      else
        ! TODO: work out where to insert duplicates
        is_duplicate = .true.
      end if
    else
      call Tree_InitNode(this, key)
    end if
  end subroutine

  recursive subroutine Tree_PopSmallest(this, key)
    type(TreeNode),              pointer :: this, temp
    integer,        intent(out)          :: key

    if (.not. associated(this)) stop "Logikfehler: Baum ist bereits leer."
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

end module
