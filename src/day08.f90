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

module Day08
  implicit none

  private
  public :: Problem08

contains

  subroutine ReadFlattenedTree(mytree)
    integer, intent(out), allocatable :: mytree(:)
    integer                           :: unit, iostat, num_entries

    call execute_command_line("rm input/day08_length.txt")
    call execute_command_line("expr `wc -w < input/day08.txt` > input/day08_length.txt")
    open(newunit=unit, file="input/day08_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) num_entries
    close(unit)
    allocate(mytree(num_entries))

    open(newunit=unit, file="input/day08.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) mytree
    close(unit)
  end subroutine

  ! The tree is made up of nodes; a single, outermost node forms the tree's root, and it
  ! contains all other nodes in the tree (or contains nodes that contain nodes, and so on).
  ! Specifically, a node consists of:
  ! - A header, which is always exactly two numbers:
  !   - The quantity of child nodes.
  !   - The quantity of metadata entries.
  ! - Zero or more child nodes (as specified in the header).
  ! - One or more metadata entries (as specified in the header).
  !
  ! Each child node is itself a node that has its own header, child nodes, and metadata.
  !
  ! The value of a node depends on whether it has child nodes.
  ! - If a node has no child nodes, its value is the sum of its metadata entries. So,
  !   the value of node B is 10+11+12=33, and the value of node D is 99.
  ! - However, if a node does have child nodes, the metadata entries become indexes
  !   which refer to those child nodes.

  recursive function ParseFlattenedTree(mytree, myval, tree_length) result(mysum)
    ! input is always of the form: [ num_nodes, num_metas, {nodes}, {metas}, junk ]
    ! the "junk" data are part of nodes to be processed in a different recursion branch
    integer, intent(in)               :: mytree(:)
    integer, intent(out)              :: myval, tree_length
    integer                           :: mysum, num_children, num_metas, subtree_length, i
    integer,              allocatable :: metas(:), vals(:)

    if (size(mytree) < 2) error stop "Logikfehler: Baumstruktur ungültig."
    num_children = mytree(1)
    num_metas = mytree(2)
    allocate(vals(num_children))
    allocate(metas(num_metas))
    tree_length = 2 ! header length
    mysum = 0
    ! recurse into each subtree, using their lengths to determine the start index of the next
    do i = 1, num_children ! 0 iterations when num_children = 0
      mysum = mysum + ParseFlattenedTree(mytree(tree_length+1:), vals(i), subtree_length)
      tree_length = tree_length + subtree_length
    end do
    metas = mytree(tree_length+1:tree_length+num_metas)
    if (num_metas /= size(metas)) error stop "Logikgehler: Anzahl der Metadaten stimmt nicht überein."
    mysum = mysum + sum(metas)
    ! process node values in accordance with the ruleset
    if (num_children == 0) then
      myval = sum(metas)
    else
      myval = sum( [( count(i == metas) * vals(i), i = 1, num_children )] )
    end if
    tree_length = tree_length + num_metas
  end function

  subroutine Problem08(c)
    integer, intent(out)              :: c(3)
    integer,              allocatable :: mytree(:)
    integer                           :: subtotal, myval, mylength

    call ReadFlattenedTree(mytree)
    call system_clock(c(1))

    ! Part 1: "What is the sum of all metadata entries?"
    subtotal = ParseFlattenedTree(mytree, myval, mylength) ! total tree length is unused
    print "(a,i0)", "Ergebnis 1: ", subtotal
    print "(a,i0)", "Richtig:    ", 37905
    call system_clock(c(2))

    ! Part 2: "What is the value of the root node?"
    print "(a,i0)", "Ergebnis 2: ", myval
    print "(a,i0)", "Richtig:    ", 33891
    call system_clock(c(3))
  end subroutine

end module
