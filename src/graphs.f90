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

module Graphs
  use Trees

  implicit none

  type Edge
    ! "right" depends on "left"
    ! so on a directed node graph left points to right
    integer :: left, right
  contains
    procedure :: IsEqual => Edge_IsEqual
    generic   :: operator(==) => IsEqual
  end type

  public

contains

  logical pure function Edge_IsEqual(edge1, edge2) result(isequal)
    class(Edge), intent(in) :: edge1, edge2

    isequal = (edge1%left == edge2%left) .and. (edge1%right == edge2%right)
  end function

  function Graph_GetAllIDs(edges) result(ids)
    type(Edge), intent(in)              :: edges(:) ! array prevents type-bound
    integer                             :: i
    type(Tree),             pointer     :: id_tree => NULL() ! prevents PURE
    integer,                allocatable :: ids(:)

    do i = 1, size(edges)
      call Tree_Insert(id_tree, edges(i)%left)
      call Tree_Insert(id_tree, edges(i)%right)
    end do
    ids = Tree_InOrder(id_tree)
  end function

  subroutine Graph_GetStartIDs(edges, nodes, start_ids)
    ! process a list of edges between nodes
    ! return unsorted lists of:
    ! * nodes with no incoming edges
    ! * all other nodes
    type(Edge), intent(in)               :: edges(:)
    integer,    intent(in)               :: nodes(:)
    integer,    intent(out), allocatable :: start_ids(:)
    integer                              :: i

    if (allocated(start_ids)) then
      allocate(start_ids(0))
    end if
    ! a node is a valid start node if no nodes would point to it
    ! i.e. if no instances of edges(:) list it as the target, `%right`
    do i = 1, size(nodes) ! these are in lexicographic order
      if (any( edges%right == nodes(i) )) then
        ! cannot count as start node
        cycle
      else
        start_ids = [ start_ids, nodes(i) ]
      end if
    end do
  end subroutine

  subroutine Graph_TopologicalSort(edges_in, nodes, start_tree, sorted_ids, is_cyclic)
    ! use Khan's algorithm as described on Wikipedia
    ! https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
    ! using a sorted tree for S imposes the required lexicographic preference
    type(Edge), intent(in)               :: edges_in(:)
    integer,    intent(in)               :: nodes(:)
    type(Tree),              pointer     :: start_tree, sorted_tree => NULL()
    integer,    intent(out), allocatable :: sorted_ids(:)
    logical,    intent(out)              :: is_cyclic
    type(Edge),              allocatable :: edges(:)
    integer                              :: n, m, e
    type(Tree),              pointer     :: removed_node_indices => NULL()

    edges = edges_in
    ! S = Set of all nodes with no incoming edge
    ! while S is non-empty
    do while (Tree_CountNodes(start_tree) > 0)
      ! remove a node n from S
      call Tree_PopSmallest(start_tree, n)
      ! add node n to tail of result
      call Tree_Append(sorted_tree, n)
      ! for each node m with an edge e from node n to node m (from those still "in" edges)
      ! TODO: further simplify loop logic
      ! with an IMPURE ELEMENTAL Tree_Insert a one-liner may be possible
      next_m: do m = 1, size(nodes)
        ! skip any `m` which have already been removed from the graph
        if ( Tree_Contains(removed_node_indices, m) ) cycle next_m
        do e = 1, size(edges)
          if ( edges(e) == Edge(n,nodes(m)) ) then
            ! remove edge e from the graph
            edges = [edges(:e-1), edges(e+1:)]
            ! if node m has no other incoming edges
            if ( any(edges%right == nodes(m)) ) then
            else
              ! insert m into S
              call Tree_Insert(start_tree, nodes(m))
              ! keep track of all `m` which have been removed from the graph
              call Tree_Insert(removed_node_indices, m)
            end if
            cycle next_m
          end if
        end do
      end do next_m
      ! repeat until S is empty
    end do
    ! if graph has edges
    is_cyclic = (size(edges) > 0)
    ! flatten output
    allocate(sorted_ids(Tree_CountNodes(sorted_tree)))
    sorted_ids(:) = Tree_InOrder(sorted_tree)
  end subroutine

end module
