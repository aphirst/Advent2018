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
    procedure :: IsEqual      => Edge_IsEqual
    generic   :: operator(==) => IsEqual
  end type

  type List
    integer, allocatable :: keys(:)
  end type

  ! must be constructed such that nodes(:) are in ascending order
  ! address this%lists(:) via same indices as nodes(:)
  ! this%lists(i)%keys(:) contains actual node IDs
  type Graph
    integer                 :: N
    integer,    allocatable :: nodes(:)
    type(List), allocatable :: lists(:)
  contains
    procedure :: Create          => Graph_Create
    procedure :: AddNode         => Graph_AddNode
    procedure :: InDegrees       => Graph_InDegrees
    procedure :: TopologicalSort => Graph_TopologicalSort
    procedure :: WorkerSort      => Graph_WorkerSort
  end type

  ! each worker knows the node he's working on and the timestamp of its completion
  type Worker
    integer :: current_task, completion_time
  contains
    procedure :: SetTask => Worker_SetTask
  end type

  public

contains

  logical pure function Edge_IsEqual(edge1, edge2) result(isequal)
    class(Edge), intent(in) :: edge1, edge2

    isequal = (edge1%left == edge2%left) .and. (edge1%right == edge2%right)
  end function

  function Graph_GetAllIDs(edges) result(ids)
    ! get them then sorts them
    ! sorting should be a separate "tree sort", or binary sort
    type(Edge), intent(in)              :: edges(:) ! array prevents type-bound
    integer                             :: i
    type(Tree),             pointer     :: id_tree => NULL() ! prevents PURE
    integer,                allocatable :: ids(:)

    do i = 1, size(edges)
      call Tree_Insert(id_tree, edges(i)%left)
      call Tree_Insert(id_tree, edges(i)%right)
    end do
    ids = Tree_InOrder(id_tree)
    call Tree_Destroy(id_tree)
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

    allocate(start_ids(0))
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

  subroutine Graph_AddNode(mygraph, myedge)
    class(Graph), intent(in out)              :: mygraph
    type(Edge),   intent(in)                  :: myedge
    integer                                   :: i, left_index, right_index
    integer,                      allocatable :: left_indices(:), right_indices(:)

    ! find myedge%left in mygraph%nodes, assumes `mygraph` prepared in advance
    allocate(left_indices(0))
    left_indices = pack( [( i, i = 1, size(mygraph%nodes) )], mygraph%nodes == myedge%left )
    left_index = left_indices(1)
    ! do the same for myedge%right
    allocate(right_indices(0))
    right_indices = pack( [( i, i = 1, size(mygraph%nodes) )], mygraph%nodes == myedge%right )
    right_index = right_indices(1)

    ! assume no duplicates
    if ( count( mygraph%lists(left_index)%keys == right_index ) > 0 ) then
      error stop "Logikfehler: Knotenpunkt befindet sich bereits im Graphen."
    else
    ! if no, add it
      mygraph%lists(left_index)%keys = [ mygraph%lists(left_index)%keys, right_index ]
    end if
  end subroutine

  subroutine Graph_Create(this, edges, nodes)
    class(Graph), intent(out) :: this
    type(Edge),   intent(in)  :: edges(:)
    integer,      intent(in)  :: nodes(:)
    integer                   :: i, empty(0)

    this%N = size(nodes)
    this%nodes = nodes
    this%lists = [( List(empty), i = 1, size(nodes) )]
    do i = 1, size(edges)
      call this%AddNode(edges(i))
    end do
  end subroutine

  pure function Graph_InDegrees(this) result(in_degrees)
    class(Graph), intent(in) :: this
    integer                  :: i, j, in_degrees(this%N)

    in_degrees(:) = 0
    do i = 1, this%N
      ! in_degree(j) is how many vertexes point into this vertex
      do j = 1, size(in_degrees)
        if ( any(this%lists(i)%keys == j) ) then
          in_degrees(j) = in_degrees(j) + 1
        end if
      end do
    end do
  end function

  subroutine Graph_TopologicalSort(this, sorted_ids, is_cyclic)
    ! use "in-degree" method, assumes no duplicate edges
    ! https://www.geeksforgeeks.org/topological-sorting-indegree-based-solution/
    ! using a sorted tree for S imposes the required lexicographic preference
    class(Graph), intent(in)               :: this
    integer,      intent(out), allocatable :: sorted_ids(:)
    logical,      intent(out)              :: is_cyclic
    integer                                :: i, j, popped, cnt, in_degrees(this%N)
    type(Tree),                pointer     :: queued_nodes => NULL(), sorted_tree => NULL()

    ! traverse adjacency lists to fill in-degrees of edges
    in_degrees = this%InDegrees()

    ! enqueue all nodes with in-degree 0
    do i = 1, this%N
      if (in_degrees(i) == 0) call Tree_Insert(queued_nodes, i)
    end do

    cnt = 0
    do while (Tree_CountNodes(queued_nodes) > 0)
      ! pop smallest node from queue and append to final result
      call Tree_PopSmallest(queued_nodes, popped)
      call Tree_Append(sorted_tree, this%nodes(popped))
      ! decrement in-degrees of all nodes pointed to by the popped node
      do i = 1, size( this%lists(popped)%keys )
        j = this%lists(popped)%keys(i)
        in_degrees(j) = in_degrees(j) - 1
        ! enqueue all with zero in-degree (no other nodes point to them)
        if (in_degrees(j) == 0) call Tree_Insert(queued_nodes, j)
      end do
      cnt = cnt + 1
    end do

    ! graph is cyclic if some nodes never got queued
    is_cyclic = (cnt /= this%N)
    ! flatten output
    allocate(sorted_ids(Tree_CountNodes(sorted_tree)))
    sorted_ids(:) = Tree_InOrder(sorted_tree)
    call Tree_Destroy(queued_nodes)
    call Tree_Destroy(sorted_tree)
  end subroutine

  subroutine Worker_SetTask(this, popped, completion_time)
    class(Worker), intent(in out) :: this
    integer,       intent(in)     :: popped, completion_time

    this%current_task = popped
    this%completion_time = completion_time
  end subroutine

  subroutine Graph_WorkerSort(this, num_workers, WhenCompleted, sorted_ids, timestamp, is_cyclic)
    class(Graph), intent(in)               :: this
    integer,      intent(in)               :: num_workers
    integer,      intent(out), allocatable :: sorted_ids(:)
    integer,      intent(out)              :: timestamp
    logical,      intent(out)              :: is_cyclic
    integer                                :: i, j, k, cnt, popped, in_degrees(this%N)
    type(Worker)                           :: workers(num_workers)
    type(Tree),                pointer     :: queued_nodes => NULL(), sorted_tree => NULL()

    interface
      integer pure function WhenCompleted(timestamp, popped)
        integer, intent(in) :: timestamp, popped
      end function
    end interface

    ! traverse adjacency lists to fill in-degrees of edges
    in_degrees = this%InDegrees()

    ! enqueue all nodes with in-degree 0
    do i = 1, this%N
      if (in_degrees(i) == 0) call Tree_Insert(queued_nodes, i)
    end do

    timestamp = 0
    workers(:)%completion_time = 0
    cnt = 0

    do while ( (Tree_CountNodes(queued_nodes) > 0) .or. (count(workers%completion_time>timestamp)>0) )
      ! by "skipping" timestamps, every loop iteration should complete at least one task
      ! does NOT guarantee that every iteration adds another task to the queue
      if ( count(workers%completion_time <= timestamp) < 1 ) error stop "Logikfehler: Keine freien Arbeiter."
      do k = 1, num_workers
        ! pop as many tasks as possible and assign them to any free workers
        if (workers(k)%completion_time <= timestamp) then
          if (Tree_CountNodes(queued_nodes) > 0) then
            call Tree_PopSmallest(queued_nodes, popped)
            ! use closure on WhenCompleted() for a generic way to specify the delay function
            call workers(k)%SetTask(popped, WhenCompleted(timestamp, popped))
          end if
        end if
      end do

      ! skip to next busy-worker completion time
      if ( any(workers%completion_time >= timestamp) ) then
        timestamp = minval(workers%completion_time, 1, workers%completion_time > timestamp)
      end if

      ! from all workers who complete at this timestamp, gather their "ready" nodes
      do k = 1, num_workers
        if (workers(k)%completion_time == timestamp) then
          popped = workers(k)%current_task
          call Tree_Append(sorted_tree, this%nodes(popped))
          ! decrement in-degrees of all nodes pointed to by the finished node
          do i = 1, size( this%lists(popped)%keys )
            j = this%lists(popped)%keys(i)
            in_degrees(j) = in_degrees(j) - 1
            ! enqueue all with zero in-degree (no other nodes point to them)
            if (in_degrees(j) == 0) call Tree_Insert(queued_nodes, j)
          end do
          cnt = cnt + 1
        end if
      end do
    end do

    ! graph is cyclic if some nodes never got queued
    is_cyclic = (cnt /= this%N)
    ! flatten output
    allocate(sorted_ids(Tree_CountNodes(sorted_tree)))
    sorted_ids(:) = Tree_InOrder(sorted_tree)
    call Tree_Destroy(queued_nodes)
    call Tree_Destroy(sorted_tree)
  end subroutine

end module
