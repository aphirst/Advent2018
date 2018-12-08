module Day07
  use Trees
  use Graphs

  implicit none

  private
  public Problem07

contains

  subroutine ReadEdgesNodes(edges, nodes)
    type(Edge),    intent(out), allocatable :: edges(:)
    integer,       intent(out), allocatable :: nodes(:)
    integer                                 :: unit, iostat, num_edges, i
    character(50)                           :: str

    call execute_command_line("rm input/day07_length.txt")
    call execute_command_line("expr `wc -m < input/day07.txt` - `wc -l < input/day07.txt` > input/day07_length.txt")
    open(newunit=unit, file="input/day07_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit,*) num_edges
    close(unit)
    allocate(edges(num_edges))

    open(newunit=unit, file="input/day07.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    do i = 1, num_edges
      read(unit, "(a)", iostat=iostat) str
      edges(i)%left = iachar(str(6:6))
      edges(i)%right = iachar(str(37:37))
    end do
    close(unit)

    nodes = Graph_GetAllIDs(edges)
  end subroutine

  subroutine Problem07(c)
    integer                 :: c(2)
    type(Edge), allocatable :: edges(:)
    integer,    allocatable :: nodes(:), start_ids(:), sorted_ids(:)
    type(Tree), pointer     :: start_tree => NULL()
    logical                 :: is_cyclic

    call ReadEdgesNodes(edges, nodes)

    ! find a list of start nodes with no incoming edges
    allocate(start_ids(0))
    call Graph_GetStartIDs(edges, nodes, start_ids)!, start_nodes, other_nodes)

    ! convert to trees
    call Tree_CreateBalanced(start_tree, start_ids)

    ! run Khan's algorithm, passing a (modifiable) tree containing the start ids
    allocate(sorted_ids(0))
    call Graph_TopologicalSort(edges, nodes, start_tree, sorted_ids, is_cyclic)

    print "(a,*(a))", "Ergebnis 1: ", achar(sorted_ids)
    print "(2a)", "Richtig:    ", "BGJCNLQUYIFMOEZTADKSPVXRHW"
    call system_clock(c(1))

    call system_clock(c(2))
  end subroutine
end module
