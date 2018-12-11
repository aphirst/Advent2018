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
    call execute_command_line("expr `wc -l < input/day07.txt` > input/day07_length.txt")
    open(newunit=unit, file="input/day07_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) num_edges
    close(unit)
    allocate(edges(num_edges))

    open(newunit=unit, file="input/day07.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    do i = 1, num_edges
      read(unit, "(a)", iostat=iostat) str
      edges(i)%left = iachar(str(6:6))
      edges(i)%right = iachar(str(37:37))
    end do
    close(unit)

    nodes = Graph_GetAllIDs(edges)
  end subroutine

  subroutine Problem07(c)
    integer                            :: c(2)
    type(Edge),            allocatable :: edges(:)
    integer,               allocatable :: nodes(:), sorted_ids(:)
    type(Graph)                        :: mygraph
    logical                            :: is_cyclic

    call ReadEdgesNodes(edges, nodes)
    ! Part 1: "In what order should the steps in your instructions be completed?"

    ! run Khan's algorithm
    call mygraph%Create(edges, nodes)
    allocate(sorted_ids(0))
    call mygraph%TopologicalSort(sorted_ids, is_cyclic)
    if (is_cyclic) error stop "Ergebener Baum enthält Schleifen."

    print "(*(a))", "Ergebnis 1: ", achar(sorted_ids)
    print "(2a)",   "Richtig:    ", "BGJCNLQUYIFMOEZTADKSPVXRHW"
    call system_clock(c(1))

    call system_clock(c(2))
  end subroutine

end module
