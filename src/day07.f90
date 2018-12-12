module Day07
  use Trees
  use Graphs

  implicit none

  integer, parameter :: num_workers = 5

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
      read(unit, "(a)") str
      edges(i)%left = iachar(str(6:6))
      edges(i)%right = iachar(str(37:37))
    end do
    close(unit)

    nodes = Graph_GetAllIDs(edges)
  end subroutine

  subroutine Problem07(c)
    integer,     intent(out)              :: c(3)
    type(Edge),               allocatable :: edges(:)
    integer,                  allocatable :: nodes(:), sorted_ids(:)
    type(Graph)                           :: mygraph
    logical                               :: is_cyclic
    integer                               :: timestamp

    call ReadEdgesNodes(edges, nodes)
    call system_clock(c(1))

    ! Part 1: "In what order should the steps in your instructions be completed?"
    call mygraph%Create(edges, nodes)

    ! run Khan's algorithm
    call mygraph%TopologicalSort(sorted_ids, is_cyclic)
    if (is_cyclic) error stop "Ergebener Baum enthält Schleifen."

    print "(*(a))", "Ergebnis 1: ", achar(sorted_ids)
    print "(2a)",   "Richtig:    ", "BGJCNLQUYIFMOEZTADKSPVXRHW"
    call system_clock(c(2))

    ! Part 2: "With 5 workers and the 60+ second step durations described
    ! above, how long will it take to complete all of the steps?"
    call mygraph%WorkerSort(num_workers, WhenCompleted, sorted_ids, timestamp, is_cyclic)
    print "(a,i0)", "Ergebnis 2: ", timestamp
    print "(a,i0)", "Richtig:    ", 1017
    call system_clock(c(3))

  contains
    integer pure function WhenCompleted(timestamp, popped)
      integer, intent(in) :: timestamp, popped

      whencompleted = timestamp + 60 + mygraph%nodes(popped) - iachar("A") + 1
    end function
  end subroutine

end module
