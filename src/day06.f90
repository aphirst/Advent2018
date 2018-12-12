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

module Day06
  implicit none

  type Coord
    integer :: x, y
  end type

  ! optimal solution using a k-d tree for storing the target coordinates
  ! makes "nearest target coordinate to a given test point" much faster

  private
  public :: Problem06

contains

  subroutine ReadCoords(mycoords)
    type(Coord), allocatable :: mycoords(:)
    integer                  :: unit, iostat, N, i

    call execute_command_line("rm input/day06_length.txt")
    call execute_command_line("expr `wc -l < input/day06.txt` > input/day06_length.txt")
    open(newunit=unit, file="input/day06_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) N
    close(unit)
    allocate(mycoords(N))

    open(newunit=unit, file="input/day06.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    do i = 1, N
      read(unit,*) mycoords(i)
    end do
    close(unit)
  end subroutine

  integer elemental function ManhattanDist(p1, p2, q1, q2) result (dist)
    integer, intent(in) :: p1, p2, q1, q2

    dist = abs(p1 - q1) + abs(p2 - q2)
  end function

  integer pure function BestCoord(coords, x, y)
    ! return the index in coords(:) of the nearest coord to the input (x,y) point
    ! returns 0 if more than one
    type(Coord), intent(in) :: coords(:)
    integer,     intent(in) :: x, y
    integer                 :: dist(size(coords))

    dist = ManhattanDist(coords%x, coords%y, x, y)
    bestcoord = minloc(dist,1)
    if ( any(dist(bestcoord+1:) == dist(bestcoord)) ) then
      bestcoord = 0
    end if
  end function

  pure function FiniteAreas(mycoords)
    ! loop over a virtual grid bounded by the target coordinates
    ! for coordinates whose regions pass off the edge (i.e. are infinite) return zero
    ! otherwise return the area (number of points) for each coordinate's nearest region
    type(Coord), intent(in) :: mycoords(:)
    integer                 :: finiteareas(size(mycoords)), xmin, xmax, ymin, ymax, i, x, y
    logical                 :: is_finite(size(mycoords))

    xmin = minval(mycoords%x,1)
    xmax = maxval(mycoords%x,1)
    ymin = minval(mycoords%y,1)
    ymax = maxval(mycoords%y,1)

    finiteareas = 0
    is_finite = .true.
    ! process all points in the virtual grid
    do x = xmin, xmax
      do y = ymin, ymax
        i = BestCoord(mycoords, x, y)
        if (.not. is_finite(i)) then
          cycle
        end if
        if ((x == xmin) .or. (x == xmax) .or. (y == ymin) .or. (y == ymax)) then
          ! disqualify target coordinate
          is_finite(i) = .false.
          finiteareas(i) = 0
          cycle
        end if
        if (i /= 0) then
          finiteareas(i) = finiteareas(i) + 1
        else
          ! ignore grid points with duplicate nearest-coords
          cycle
        end if
      end do
    end do
  end function

  integer pure function CountDistUnderN(mycoords, N) result(tally)
    type(Coord), intent(in) :: mycoords(:)
    integer,     intent(in) :: N
    integer                 :: xmin, xmax, ymin, ymax, x, y, i

    xmin = minval(mycoords%x,1)
    xmax = maxval(mycoords%x,1)
    ymin = minval(mycoords%y,1)
    ymax = maxval(mycoords%y,1)

    tally = 0
    do x = xmin, xmax
      do y = ymin, ymax
        i = sum(ManhattanDist(mycoords(:)%x, mycoords(:)%y, x, y))
        if (i < N) tally = tally + 1
      end do
    end do
  end function

  subroutine Problem06(c)
    integer,     intent(out)              :: c(3)
    type(Coord),              allocatable :: mycoords(:)

    call ReadCoords(mycoords)
    call system_clock(c(1))

    ! Part 1: "What is the size of the largest area that isn't infinite?"
    print "(a,i0)", "Ergebnis 1: ", maxval(FiniteAreas(mycoords),1)
    print "(a,i0)", "Richtig:    ", 4016
    call system_clock(c(2))

    ! Part 2: "What is the size of the region containing all locations which
    ! have a total distance to all given coordinates of less than 10000?"
    print "(a,i0)", "Ergebnis 2: ", CountDistUnderN(mycoords, N=10000)
    print "(a,i0)", "Richtig:    ", 46306
    call system_clock(c(3))
  end subroutine

end module
