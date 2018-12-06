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

contains

  subroutine ReadCoords(mycoords)
    type(Coord), allocatable :: mycoords(:)
    integer                  :: unit, iostat, N, i

    call execute_command_line("rm input/day06_length.txt")
    call execute_command_line("expr `wc -l < input/day06.txt` > input/day06_length.txt")
    open(newunit=unit, file="input/day06_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit,*) N
    close(unit)
    allocate(mycoords(N))

    open(newunit=unit, file="input/day06.txt", iostat=iostat, status="old")
    do i = 1, N
      read(unit,*) mycoords(i)
    end do
    close(unit)
  end subroutine

  integer elemental function ManhattanDist(p1, p2, q1, q2) result (dist)
    integer, intent(in) :: p1, p2, q1, q2

    dist = abs(p1 - q1) + abs(p2 - q2)
  end function

  integer function BestCoord(coords, x, y)
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

  subroutine Problem06a()
    ! read all target coordinates
    ! determine size of virtual grid
    type(Coord), allocatable :: mycoords(:)
    integer                  :: i, xmin, xmax, ymin, ymax, x, y
    integer,     allocatable :: tally(:)
    logical,     allocatable :: is_still_qualified(:)

    call ReadCoords(mycoords)

    xmin = minval(mycoords%x,1)
    xmax = maxval(mycoords%x,1)
    ymin = minval(mycoords%y,1)
    ymax = maxval(mycoords%y,1)

    allocate(tally( size(mycoords) ))
    tally = [( 0, i = 1, size(mycoords) )]
    is_still_qualified = [( .true., i = 1, size(mycoords) )]
    ! process all points in the virtual grid
    do x = xmin, xmax
      do y = ymin, ymax
        i = BestCoord(mycoords, x, y)
        if (i /= 0) then
          tally(i) = tally(i) + 1
        else
          ! ignore grid points with duplicate nearest-coords
          cycle
        end if
        ! otherwise
        if ((x == xmin) .or. (x == xmax) .or. (y == ymin) .or. (y == ymax)) then
          ! disqualify target coordinate
          is_still_qualified(i) = .false.
        end if
      end do
    end do

    print "(a,i0)", "Ergebnis: ", maxval(tally(1:),1,is_still_qualified)
  end subroutine
end module
