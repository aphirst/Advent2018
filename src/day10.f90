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

module Day10
  use, intrinsic :: iso_fortran_env

  implicit none

  real :: phi_inv = 2 / (sqrt(5.0) + 1)

  type Star
    integer :: x, y, dx, dy
  contains
    procedure :: Propagated     => Star_Propagated
    procedure :: SameTrajectory => Star_SameTrajectory
  end type

  type BBox
    integer :: x(2), y(2), width, height
  end type

  type Field
    type(Star), allocatable :: stars(:)
    integer                 :: num_stars
  contains
    procedure :: ReadStars   => Field_ReadStars
    procedure :: BoundingBox => Field_BoundingBox
    procedure :: Propagated  => Field_Propagated
    procedure :: Plot        => Field_Plot
    procedure :: Predict     => Field_Predict
  end type

  private
  public Problem10

contains

  type(Star) pure function Star_Propagated(this, t) result(mystar)
    class(Star), intent(in) :: this
    integer,     intent(in) :: t

    mystar%x = this%x + t * this%dx
    mystar%y = this%y + t * this%dy
    mystar%dx = this%dx
    mystar%dy = this%dy
  end function

  logical pure function Star_SameTrajectory(this, mystar) result(isc)
    class(Star), intent(in) :: this
    type(Star),  intent(in) :: mystar

    isc = (this%dx == mystar%dx) .and. (this%dy == mystar%dy )
  end function

  subroutine Field_ReadStars(this)
    class(Field), intent(out) :: this
    integer                   :: unit, iostat, num_stars, i
    character(50)             :: str

    call execute_command_line("rm input/day10_length.txt")
    call execute_command_line("expr `wc -l < input/day10.txt` > input/day10_length.txt")
    open(newunit=unit, file="input/day10_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) num_stars
    close(unit)
    allocate(this%stars(num_stars))

    open(newunit=unit, file="input/day10.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    do i = 1, num_stars
      read(unit, "(a)", iostat=iostat) str
      read(str(11:16), *) this%stars(i)%x
      read(str(19:24), *) this%stars(i)%y
      read(str(37:38), *) this%stars(i)%dx
      read(str(41:42), *) this%stars(i)%dy
    end do
    close(unit)

    this%num_stars = num_stars
  end subroutine

  type(BBox) pure function Field_BoundingBox(this) result(bb)
    class(Field), intent(in) :: this

    bb%x = [ minval(this%stars%x), maxval(this%stars%x) ]
    bb%y = [ minval(this%stars%y), maxval(this%stars%y) ]
    bb%width = bb%x(2) - bb%x(1) + 1
    bb%height = bb%y(2) - bb%y(1) + 1
  end function

  type(Field) pure function Field_Propagated(this, t) result(aftert)
    ! returns a new field propagated t seconds
    class(Field), intent(in) :: this
    integer,      intent(in) :: t
    integer                  :: i

    allocate(aftert%stars(this%num_stars))
    do i = 1, this%num_stars
      aftert%stars(i) = this%stars(i)%Propagated(t)
    end do
    aftert%num_stars = this%num_stars
  end function

  integer pure function Field_Predict(this) result(t)
    class(Field), intent(in) :: this
    integer                  :: i, j
    type(Star)               :: pi, pj

    if (this%num_stars < 2) error stop "Logikfehler: Mehrere Sterne benötigt um Schnittpunkte zu berechnen."
    ! find 2 stars which will intersect and compute their intersection time
    do i = 1, this%num_stars
      pi = this%stars(i)
      do j = i+1, this%num_stars
        pj = this%stars(j)
        if ( pi%SameTrajectory(pj) ) then
          cycle
        end if
        ! find intersection t
        ! (xi,yi) + t(dxi,dyi) = (xj,yj) + t(dxj,dyj)
        if (pi%dx /= pj%dx) then
          t = (pj%x - pi%x) / (pi%dx - pj%dx)
        else if (pi%dy /= pj%dy) then
          t = (pj%y - pi%y) / (pi%dy - pj%dy)
        else
          error stop "Logikfehler: Test `%SameTrajectory` gescheitert."
        end if
        return
      end do
    end do
    error stop "Logikfehler: Alle Sterne sind mit dem Ersten gleichgerichtet."
  end function

  subroutine Field_Plot(this, t)
    class(Field), intent(in)              :: this
    integer,      intent(in)              :: t
    type(Field)                           :: this_t
    character(:),             allocatable :: line(:)
    type(BBox)                            :: bb
    integer                               :: i, x, y

    this_t = this%Propagated(t)
    bb = this_t%BoundingBox()
    allocate( line(bb%y(1):bb%y(2)), source = repeat(" ",bb%width+1) )
    do i = 1, this%num_stars ! for each star
      y = this_t%stars(i)%y
      x = this_t%stars(i)%x - bb%x(1) + 1
      line(y)(x:x) = "#"
    end do
    do i = lbound(line,1), ubound(line,1)
      print *, line(i)
    end do
  end subroutine

  subroutine Problem10(c)
    integer,     intent(out) :: c(2)
    type(Field)              :: myfield
    integer                  :: tleft, t1, t2, tright, iter
    type(BBox)               :: bb1, bb2

    call myfield%ReadStars()

    ! Part 1: "What message will eventually appear in the sky?"

    ! if solution exists, stars must necessarily intersect each other within solution bounding box
    ! furthermore, solution bounding box must necessarily be inside any current bounding box
    ! => for 2 stars which intersect, solution must lie in the neighbourhood of their intersection t
    ! (stars with equal velocities will maintain constant separation)

    t1 = myfield%Predict()
    ! worst possible case is as follows:
    ! bounding box of solution is square of side N
    ! in solution, star on one side moving horizontaly, star on other side moving vertically
    ! both stars at minimum speed (1)
    ! intersection between trajectories either occurs in N steps or occurred N steps ago
    ! (depends on whether %x or %y branch was used for intersection estimate)
    bb1 = Field_BoundingBox(myfield%Propagated(t1))
    tleft = t1 - max(bb1%height, bb1%width)
    tright = t1 + max(bb1%height, bb1%width)
    ! in hindsight, solutions have bounding box height 9, so could hardcode +/- 9

    ! perform golden-section search find the true intersection time
    ! function must be "unimodal"
    t1 = F1(tleft, tright)
    t2 = F2(tleft, tright)
    iter = 0
    do while (t1 < t2)
      ! start t at golden ratio through [tleft, tright]
      bb1 = Field_BoundingBox(myfield%Propagated(t1))
      bb2 = Field_BoundingBox(myfield%Propagated(t2))
      if (bb1%width > bb2%width) then
        tleft = t1
        t1 = t2
        t2 = F1(t1, tright)
      else
        tright = t2
        t2 = t1
        t1 = F2(tleft, t2)
      end if
      iter = iter + 1
    end do
    print "(a)",  "Ergebnis 1: "
    call myfield%Plot(t1)
    print "(2a)", "Richtig:    ", "RPNNXFZR"
    call system_clock(c(1))

    ! Part 2: "Exactly how many seconds would they have needed to wait for that message to appear?"
    print "(a,i0)", "Ergebnis 2: ", t1
    print "(a,i0)", "Richtig:    ", 10946
    call system_clock(c(2))

  contains

    integer pure function F1(left, right)
      integer, intent(in) :: left, right
      F1 = right - nint( real(right-left)*phi_inv )
    end function

    integer pure function F2(left, right)
      integer, intent(in) :: left, right
      F2 = left + nint( real(right-left)*phi_inv )
    end function

  end subroutine

end module
