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

module Day03
  implicit none

  type Claim
    integer :: x, y, width, height
  contains
    procedure :: x2, y2
    procedure :: IsOverlap
  end type

  private
  public :: Problem03a, Problem03b

contains

  pure function ParseClaim(string)
    ! each line is defined like `#62 @ 272,752: 28x14`
    ! index, (x,y), (width, height)
    ! there are 4 significant breaks, "@", "," ":" and "x""
    character(30), intent(in) :: string
    type(Claim)               :: parseclaim
    integer                   :: breaks(4)
    character(4)              :: x, y, width, height

    breaks(1) = scan(string, "@")
    breaks(2) = scan(string, ",")
    breaks(3) = scan(string, ":")
    breaks(4) = scan(string, "x")

    x = string(breaks(1)+1:breaks(2)-1)
    y = string(breaks(2)+1:breaks(3)-1)
    width = string(breaks(3)+1:breaks(4)-1)
    height = string(breaks(4)+1:len(string))

    read(x,*) parseclaim%x
    read(y,*) parseclaim%y
    read(width,*) parseclaim%width
    read(height,*) parseclaim%height
  end function

  subroutine ReadClaims(claims)
    type(Claim), intent(out), allocatable :: claims(:)
    integer                               :: unit, iostat
    character(30)                         :: string

    allocate(claims(0))

    open(newunit=unit, file="input/day03.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit, "(a30)", iostat=iostat) string
    do while (iostat == 0)
      claims = [claims, ParseClaim(string)]
      read(unit, "(a30)", iostat=iostat) string
    end do
    close(unit)
  end subroutine

  subroutine Problem03a()
    ! read a data structure comprising all the fabric claims
    ! superimpose each region onto a 1000x1000 integer array representing the entire fabric
    ! each square counts how many claims placed on it
    ! count the number of squares with more than 1 claim
    type(Claim), allocatable :: claims(:)
    integer                  :: i, fabric(0:1000,0:1000)

    call ReadClaims(claims)
    fabric = 0
    do i = 1, size(claims)
      associate(x => claims(i)%x, y => claims(i)%y, width => claims(i)%width, height => claims(i)%height)
        fabric(x:x+width-1,y:y+height-1) = fabric(x:x+width-1,y:y+height-1) + 1
      end associate
    end do
    print "(a,i0)", "Ergebnis: ", count(fabric > 1)
  end subroutine

  integer pure function x2(myclaim)
    class(Claim), intent(in) :: myclaim

    x2 = myclaim%x + myclaim%width
  end function

  integer pure function y2(myclaim)
    class(Claim), intent(in) :: myclaim

    y2 = myclaim%y + myclaim%height
  end function

  logical pure function IsOverlap(claim1, claim2)
    ! compare claims to see whether their regions overlap
    class(Claim), intent(in) :: claim1, claim2

    isoverlap = .false.
    if (claim1%x2() <= claim2%x) then
      return
    else if (claim1%x >= claim2%x2()) then
      return
    else if (claim1%y2() <= claim2%y) then
      return
    else if (claim1%y >= claim2%y2()) then
      return
    else
      isoverlap = .true.
    end if
  end function

  subroutine Problem03b()
    ! read the fabric claim data structure
    ! perform bounding-box tests
    type(Claim), allocatable :: claims(:)
    integer                  :: i, j
    logical,     allocatable :: disqualified(:)

    call ReadClaims(claims)
    allocate(disqualified(size(claims)))
    disqualified = [( .false., i = 1, size(claims) )]

    outer: do i = 1, size(claims)
      if (disqualified(i)) cycle outer
      inner: do j = 1, size(claims)
        if (i == j) cycle inner
        if (claims(i)%IsOverlap(claims(j))) then
          disqualified([i,j]) = .true.
        end if
      end do inner
    end do outer
    print "(a,i0)", "Ergebnis: ", pack([(i, i = 1, size(claims))], .not. disqualified)
  end subroutine
end module
