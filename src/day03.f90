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

  ! for larger fabric, trade space for time
  ! iterate over virtual fabric array, compare each cell to the list of claims

  private
  public :: Problem03

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
    type(Claim),   intent(out), allocatable :: claims(:)
    integer                                 :: unit, iostat, num_claims, i
    character(30)                           :: string

    call execute_command_line("rm input/day03_length.txt")
    call execute_command_line("expr `wc -l < input/day03.txt` > input/day03_length.txt")
    open(newunit=unit, file="input/day03_length.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) num_claims
    close(unit)
    allocate(claims(num_claims))

    open(newunit=unit, file="input/day03.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    do i = 1, num_claims
      read(unit, "(a30)") string
      claims(i) = ParseClaim(string)
    end do
    close(unit)
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

  subroutine FillFabric(fabric, claims)
    ! superimpose each claim onto an integer array representing the entire fabric
    ! each square counts how many claims placed on it
    ! count the number of squares with more than 1 claim
    integer,     intent(out), allocatable :: fabric(:,:)
    type(Claim), intent(in)               :: claims(:)
    integer                               :: i

    allocate(fabric(0:1000,0:1000))
    fabric(:,:) = 0

    do i = 1, size(claims)
      associate(x => claims(i)%x, y => claims(i)%y, width => claims(i)%width, height => claims(i)%height)
        fabric(x:x+width-1,y:y+height-1) = fabric(x:x+width-1,y:y+height-1) + 1
      end associate
    end do
  end subroutine

  subroutine Problem03(c)
    integer,     intent(out)              :: c(2)
    type(Claim),              allocatable :: claims(:)
    integer                               :: i, j
    integer,                  allocatable :: fabric(:,:)
    logical,                  allocatable :: disqualified(:)

    call ReadClaims(claims)

    ! Part 1: "How many square inches of fabric are within two or more claims?"
    call FillFabric(fabric, claims)

    print "(a,i0)", "Ergebnis 1: ", count(fabric > 1)
    print "(a,i0)", "Richtig:    ", 110827
    call system_clock(c(1))

    ! Part 2: "What is the ID of the only claim that doesn't overlap?"
    !print "(a,i0)", "Ergebnis 2: ", pack([(i, i = 1, size(claims))], .not. disqualified)
    allocate(disqualified(size(claims)))
    disqualified = [( .false., i = 1, size(claims) )]

    outer: do i = 1, size(claims)
      if (disqualified(i)) cycle outer
      inner: do j = 1, size(claims)
        if (i == j) then
          cycle inner
        else if (claims(i)%IsOverlap(claims(j))) then
          disqualified([i,j]) = .true.
          cycle
        end if
      end do inner
    end do outer

    print "(a,i0)", "Ergebnis 2: ", pack([(i, i = 1, size(claims))], .not. disqualified)
    print "(a,i0)", "Richtig:    ", 116
    call system_clock(c(2))
  end subroutine

end module
