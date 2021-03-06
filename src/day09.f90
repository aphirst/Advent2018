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

module Day09
  use, intrinsic :: iso_fortran_env
  implicit none

  type Marble
    type(Marble), pointer :: prev => NULL(), next => NULL()
    integer               :: n
  end type

  integer, parameter :: num_players = 416, last_marble = 71975

  type Ring
    type(Marble),   pointer :: current => NULL()
    integer                 :: num_marbles, player
    integer(int64)          :: score(num_players)
  contains
    procedure :: Create     => Ring_Create
    procedure :: Rotate     => Ring_Rotate
    procedure :: Insert     => Ring_Insert
    procedure :: Remove     => Ring_Remove
    procedure :: GivePoints => Ring_GivePoints
    procedure :: Play       => Ring_Play
    procedure :: NextPlayer => Ring_NextPlayer
    procedure :: Destroy    => Ring_Destroy
  end type

  private
  public :: Problem09

contains

  subroutine Ring_Create(this)
    ! starts the marble circle game with marble 0
    ! currently draws from global variables
    class(Ring), intent(out) :: this

    allocate(this%current)
    this%current%n = 0
    this%current%next => this%current
    this%current%prev => this%current

    this%num_marbles = 1
    this%player = 1
    this%score = 0
  end subroutine

  subroutine Ring_Rotate(this, i)
    class(Ring), intent(in out) :: this
    integer,     intent(in)     :: i
    integer                     :: j

    if (i == 0) return
    if (i > 0) then ! cycle right
      do j = 1, i
        this%current => this%current%next
      end do
    else ! i < 0, cycle left
      do j = -1, i, -1
        this%current => this%current%prev
      end do
    end if
  end subroutine

  subroutine Ring_Insert(this, i)
    ! before [a b c],   current element "b", insert "i""
    ! after  [a b i c], current element "i"
    class(Ring),  intent(in out)          :: this
    integer,      intent(in)              :: i
    type(Marble),                 pointer :: new => NULL()

    allocate(new)
    new%n = i
    ! link new marble to its adjacent marbles
    new%next => this%current%next
    new%prev => this%current
    ! link old left marble to new marble
    new%next%prev => new
    new%prev%next => new
    ! new marble is now the current
    this%current => new
    this%num_marbles = this%num_marbles + 1
  end subroutine

  subroutine Ring_Remove(this)
    ! "The marble located immediately clockwise of the marble that was
    ! removed becomes the new current marble."
    ! before [a B c], current element "b", remove "b"
    ! after  [a C d], current element "c"
    class(Ring),  intent(in out)          :: this
    type(Marble),                 pointer :: temp1 => NULL(), temp2 => NULL()

    if (.not. associated(this%current)) error stop "Logikfehler: Keine Murmel zu entfernen."

    temp1 => this%current%prev
    temp2 => this%current%next

    temp1%next => temp2
    temp2%prev => temp1

    deallocate(this%current)
    this%current => temp2

    this%num_marbles = this%num_marbles - 1
  end subroutine

  subroutine Ring_GivePoints(this, points)
    ! give points to the current player
    class(Ring), intent(in out) :: this
    integer,     intent(in)     :: points

    this%score(this%player) = this%score(this%player) + points
  end subroutine

  subroutine Ring_Play(this, i)
    class(Ring), intent(in out) :: this
    integer,     intent(in)     :: i

    ! "Then, each Elf takes a turn placing the lowest-numbered remaining marble into
    ! the circle between the marbles that are 1 and 2 marbles clockwise of the current
    ! marble. The marble that was just placed then becomes the current marble."
    if (mod(i,23) /= 0) then
      call this%Rotate(1)
      call this%Insert(i)
    else
      ! "First, the current player keeps the marble they would have placed, adding
      ! it to their score. In addition, the marble 7 marbles counter-clockwise from
      ! the current marble is removed from the circle and also added to the current
      ! player's score. The marble located immediately clockwise of the marble that
      ! was removed becomes the new current marble."
      call this%Rotate(-7)
      call this%GivePoints(i+this%current%n)
      call this%Remove()
    end if
  end subroutine

  subroutine Ring_NextPlayer(this)
    class(Ring), intent(in out) :: this

    if (this%player < 0) error stop "Logikfehler: Zahl des aktuellen Spielers kleiner als Anzahl der Spieler."
    this%player = this%player + 1
    if (this%player > num_players) this%player = 1
  end subroutine

  subroutine Ring_Destroy(this)
    ! [... a b c ... ]
    class(Ring), intent(in out) :: this
    integer                     :: i

    do i = 1, this%num_marbles
      call this%Remove()
    end do
    this%num_marbles = 0
    this%player = 0
    this%score = 0
  end subroutine

  subroutine Problem09(c)
    integer,    intent(out) :: c(3)
    type(Ring)              :: game1, game2
    integer                 :: i

    ! no input file for this problem
    call system_clock(c(1))

    ! Part 1: "What is the winning Elf's score?"
    call game1%Create()
    do i = 1, last_marble
      call game1%Play(i)
      call game1%NextPlayer()
    end do

    print "(a,i0)", "Ergebnis 1: ", maxval(game1%score)
    print "(a,i0)", "Richtig:    ", 439341
    call system_clock(c(2))

    ! Part 2: "What would the new winning Elf's score be if the number of the last marble were 100 times larger?"
    call game2%Create()
    do i = 1, 100*last_marble
      call game2%Play(i)
      call game2%NextPlayer()
    end do

    print "(a,i0)", "Ergebnis 2: ", maxval(game2%score)
    print "(a,i0)", "Richtig:    ", 3566801385_int64
    call system_clock(c(3))

    call game1%Destroy()
    call game2%Destroy()
  end subroutine

end module
