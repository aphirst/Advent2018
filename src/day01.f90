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

module Day01
  use Trees, Node => TreeNode

  implicit none

  ! number of buckets, performance caps out around here
  integer, parameter :: N = 65536

  type Bucket
    type(Node), pointer :: head => NULL()
  end type

  private
  public :: Problem01

contains

  subroutine ReadDiffs(diffs)
    integer              :: unit, iostat
    integer              :: nextint
    integer, allocatable :: diffs(:)

    allocate(diffs(0))

    open(newunit=unit, file="input/day01.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit, *, iostat=iostat) nextint
    do while (iostat == 0)
      diffs = [ diffs, nextint ]
      read(unit, *, iostat=iostat) nextint
    end do
    close(unit)
  end subroutine

  subroutine Problem01(c)
    ! use a hash table of binary search trees
    integer,      intent(out)              :: c(2)
    integer                                :: total, i, hash
    integer,                   allocatable :: diffs(:)
    type(Bucket)                           :: mybuckets(N)
    logical                                :: is_duplicate

    call ReadDiffs(diffs)

    ! Part 1: "What is the resulting frequency after all of the changes in
    ! frequency have been applied?"
    print "(a,i0)", "Ergebnis 1: ", sum(diffs)
    call system_clock(c(1))

    ! Part 2: "What is the first frequency your device reaches twice?"
    total = 0
    is_duplicate = .false.

    outer: do
      inner: do i = 1, size(diffs)
        total = total + diffs(i)
        hash = modulo(total, N) + 1
        call Tree_InsertNode(mybuckets(hash)%head, total, is_duplicate)
        if (is_duplicate) exit outer
      end do inner
    end do outer

    print "(a,i0)", "Ergebnis 2: ", total
    call system_clock(c(2))

    do i = 1, N
      call Tree_Destroy(mybuckets(i)%head)
    end do
  end subroutine

end module
