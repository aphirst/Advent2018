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

module Day04
  implicit none

  type Event

  end type

  type Night
    integer :: guard, sleep
    logical :: is_asleep(0:59)
  end type

contains

  subroutine ParseNights(nights)
    type(Night),   allocatable :: nights(:)
    integer                    :: unit, iostat, N, i, breaks(2), minute
    character(60)              :: string
    character(2)               :: minute_str
    character(4)               :: guard_str

    call execute_command_line("rm day04_nights.txt")
    call execute_command_line("fgrep -o '#' day04.txt | wc -l > day04_nights.txt")
    open(newunit=unit, file="day04_nights.txt", iostat=iostat, status="old")
    if (iostat /= 0) stop "Datenfehler."
    read(unit,*) N
    close(unit)
    allocate(nights(N))

    call execute_command_line("rm day04_sorted.txt")
    call execute_command_line("cat day04.txt | sort > day04_sorted.txt")
    open(newunit=unit, file="day04_sorted.txt", iostat=iostat, status="old")
    read(unit, "(a)") string
    outer: do i = 1, N
      ! determine the guard number
      breaks(2) = scan(string, "#")
      guard_str = string(breaks(2)+1:)
      read(guard_str,*) nights(i)%guard
      nights(i)%is_asleep = .false.
      inner: do
        ! get next line
        read(unit, "(a)", iostat=iostat) string
        if (iostat /= 0) cycle outer
        ! if it's a new guard, cycle to the next night
        if (scan(string, "#") /= 0) then
          cycle outer
        else
          breaks(1) = scan(string, ":")
          minute_str = string(breaks(1)+1:)
          read(minute_str, *) minute
          if (index(string, "falls") /= 0) then
            ! sleep from here
            nights(i)%is_asleep(minute:) = .true.
          else if (index(string, "wakes") /= 0) then
            ! wake from here
            nights(i)%is_asleep(minute:) = .false.
          else
            stop "Unerwartetes Ereignis."
          end if
        end if
      end do inner
    end do outer

    do i = 1, N
      nights(i)%sleep = count(nights(i)%is_asleep)
    end do
  end subroutine

  integer pure function IntegerFind(list, item)
    integer, intent(in) :: list(:), item
    integer             :: i

    do i = 1, size(list)
      if (item == list(i)) then
        integerfind = i
        return
      end if
    end do
    integerfind = 0
  end function

  pure subroutine CondenseGuards(nights, guards_unique, sleep_condensed)
    ! get the IDs of unique guards
    type(Night), intent(in)               :: nights(:)
    integer,     intent(out), allocatable :: guards_unique(:), sleep_condensed(:)
    integer                               :: guards_all(size(nights)), sleep_all(size(nights)), i, j

    guards_all = nights%guard
    sleep_all = nights%sleep
    allocate(guards_unique(0))
    allocate(sleep_condensed(0))
    do i = 1, size(guards_all)
      if (any(guards_all(i) == guards_unique)) then
        !j = findloc(guards_unique, guards_all(i))
        j = IntegerFind(guards_unique, guards_all(i))
        sleep_condensed(j) = sleep_condensed(j) + sleep_all(i)
        cycle
      else
        guards_unique = [ guards_unique, guards_all(i) ]
        sleep_condensed = [ sleep_condensed, sleep_all(i) ]
      end if
    end do
  end subroutine

  subroutine CondenseNights(nights, bestguard, bestminute)
    type(Night), intent(in)  :: nights(:)
    integer,     intent(in)  :: bestguard
    integer,     intent(out) :: bestminute
    integer                  :: i, j, minutes(0:59)

    minutes = 0
    do i = 1, size(nights)
      if (nights(i)%guard == bestguard) then
        do concurrent (j = 0:59)
          if (nights(i)%is_asleep(j)) minutes(j) = minutes(j) + 1
        end do
      else
        cycle
      end if
    end do
    ! maxloc doesn't seem to honour the (0:59) array bounds
    bestminute = maxloc(minutes,1)-1
  end subroutine

  subroutine Problem04a()
    ! every "day" has 1 guard on duty (may start before midnight, i.e. the night before)
    ! the only relevant time period on each day is 00:00 to 01:00, 60 minutes
    ! list of all days, the guard ID for each, and whether asleep or awake for each minute
    type(Night), allocatable :: nights(:)
    integer,     allocatable :: guards(:), sleep(:)
    integer                  :: bestguard, bestminute

    call ParseNights(nights)
    call CondenseGuards(nights, guards, sleep)
    bestguard = guards( maxloc(sleep,1) )
    call CondenseNights(nights, bestguard, bestminute)
    print *, "Ergebnis: ", bestguard*bestminute
  end subroutine

end module
