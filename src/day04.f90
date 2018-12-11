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

  type Night
    integer :: guard_id, sleep
    logical :: is_asleep(0:59)
  end type

  private
  public :: Problem04

contains

  subroutine ParseNights(nights)
    ! every "day" has 1 guard on duty (may start before midnight, i.e. the night before)
    ! the only relevant time period on each day is 00:00 to 01:00, 60 minutes
    ! list of all days, the guard ID for each, and whether asleep or awake for each minute
    type(Night),   allocatable :: nights(:)
    integer                    :: unit, iostat, N, i, breaks(2), minute
    character(60)              :: string
    character(2)               :: minute_str
    character(4)               :: guard_str

    call execute_command_line("rm input/day04_nights.txt")
    call execute_command_line("fgrep -o '#' input/day04.txt | wc -l > input/day04_nights.txt")
    open(newunit=unit, file="input/day04_nights.txt", iostat=iostat, status="old")
    if (iostat /= 0) error stop "Datenfehler."
    read(unit,*) N
    close(unit)
    allocate(nights(N))

    call execute_command_line("rm input/day04_sorted.txt")
    call execute_command_line("cat input/day04.txt | sort > input/day04_sorted.txt")
    open(newunit=unit, file="input/day04_sorted.txt", iostat=iostat, status="old")
    read(unit, "(a)") string
    outer: do i = 1, N
      ! determine the guard number
      breaks(2) = scan(string, "#")
      guard_str = string(breaks(2)+1:)
      read(guard_str,*) nights(i)%guard_id
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
            error stop "Unerwartetes Ereignis."
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

  pure subroutine UniqueGuards(nights, guards)
    ! get the IDs of unique guards
    type(Night), intent(in)               :: nights(:)
    integer,     intent(out), allocatable :: guards(:)
    integer                               :: i

    allocate(guards(0))
    do i = 1, size(nights)
      if (any(nights(i)%guard_id == guards)) then
        cycle
      else
        guards = [ guards, nights(i)%guard_id ]
      end if
    end do
  end subroutine

  pure subroutine TotalSleepForGuard(nights, guard_id, sleep)
    type(Night), intent(in)  :: nights(:)
    integer,     intent(in)  :: guard_id
    integer,     intent(out) :: sleep
    integer                  :: i

    sleep = 0
    do i = 1, size(nights)
      if (guard_id == nights(i)%guard_id) then
        sleep = sleep + nights(i)%sleep
      end if
    end do
  end subroutine

  pure subroutine BestMinuteForGuard(nights, guard_id, bestminute, incidence)
    type(Night), intent(in)            :: nights(:)
    integer,     intent(in)            :: guard_id
    integer,     intent(out)           :: bestminute
    integer,     intent(out), optional :: incidence
    integer                            :: i, minutes(0:59)

    minutes = 0
    do i = 1, size(nights)
      if (nights(i)%guard_id == guard_id) then
        where (nights(i)%is_asleep)
          minutes = minutes + 1
        end where
      else
        cycle
      end if
    end do
    ! maxloc doesn't seem to honour the (0:59) array bounds
    bestminute = maxloc(minutes,1)-1
    if(present(incidence)) incidence = minutes(bestminute)
  end subroutine

  subroutine Problem04(c)
    integer,     intent(out)              :: c(2)
    type(Night),              allocatable :: nights(:)
    integer,                  allocatable :: guard_ids(:), sleep(:), guard_bestmins(:), bestincidences(:)
    integer                               :: i, bestguard, bestminute

    call ParseNights(nights)
    call UniqueGuards(nights, guard_ids)

    ! Part 1: "Find the guard that has the most minutes asleep. What minute
    ! does that guard spend asleep the most? What is the ID of the guard you
    ! chose multiplied by the minute you chose?"
    allocate(sleep(size(guard_ids)))
    do i = 1, size(guard_ids)
      call TotalSleepForGuard(nights, guard_ids(i), sleep(i))
    end do
    bestguard = guard_ids( maxloc(sleep,1) )
    call BestMinuteForGuard(nights, bestguard, bestminute)

    print "(a,i0)", "Ergebnis 1: ", bestguard * bestminute
    print "(a,i0)", "Richtig:    ", 146622
    call system_clock(c(1))

    ! Part 2: "Of all guards, which guard is most frequently asleep on the same
    ! minute? What is the ID of that multiplied by that minute?"
    allocate(guard_bestmins(size(guard_ids)))
    allocate(bestincidences(size(guard_ids)))
    do i = 1, size(guard_ids)
      call BestMinuteForGuard(nights, guard_ids(i), guard_bestmins(i), bestincidences(i))
    end do
    bestguard = guard_ids( maxloc(bestincidences,1) )
    bestminute = guard_bestmins( maxloc(bestincidences,1) )

    print "(a,i0)", "Ergebnis 2: ", bestguard * bestminute
    print "(a,i0)", "Richtig:    ", 31848
    call system_clock(c(2))
  end subroutine

end module
