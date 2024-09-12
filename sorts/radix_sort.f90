!> Radix Sort Algorithm

!> This module implements the Radix Sort algorithm with configurable base.
!!
!! Radix Sort is a non-comparison-based sorting algorithm that sorts numbers by processing individual digits.
!! It is particularly efficient for sorting large lists of integers with a fixed number of digits.
!!
!! Time Complexity: O(d * (n + k)) where n is the number of elements, d is the number of digits, and k is the range of digits.
!!
!! Input:
!! - An array of non-negative integers.
!! - Base (radix) of the numbers to be sorted.
!!
!! Output:
!! - A sorted array of integers.
module radix_sort_module
    implicit none

contains

    !> Subroutine to perform Radix Sort on an array
    subroutine radix_sort(array, n, base)
        implicit none
        integer, dimension(:), intent(inout) :: array   ! Input/output array to be sorted
        integer, intent(in) :: n                        ! Size of the array
        integer, intent(in) :: base                     ! Base (radix) for sorting
        integer :: max_digit, exp

        ! Check if base is valid
        if (base < 2) then
            print *, "Error: Base must be greater than or equal to 2."
            return
        end if

        ! Find the maximum number to determine the number of digits
        max_digit = maxval(array)
        exp = 1

        ! Perform Counting Sort for each digit
        do while (max_digit / exp >= 1)
            call counting_sort(array, n, exp, base)
            exp = exp * base
        end do

    end subroutine radix_sort

    !> Subroutine to perform Counting Sort based on the digit represented by exp
    subroutine counting_sort(array, n, exp, base)
        implicit none
        integer, dimension(:), intent(inout) :: array    ! Input/output array to be sorted
        integer, intent(in) :: n                         ! Size of the array
        integer, intent(in) :: exp                       ! Exponent for digit place
        integer, intent(in) :: base                      ! Base (radix) for sorting
        integer :: i, count_size, digit
        integer, dimension(:), allocatable :: count, output

        count_size = base
        allocate(count(count_size), output(n))

        ! Initialize count array
        count = 0

        ! Store count of occurrences
        do i = 1, n
            digit = mod(array(i) / exp, base)
            count(digit + 1) = count(digit + 1) + 1
        end do

        ! Change count[i] so that count[i] contains the actual position of this digit in output[]
        do i = 2, count_size
            count(i) = count(i) + count(i - 1)
        end do

        ! Build the output array
        do i = n, 1, -1
            digit = mod(array(i) / exp, base)
            output(count(digit + 1)) = array(i)
            count(digit + 1) = count(digit + 1) - 1
        end do

        ! Copy the sorted elements into the original array
        array = output

        ! Deallocate temporary arrays
        deallocate(count, output)

    end subroutine counting_sort

end module radix_sort_module