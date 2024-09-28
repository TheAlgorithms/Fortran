!> Merge Sort Algorithm
!!
!! This module implements the Merge Sort algorithm.
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #7
!!  https://github.com/TheAlgorithms/Fortran/pull/7
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! Merge Sort is a divide-and-conquer algorithm. It divides the input array into two halves, recursively sorts them,
!! and then merges the two sorted halves.
!!
!! Time Complexity: O(n log n) where n is the number of elements in the input array.
!!
!! Input:
!! - An array of integers.
!!
!! Output:
!! - A sorted array of integers.
!!
module merge_sort_module
    implicit none

contains

    !> Recursive subroutine to sort an array using merge sort
    recursive subroutine merge_sort(array, n)
        implicit none
        integer, dimension(:), intent(inout) :: array   ! Input/output array to be sorted
        integer, intent(in) :: n                        ! Size of the array
        integer :: middle
        integer, dimension(:), allocatable :: left_half, right_half, sorted_array

        ! Base case: return if the array has 1 or fewer elements
        if (n <= 1) return

        ! Calculate the middle point to split the array
        middle = n/2

        ! Allocate space for the two halves
        allocate (left_half(middle), right_half(n - middle), sorted_array(n))

        ! Split array into two halves
        left_half = array(1:middle)
        right_half = array(middle + 1:n)

        ! Recursively sort each half
        call merge_sort(left_half, middle)
        call merge_sort(right_half, n - middle)

        ! Merge the sorted halves
        call merge(left_half, middle, right_half, n - middle, sorted_array)

        ! Copy the sorted array back
        array = sorted_array

        ! Deallocate the temporary arrays
        deallocate (left_half, right_half, sorted_array)

    end subroutine merge_sort

    !> Subroutine to merge two sorted halves of an array
    subroutine merge(left_half, n_left, right_half, n_right, sorted_array)
        implicit none
        integer, dimension(:), intent(in) :: left_half, right_half    ! Input sorted halves
        integer, dimension(:), intent(out) :: sorted_array            ! Output sorted array
        integer, intent(in) :: n_left, n_right                        ! Sizes of the input halves
        integer :: i, j, k                                            ! Loop counters

        i = 1
        j = 1
        k = 1

        ! Merge the two halves
        do while (i <= n_left .and. j <= n_right)
            if (left_half(i) < right_half(j)) then
                sorted_array(k) = left_half(i)
                i = i + 1
            else
                sorted_array(k) = right_half(j)
                j = j + 1
            end if
            k = k + 1
        end do

        ! Copy remaining elements of left_half, if any
        do while (i <= n_left)
            sorted_array(k) = left_half(i)
            i = i + 1
            k = k + 1
        end do

        ! Copy remaining elements of right_half, if any
        do while (j <= n_right)
            sorted_array(k) = right_half(j)
            j = j + 1
            k = k + 1
        end do

    end subroutine merge

end module merge_sort_module
