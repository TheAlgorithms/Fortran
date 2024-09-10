!> Quick Sort Algorithm
!! This module implements the Quick Sort algorithm, a highly efficient
!! sorting technique that uses the divide-and-conquer strategy.
!!
!! Quick Sort works by selecting a pivot element and partitioning the
!! array into elements less than the pivot and elements greater than the pivot.
!!
!! Time Complexity: O(n log n) on average, though it can degrade to O(n^2)
!! in the worst case (depending on the pivot choice).
!!
!! Input:
!! - An array of integers.
!!
!! Output:
!! - A sorted array of integers.
!!
module quick_sort_module
    implicit none

contains

    !> Subroutine to sort an array using Quick Sort
    recursive subroutine quick_sort(array, low, high)
        implicit none
        integer, dimension(:), intent(inout) :: array   ! Input/output array to be sorted
        integer, intent(in) :: low, high                ! Indices of the array

        integer :: pivot_index

        if (low < high) then
            ! Partition the array and get the pivot index
            pivot_index = partition(array, low, high)

            ! Recursively sort elements before and after partition
            call quick_sort(array, low, pivot_index - 1)
            call quick_sort(array, pivot_index + 1, high)
        end if

    end subroutine quick_sort

    !> Subroutine to partition the array based on the pivot element
    function partition(array, low, high) result(pivot_index)
        implicit none
        integer, dimension(:), intent(inout) :: array   ! Input/output array to be partitioned
        integer, intent(in) :: low, high                ! Indices of the array
        integer :: pivot, pivot_index, i, j

        pivot = array(high)
        i = low - 1

        do j = low, high - 1
            if (array(j) <= pivot) then
                i = i + 1
                call swap(array, i, j)
            end if
        end do

        call swap(array, i + 1, high)
        pivot_index = i + 1

    end function partition

    !> Helper subroutine to swap two elements in the array
    subroutine swap(array, i, j)
        implicit none
        integer, dimension(:), intent(inout) :: array
        integer, intent(in) :: i, j
        integer :: temp

        temp = array(i)
        array(i) = array(j)
        array(j) = temp
    end subroutine swap

end module quick_sort_module
