!> ## Heap Sort Algorithm
!>
!! This module implements the Heap Sort algorithm.
!!
!! Heap Sort is a comparison-based sorting algorithm that uses a binary heap data structure.
!! It first builds a max heap from the input data and then repeatedly extracts the maximum
!! element from the heap and reconstructs the heap until the array is sorted.
!!
!! Time Complexity: O(n log n) where n is the number of elements in the input array.
!!
!! Input:
!! - An array of integers.
!!
!! Output:
!! - A sorted array of integers.
module heap_sort_module
    implicit none

contains

    !> Subroutine to perform heap sort on an array
    subroutine heap_sort(array, n)
        implicit none
        integer, dimension(:), intent(inout) :: array   ! Input/output array to be sorted
        integer, intent(in) :: n                        ! Size of the array
        integer :: i

        ! Build the max heap
        do i = n / 2, 1, -1
            call heapify(array, n, i)
        end do

        ! Extract elements one by one from the heap
        do i = n, 2, -1
            ! Move the current root to the end
            call swap(array, 1, i)

            ! Call max heapify on the reduced heap
            call heapify(array, i - 1, 1)
        end do

    end subroutine heap_sort

    !> Subroutine to maintain the heap property
    recursive subroutine heapify(array, n, i)
        implicit none
        integer, dimension(:), intent(inout) :: array   ! Input/output array to be heapified
        integer, intent(in) :: n                        ! Size of the heap
        integer, intent(in) :: i                        ! Index of the root
        integer :: largest, left, right

        largest = i
        left = 2 * i
        right = 2 * i + 1

        ! Is Left Child is larger than Root?
        if (left <= n .and. array(left) > array(largest)) then
            largest = left
        end if

        ! Is Right Child larger than Largest so far?
        if (right <= n .and. array(right) > array(largest)) then
            largest = right
        end if

        ! Swap and heapify if Root is not the Largest
        if (largest /= i) then
            call swap(array, i, largest)
            call heapify(array, n, largest)
        end if

    end subroutine heapify

    !> Subroutine helper to swap two elements in an array
    subroutine swap(array, i, j)
        implicit none
        integer, dimension(:), intent(inout) :: array   ! Input/output array in which elements are swapped
        integer, intent(in) :: i, j                      ! Indices of the elements to be swapped
        integer :: temp

        temp = array(i)
        array(i) = array(j)
        array(j) = temp

    end subroutine swap

end module heap_sort_module