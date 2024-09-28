!> Gnome Sort Algorithm
!!
!! This module implements the Gnome Sort algorithm.
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #09
!!  https://github.com/TheAlgorithms/Fortran/pull/9
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! Gnome Sort is a simple comparison-based sorting algorithm.
!! It iterates through the array, comparing and swapping elements if needed.
!!
!! Time Complexity: O(n^2) where n is the number of elements in the input array.
!!
!! Input:
!! - An array of integers.
!!
!! Output:
!! - A sorted array of integers.
!!
module gnome_sort_module
    implicit none

contains

    !> Subroutine to sort an array using Gnome Sort
    subroutine gnome_sort(array)
        implicit none
        integer, dimension(:), intent(inout) :: array   ! Input/output array to be sorted
        integer :: i, n

        n = size(array)
        i = 1

        ! Gnome Sort algorithm
        do while (i <= n)
            if (i == 1 .or. array(i) >= array(i - 1)) then
                i = i + 1
            else
                ! Swap elements
                call swap(array(i), array(i - 1))
                i = i - 1
            end if
        end do

    end subroutine gnome_sort

    !> Helper subroutine to swap two elements in an array
    subroutine swap(x, y)
        implicit none
        integer, intent(inout) :: x, y
        integer :: temp

        temp = x
        x = y
        y = temp

    end subroutine swap

end module gnome_sort_module
