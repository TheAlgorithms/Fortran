!> Ternary Search Algorithm Module
!!
!! This module implements two types of ternary search algorithms: array-based and function-based.
!! The array-based ternary search is used to find a target element within a sorted array, while the function-based
!! approach is used to find the minimum or maximum of a unimodal function.
!!
!! Array-based ternary search:
!! - Given a sorted array and a target, it splits the array into three parts and recursively searches for the target.
!!
!! Function-based ternary search:
!! - Used for unimodal functions, which have a single peak (maximum) or valley (minimum).
!!   This method divides the function’s search space into thirds to converge on the minimum or maximum.
!!

module ternary_search
    implicit none
contains

    !> Array-based ternary search algorithm
    !! This recursive function searches for the target value in a sorted array.
    !!
    !! Input:
    !! - arr: The sorted array to search within.
    !! - target: The value to search for.
    !! - left, right: The range of indices within which to search.
    !!
    !! Output:
    !! - The index of the target element if found, otherwise -1.
    recursive integer function ternary_search_array(arr, target, left, right) result(result_index)
        implicit none
        integer, intent(in) :: arr(:)      ! Array to search within
        integer, intent(in) :: target      ! Target value to search for
        integer, intent(in) :: left, right ! Left and right indices
        integer :: mid1, mid2              ! Midpoints
        integer :: new_left, new_right     ! Temporary indices

        ! Base case: if the range is invalid, return -1 (not found)
        if (right < left) then
            result_index = -1
            return
        end if

        ! Divide array into three parts
        mid1 = left + (right - left) / 3
        mid2 = right - (right - left) / 3

        ! Check if the target is at mid1 or mid2
        if (arr(mid1) == target) then
            result_index = mid1
            return
        else if (arr(mid2) == target) then
            result_index = mid2
            return
        end if

        ! Recursive search in the appropriate third of the array
        if (target < arr(mid1)) then
            new_right = mid1 - 1
            result_index = ternary_search_array(arr, target, left, new_right)
        else if (target > arr(mid2)) then
            new_left = mid2 + 1
            result_index = ternary_search_array(arr, target, new_left, right)
        else
            new_left = mid1 + 1
            new_right = mid2 - 1
            result_index = ternary_search_array(arr, target, new_left, new_right)
        end if
    end function ternary_search_array

    !> Function-based ternary search to find the minimum of a unimodal function
    !! This function finds the minimum point of a unimodal function using the ternary search algorithm.
    !!
    !! Input:
    !! - f: The unimodal function to search.
    !! - left, right: The range within which to search for the minimum.
    !! - tol: The tolerance to determine convergence.
    !!
    !! Output:
    !! - The point at which the function achieves its minimum value.
    recursive real(8) function ternary_search_minimum(f, left, right, tol) result(minimum)
        implicit none
        interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
        end interface
        real(8), intent(in) :: left, right, tol
        real(8) :: mid1, mid2
        real(8) :: l, r

        l = left
        r = right

        ! Termination condition based on tolerance
        do while (r - l > tol)
            mid1 = l + (r - l) / 3.0
            mid2 = r - (r - l) / 3.0

            ! Compare function values at midpoints
            if (f(mid1) < f(mid2)) then
                r = mid2
            else
                l = mid1
            end if
        end do

        ! The minimum point is approximately at the midpoint
        minimum = (l + r) / 2.0
    end function ternary_search_minimum

    !> Function-based ternary search to find the maximum of a unimodal function
    !! This function finds the maximum point of a unimodal function using the ternary search algorithm.
    !!
    !! Input:
    !! - f: The unimodal function to search.
    !! - left, right: The range within which to search for the maximum.
    !! - tol: The tolerance to determine convergence.
    !!
    !! Output:
    !! - The point at which the function achieves its maximum value.
    recursive real(8) function ternary_search_maximum(f, left, right, tol) result(maximum)
        implicit none
        interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
        end interface
        real(8), intent(in) :: left, right, tol
        real(8) :: mid1, mid2
        real(8) :: l, r

        l = left
        r = right

        ! Termination condition based on tolerance
        do while (r - l > tol)
            mid1 = l + (r - l) / 3.0
            mid2 = r - (r - l) / 3.0

            ! Compare function values at midpoints
            if (f(mid1) > f(mid2)) then
                r = mid2
            else
                l = mid1
            end if
        end do

        ! The maximum point is approximately at the midpoint
        maximum = (l + r) / 2.0
    end function ternary_search_maximum

end module ternary_search
