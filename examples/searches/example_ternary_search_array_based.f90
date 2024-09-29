! Example Program: Array-based Ternary Search
! This program demonstrates how to use the array-based ternary search algorithm
! implemented in the `ternary_search` module to find a target element in a sorted array.

program example_ternary_search_array
    use ternary_search
    implicit none
    integer :: result           ! Holds the index of the found target
    integer, dimension(10) :: arr = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]     ! Sorted Test Array
    integer :: target           ! Target value to search for

    target = 17
    result = ternary_search_array(arr, target, 1, size(arr))

    if (result /= -1) then
        print *, "Target found at index:", result
    else
        print *, "Target not found."
    end if
end program example_ternary_search_array
