!> Example program demonstrating Recursive Linear Search
!!
!! This program demonstrates the use of the `recursive_linear_search_module` to search for target values within arrays.

program recursive_linear_search_example
    use recursive_linear_search_module
    implicit none

    integer, dimension(5) :: array

    array = (/ 306, 1005, 5, 62, 0 /)

    !! Search for the number 62 in the array
    print*, "Target = 62: ", recursive_linear_search(array, size(array), 62) !! Prints 4.

    !! Search for the number 10 in the array
    print*, "Target = 10: ", recursive_linear_search(array, size(array), 10) !! Prints -1 because item 10 is not found.

end program recursive_linear_search_example
