!> Example program for the Linear Search algorithm
!!
!! This program demonstrates the use of the the linear_search_module to search for targets within arrays.

program linear_search_program
    use linear_search_module
    implicit none

    integer, dimension(5) :: array

    array = (/540, 6, 10, 100, 3/)

    !! Search for the number 6 in array.
    print *, "Target = 6: ", linear_search(array, 6) !! Prints 2.

    !! Search for the number 5 in array.
    print *, "Target = 5: ", linear_search(array, 5) !! Prints -1 because item 5 is not found.

end program linear_search_program
