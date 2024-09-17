!> Example program demonstrating Bubble Sort
!!
!! This program demonstrates the use of the `bubble_sort_module` to sort an array using the bubble sort algorithm.

program bubble_sort_example
    use bubble_sort_module
    implicit none

    real :: array(5)

    !! Fill the array with random numbers
    call random_number(array)

    print*, "Before:", array

    !! Call the bubble_sort subroutine to sort the array
    call bubble_sort(array)

    print*, "After:", array

end program bubble_sort_example
