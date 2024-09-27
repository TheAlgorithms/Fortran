!> Example Program for Recursive Bubble Sort
!!
!! This program demonstrates the use of the `recursive_bubble_sort_module` to sort an array using the recursive bubble sort algorithm.

program recursive_bubble_sort_example
    use recursive_bubble_sort_module
    implicit none

    real :: array(5)

    !! Fill the array with random numbers.
    call random_number(array)

    print *, "Before:", array

    !! Bubble sort subroutine call.
    call recursive_bubble_sort(array, size(array))

    print *, "After:", array

end program recursive_bubble_sort_example
