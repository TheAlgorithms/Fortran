!> Example program for the Heap Sort algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #8
!!  https://github.com/TheAlgorithms/Fortran/pull/8
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program demonstrates the use of the heap_sort_module by sorting an array of integers.

program test_heap_sort
    use heap_sort_module
    implicit none
    integer, parameter :: n = 12
    integer :: i
    integer, dimension(n) :: array(n)   ! Test array

    ! Initialize the test array
    array = (/12, 11, 13, 5, 6, 7, 3, 9, -1, 2, -12, 1/)

    ! Print the original array
    print *, "Original array:"
    do i = 1, n
        print *, array(i)
    end do

    ! Call heap_sort from the module to sort the array
    call heap_sort(array, n)

    ! Print the sorted array
    print *, "Sorted array:"
    do i = 1, n
        print *, array(i)
    end do

end program test_heap_sort
