!> Example program for the Quick Sort algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #10
!!  https://github.com/TheAlgorithms/Fortran/pull/10
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program demonstrates the use of the quick_sort_module by sorting an array of integers.

program test_quick_sort
    use quick_sort_module
    implicit none

    integer, dimension(10) :: array      ! Test array
    integer :: n, i

    ! Initialize the test array
    array = (/10, 7, 8, 9, 1, 5, -2, 12, 0, -5/)
    n = size(array)

    ! Print the original array
    print *, "Original array:"
    do i = 1, n
        print *, array(i)
    end do

    ! Call quick_sort from the module to sort the array
    call quick_sort(array, 1, n)        ! (1: low bound , n: high bound) of the array

    ! Print the sorted array
    print *, "Sorted array:"
    do i = 1, n
        print *, array(i)
    end do

end program test_quick_sort
