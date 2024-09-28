!> Test program for the Merge Sort algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #7
!!  https://github.com/TheAlgorithms/Fortran/pull/7
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program demonstrates the use of the merge_sort_module by sorting an array of integers.

program test_merge_sort
    use merge_sort_module
    implicit none
    integer, dimension(8) :: array      ! Test array
    integer :: n, i

    ! Initialize the test array
    array = (/-2, 3, -10, 11, 99, 100000, 100, -200/)
    n = size(array)

    ! Call merge_sort from the module to sort the array
    call merge_sort(array, n)

    print *, "Sorted array:"
    do i = 1, n
        print *, array(i)
    end do
end program test_merge_sort
