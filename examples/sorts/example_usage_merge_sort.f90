!> Test program for the Merge Sort algorithm

!! This program demonstrates the use of the merge_sort_module by sorting an array of integers.

program test_merge_sort
    use merge_sort_module
    implicit none
    integer, dimension(8) :: array      ! Test array
    integer :: n, i

    ! Initialize the test array
    array = (/ -2, 3, -10, 11, 99, 100000, 100, -200 /)
    n = size(array)

    ! Call merge_sort from the module to sort the array
    call merge_sort(array, n)

    print *, "Sorted array:"
    do i = 1, n
        print *, array(i)
    end do
end program test_merge_sort
