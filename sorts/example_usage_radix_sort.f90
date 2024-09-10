!> Test program for the Radix Sort algorithm

!! This program demonstrates the use of the radix_sort_module by sorting an array of integers.

program test_radix_sort
    use radix_sort_module
    implicit none
    integer, dimension(10) :: array
    integer :: n, i
    integer, parameter :: base10 = 10, base2 = 2, base16 = 16

    ! Test for base 10
    print *, "Testing Radix Sort with base 10:"
    array = (/ 170, 45, 75, 90, 802, 24, 2, 66, 15, 40 /)
    n = size(array)
    call radix_sort(array, n, base10)
    print *, "Sorted array in base 10:"
    do i = 1, n
        print *, array(i)
    end do

    ! Test for base 2
    print *, "Testing Radix Sort with base 2:"
    array = (/ 10, 13, 9, 14, 2, 5, 15, 6, 8, 1 /)  ! Binary values as decimal
    n = size(array)
    call radix_sort(array, n, base2)
    print *, "Sorted binary array:"
    do i = 1, n
        print *, array(i)
    end do

    ! Test for base 16
    print *, "Testing Radix Sort with base 16:"
    array = (/ 171, 31, 61, 255, 16, 5, 211, 42, 180, 0 /)  ! Hexadecimal values as decimal
    n = size(array)
    call radix_sort(array, n, base16)
    print *, "Sorted hexadecimal array:"
    do i = 1, n
        print *, array(i)
    end do

end program test_radix_sort

