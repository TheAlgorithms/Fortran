!> Test program for the Radix Sort algorithm

!! This program provides additional test cases to validate the radix_sort_module.
!! The radix (base) parameter affects the internal digit processing for sorting, but the final output is always in decimal form.

program tests_radix_sort
    use radix_sort_module
    implicit none
    integer :: i
    integer, dimension(:), allocatable :: array
    integer, parameter :: base10 = 10, base2 = 2, base16 = 16

    ! Test 1: Base 10
    print *, "Test 1: Radix Sort with base 10"
    if (allocated(array)) deallocate (array)
    allocate (array(10))
    array = (/170, 45, 75, 90, 802, 24, 2, 66, 15, 40/)
    call run_test(array, base10)

    ! Test 2: Base 2
    print *, "Test 2: Radix Sort with base 2"
    if (allocated(array)) deallocate (array)
    allocate (array(10))
    array = (/10, 13, 9, 14, 2, 5, 15, 6, 8, 1/)  ! Binary values as decimal
    call run_test(array, base2)

    ! Test 3: Base 16
    print *, "Test 3: Radix Sort with base 16"
    if (allocated(array)) deallocate (array)
    allocate (array(10))
    array = (/171, 31, 61, 255, 16, 5, 211, 42, 180, 0/)  ! Hexadecimal values as decimal
    call run_test(array, base16)

    ! Test 4: Repeated elements
    print *, "Test 4: Array with repeated elements"
    if (allocated(array)) deallocate (array)
    allocate (array(12))
    array = (/5, 3, 8, 3, 1, 5, 7, 5, 10, 7, 3, 1/)
    call run_test(array, base10)

    ! Test 5: Already sorted array
    print *, "Test 5: Already sorted array"
    if (allocated(array)) deallocate (array)
    allocate (array(8))
    array = (/1, 2, 3, 4, 5, 6, 7, 8/)
    call run_test(array, base10)

    ! Test 6: Reverse sorted array
    print *, "Test 6: Reverse sorted array"
    if (allocated(array)) deallocate (array)
    allocate (array(8))
    array = (/8, 7, 6, 5, 4, 3, 2, 1/)
    call run_test(array, base10)

    ! Test 7: Array with all negative numbers (Note: Radix Sort only handles non-negative integers)
    print *, "Test 7: Array with all negative numbers (handled as base 10)"
    if (allocated(array)) deallocate (array)
    allocate (array(8))
    array = (/-1, -5, -3, -7, -2, -12, -15, -4/)
    call run_test(array, base10)

    ! Test 8: Single element array
    print *, "Test 8: Single element array"
    if (allocated(array)) deallocate (array)
    allocate (array(1))
    array = (/42/)
    call run_test(array, base10)

    ! Test 9: Array with identical elements
    print *, "Test 9: Array with identical elements"
    if (allocated(array)) deallocate (array)
    allocate (array(5))
    array = (/7, 7, 7, 7, 7/)
    call run_test(array, base10)

    ! Test 10: Array with alternating high and low values
    print *, "Test 10: Array with alternating high and low values"
    if (allocated(array)) deallocate (array)
    allocate (array(6))
    array = (/1, 1000, 2, 999, 3, 998/)
    call run_test(array, base10)

    ! Test 11: Empty array
    print *, "Test 11: Empty array"
    if (allocated(array)) deallocate (array)
    allocate (array(0))
    call run_test(array, base10)

contains

    !> Subroutine to run and print the radix sort test
    subroutine run_test(array, base)
        integer, dimension(:), intent(inout) :: array
        integer, intent(in) :: base
        integer :: n, i

        n = size(array)

        ! Print original array
        print *, "Original array:"
        do i = 1, n
            print *, array(i)
        end do

        ! Call radix_sort
        ! The parameters specify the array to sort, its size, and the base for sorting.
        call radix_sort(array, n, base)

        ! Print sorted array
        print *, "Sorted array:"
        do i = 1, n
            print *, array(i)
        end do

        print *, ""
    end subroutine run_test

end program tests_radix_sort
