!> Test program for the Merge Sort algorithm

!! This program provides additional test cases to validate the merge_sort_module.

program tests_merge_sort

    use merge_sort_module
    implicit none
    integer, dimension(:), allocatable :: array

    ! Test 1: Repeated elements
    print *, "Test 1: Array with repeated elements"
    array = (/4, 2, 7, 3, 1, 4, 9, 5, 10, 9, 2, 1/)
    call run_test(array)

    ! Test 2: Already sorted array
    print *, "Test 2: Already sorted array"
    if (allocated(array)) deallocate (array)
    allocate (array(8))
    array = (/1, 2, 3, 4, 5, 6, 7, 8/)
    call run_test(array)

    ! Test 3: Reverse sorted array
    print *, "Test 3: Reverse sorted array"
    if (allocated(array)) deallocate (array)
    allocate (array(8))
    array = (/8, 7, 6, 5, 4, 3, 2, 1/)
    call run_test(array)

    ! Test 4: Array with all negative numbers
    print *, "Test 4: Array with all negative numbers"
    if (allocated(array)) deallocate (array)
    allocate (array(8))
    array = (/-11, -55, -43, -70, -2, -1, -15, -9/)
    call run_test(array)

    ! Test 5: Single element array
    print *, "Test 5: Single element array"
    if (allocated(array)) deallocate (array)
    allocate (array(1))
    array = (/62/)
    call run_test(array)

    ! Test 6: Array with identical elements
    print *, "Test 6: Array with identical elements"
    if (allocated(array)) deallocate (array)
    allocate (array(5))
    array = (/4, 4, 4, 4, 4/)
    call run_test(array)

    ! Test 7: Array with alternating high and low values
    print *, "Test 7: Array with alternating high and low values"
    if (allocated(array)) deallocate (array)
    allocate (array(6))
    array = (/10, 2000, 20, 888, 30, 798/)
    call run_test(array)

    ! Test 8: Empty array
    print *, "Test 8: Empty array"
    if (allocated(array)) deallocate (array)
    allocate (array(0))
    call run_test(array)

contains

    !> Subroutine to run and print the merge sort test
    subroutine run_test(array)
        integer, dimension(:), intent(inout) :: array
        integer :: n, i

        n = size(array)

        ! Print original array
        print *, "Original array:"
        do i = 1, n
            print *, array(i)
        end do

        ! Call merge_sort
        call merge_sort(array, n)

        ! Print sorted array
        print *, "Sorted array:"
        do i = 1, n
            print *, array(i)
        end do

        print *, ""
    end subroutine run_test

end program tests_merge_sort
