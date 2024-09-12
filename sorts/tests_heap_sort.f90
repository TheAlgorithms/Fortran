!> Test program for the Heap Sort algorithm

!! This program provides additional test cases to validate the heap_sort_module.

program tests_heap_sort

    use heap_sort_module
    implicit none
    integer :: i
    integer, dimension(:), allocatable :: array

    ! Test 1: Repeated elements
    print *, "Test 1: Array with repeated elements"
    array = (/ 5, 3, 8, 3, 1, 5, 7, 5, 10, 7, 3, 1 /)
    call run_test(array)

    ! Test 2: Already sorted array
    print *, "Test 2: Already sorted array"
    if (allocated(array)) deallocate(array)
    allocate(array(8))
    array = (/ 1, 2, 3, 4, 5, 6, 7, 8 /)
    call run_test(array)

    ! Test 3: Reverse sorted array
    print *, "Test 3: Reverse sorted array"
    if (allocated(array)) deallocate(array)
    allocate(array(8))
    array = (/ 8, 7, 6, 5, 4, 3, 2, 1 /)
    call run_test(array)

    ! Test 4: Array with all negative numbers
    print *, "Test 4: Array with all negative numbers"
    if (allocated(array)) deallocate(array)
    allocate(array(8))
    array = (/ -1, -5, -3, -7, -2, -12, -15, -4 /)
    call run_test(array)

    ! Test 5: Single element array
    print *, "Test 5: Single element array"
    if (allocated(array)) deallocate(array)
    allocate(array(1))
    array = (/ 42 /)
    call run_test(array)

    ! Test 6: Array with identical elements
    print *, "Test 6: Array with identical elements"
    if (allocated(array)) deallocate(array)
    allocate(array(5))
    array = (/ 7, 7, 7, 7, 7 /)
    call run_test(array)

    ! Test 7: Array with alternating high and low values
    print *, "Test 7: Array with alternating high and low values"
    if (allocated(array)) deallocate(array)
    allocate(array(6))
    array = (/ 1, 1000, 2, 999, 3, 998 /)
    call run_test(array)

    ! Test 8: Empty array
    print *, "Test 8: Empty array"
    if (allocated(array)) deallocate(array)
    allocate(array(0))
    call run_test(array)

contains

    !> Subroutine to run and print the heap sort test
    subroutine run_test(array)
        integer, dimension(:), intent(inout) :: array
        integer :: n, i

        n = size(array)

        ! Print original array
        print *, "Original array:"
        do i = 1, n
            print *, array(i)
        end do

        ! Call heap_sort
        call heap_sort(array, n)

        ! Print sorted array
        print *, "Sorted array:"
        do i = 1, n
            print *, array(i)
        end do

        print *, ""
    end subroutine run_test


end program tests_heap_sort