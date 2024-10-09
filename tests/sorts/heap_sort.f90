!> Test program for the Heap Sort algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #8
!!  https://github.com/TheAlgorithms/Fortran/pull/8
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program provides additional test cases to validate the heap_sort_module.

program tests_heap_sort

    use heap_sort_module
    implicit none
    integer, dimension(:), allocatable :: array, expected

    ! Run test cases
    call test_repeated_elements()
    call test_already_sorted()
    call test_reverse_sorted()
    call test_negative_numbers()
    call test_single_element()
    call test_identical_elements()
    call test_alternating_values()
    call test_empty_array()

    print *, "All tests completed."

contains

    ! Test case 1: Array with repeated elements
    subroutine test_repeated_elements()
        array = (/5, 3, 8, 3, 1, 5, 7, 5, 10, 7, 3, 1/)
        expected = (/1, 1, 3, 3, 3, 5, 5, 5, 7, 7, 8, 10/)
        call run_test(array, expected, "Test 1: Array with repeated elements")
    end subroutine test_repeated_elements

    ! Test case 2: Already sorted array
    subroutine test_already_sorted()
        array = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10/)
        expected = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10/)
        call run_test(array, expected, "Test 2: Already sorted array")
    end subroutine test_already_sorted

    ! Test case 3: Reverse sorted array
    subroutine test_reverse_sorted()
        array = (/9, 8, 7, 6, 5, 4, 3, 2, 1/)
        expected = (/1, 2, 3, 4, 5, 6, 7, 8, 9/)
        call run_test(array, expected, "Test 3: Reverse sorted array")
    end subroutine test_reverse_sorted

    ! Test case 4: Array with all negative numbers
    subroutine test_negative_numbers()
        array = (/-110, -550, -430, -700, -20, -10, -150, -90, -250/)
        expected = (/-700, -550, -430, -250, -150, -110, -90, -20, -10/)
        call run_test(array, expected, "Test 4: Array with all negative numbers")
    end subroutine test_negative_numbers

    ! Test case 5: Single element array
    subroutine test_single_element()
        array = (/43/)
        expected = (/43/)
        call run_test(array, expected, "Test 5: Single element array")
    end subroutine test_single_element

    ! Test case 6: Array with identical elements
    subroutine test_identical_elements()
        array = (/7, 7, 7, 7, 7/)
        expected = (/7, 7, 7, 7, 7/)
        call run_test(array, expected, "Test 6: Array with identical elements")
    end subroutine test_identical_elements

    ! Test case 7: Array with alternating high and low values
    subroutine test_alternating_values()
        array = (/1, 1000, 2, 999, 3, 998/)
        expected = (/1, 2, 3, 998, 999, 1000/)
        call run_test(array, expected, "Test 7: Array with alternating high and low values")
    end subroutine test_alternating_values

    ! Test case 8: Empty array
    subroutine test_empty_array()
        if (allocated(array)) deallocate (array)
        if (allocated(expected)) deallocate (expected)
        allocate (array(0))
        allocate (expected(0))
        call run_test(array, expected, "Test 8: Empty array")
    end subroutine test_empty_array

    !> Subroutine to run the heap sort test
    subroutine run_test(array, expected, test_name)
        integer, dimension(:), intent(inout) :: array
        integer, dimension(:), intent(in) :: expected
        character(len=*), intent(in) :: test_name
        integer :: n

        n = size(array)

        ! Call heap_sort in module
        call heap_sort(array, n)

        ! Assert that the sorted array matches the expected array otherwise report failure for ctest
        if (all(array == expected)) then
            print *, test_name, " PASSED"
        else
            print *, test_name, " FAILED"
            print *, "Expected: ", expected
            print *, "Got: ", array
            stop 1
        end if

    end subroutine run_test

end program tests_heap_sort
