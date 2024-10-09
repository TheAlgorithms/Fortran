!> Test program for the Merge Sort algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #29
!!  https://github.com/TheAlgorithms/Fortran/pull/29
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program provides additional test cases to validate the merge_sort_module.

program tests_merge_sort

    use merge_sort_module
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
        array = (/4, 2, 7, 3, 1, 4, 9, 5, 10, 9, 2, 1/)
        expected = (/1, 1, 2, 2, 3, 4, 4, 5, 7, 9, 9, 10/)
        call run_test(array, expected, "Test 1: Array with repeated elements")
    end subroutine test_repeated_elements

    ! Test case 2: Already sorted array
    subroutine test_already_sorted()
        array = (/1, 2, 3, 4, 5, 6, 7, 8/)
        expected = (/1, 2, 3, 4, 5, 6, 7, 8/)
        call run_test(array, expected, "Test 2: Already sorted array")
    end subroutine test_already_sorted

    ! Test case 3: Reverse sorted array
    subroutine test_reverse_sorted()
        array = (/8, 7, 6, 5, 4, 3, 2, 1/)
        expected = (/1, 2, 3, 4, 5, 6, 7, 8/)
        call run_test(array, expected, "Test 3: Reverse sorted array")
    end subroutine test_reverse_sorted

    ! Test case 4: Array with all negative numbers
    subroutine test_negative_numbers()
        array = (/-11, -55, -43, -70, -2, -1, -15, -9/)
        expected = (/-70, -55, -43, -15, -11, -9, -2, -1/)
        call run_test(array, expected, "Test 4: Array with all negative numbers")
    end subroutine test_negative_numbers

    ! Test case 5: Single element array
    subroutine test_single_element()
        array = (/62/)
        expected = (/62/)
        call run_test(array, expected, "Test 5: Single element array")
    end subroutine test_single_element

    ! Test case 6: Array with identical elements
    subroutine test_identical_elements()
        array = (/4, 4, 4, 4, 4/)
        expected = (/4, 4, 4, 4, 4/)
        call run_test(array, expected, "Test 6: Array with identical elements")
    end subroutine test_identical_elements

    ! Test case 7: Array with alternating high and low values
    subroutine test_alternating_values()
        array = (/10, 2000, 20, 888, 30, 798/)
        expected = (/10, 20, 30, 798, 888, 2000/)
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

    !> Subroutine to run the merge sort test
    subroutine run_test(array, expected, test_name)
        integer, dimension(:), intent(inout) :: array
        integer, dimension(:), intent(in) :: expected
        character(len=*), intent(in) :: test_name
        integer :: n

        n = size(array)

        ! Call merge_sort in module
        call merge_sort(array, n)

        ! Assert that the sorted array matches the expected array
        if (all(array == expected)) then
            print *, test_name, " PASSED"
        else
            print *, test_name, " FAILED"
            print *, "Expected: ", expected
            print *, "Got: ", array
        end if

    end subroutine run_test

end program tests_merge_sort
