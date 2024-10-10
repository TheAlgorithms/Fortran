!> Test program for the Array-Based Ternary Search algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #30
!!  https://github.com/TheAlgorithms/Fortran/pull/30
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!!  This program provides additional test cases to validate the array-based ternary_search module.

program tests_ternary_search_array
    use ternary_search
    implicit none
    integer, dimension(:), allocatable :: sorted_array
    integer :: target, index, expected

    ! Run test cases
    call test_found()
    call test_not_found()
    call test_first_element()
    call test_last_element()
    call test_multiple_occurrences()
    call test_single_element_found()
    call test_single_element_not_found()
    call test_empty_array()

    print *, "All tests completed."

contains

    ! Test case 1: Target found
    subroutine test_found()
        sorted_array = (/1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25/)
        target = 21
        expected = 11
        index = ternary_search_array(sorted_array, target, 1, size(sorted_array))
        call assert_test(index, expected, "Test 1: Target found in the array")
    end subroutine test_found

    ! Test case 2: Target not found
    subroutine test_not_found()
        sorted_array = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12/)
        target = 110
        expected = -1
        index = ternary_search_array(sorted_array, target, 1, size(sorted_array))
        call assert_test(index, -1, "Test 2: Target not found in the array")
    end subroutine test_not_found

    ! Test case 3: Target is the first element
    subroutine test_first_element()
        sorted_array = (/10, 20, 30, 40, 50, 60, 70, 80/)
        target = sorted_array(1)
        expected = 1
        index = ternary_search_array(sorted_array, target, 1, size(sorted_array))
        call assert_test(index, expected, "Test 3: Target is the first element")
    end subroutine test_first_element

    ! Test case 4: Target is the last element
    subroutine test_last_element()
        sorted_array = (/100, 200, 300, 400, 500, 600, 700, 800, 900/)
        target = sorted_array(size(sorted_array))
        expected = size(sorted_array)
        index = ternary_search_array(sorted_array, target, 1, size(sorted_array))
        call assert_test(index, expected, "Test 4: Target is the last element")
    end subroutine test_last_element

    ! Test case 5: Multiple occurrences of the target
    subroutine test_multiple_occurrences()
        sorted_array = (/1, 1, 2, 3, 4, 4, 5, 5, 6, 7, 8, 8, 9, 10, 11, 12, 12/)
        target = 12
        expected = 16
        index = ternary_search_array(sorted_array, target, 1, size(sorted_array))
        call assert_test(index, expected, "Test 5: Target has multiple occurrences (first found)")
    end subroutine test_multiple_occurrences

    ! Test case 6: Single element found
    subroutine test_single_element_found()
        sorted_array = (/59/)
        target = 59
        expected = 1
        index = ternary_search_array(sorted_array, target, 1, size(sorted_array))
        call assert_test(index, expected, "Test 6: Single element found")
    end subroutine test_single_element_found

    ! Test case 7: Single element not found
    subroutine test_single_element_not_found()
        sorted_array = (/42/)
        target = 99
        expected = -1
        index = ternary_search_array(sorted_array, target, 1, size(sorted_array))
        call assert_test(index, expected, "Test 7: Single element not found")
    end subroutine test_single_element_not_found

    ! Test case 8: Empty array
    subroutine test_empty_array()
        if (allocated(sorted_array)) deallocate (sorted_array)
        allocate (sorted_array(0))         ! Empty array
        target = 1
        expected = -1
        index = ternary_search_array(sorted_array, target, 1, size(sorted_array))
        call assert_test(index, expected, "Test 8: Search in an empty array")
    end subroutine test_empty_array

    !> Subroutine to assert the test results
    subroutine assert_test(actual, expected, test_name)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name

        if (actual == expected) then
            print *, test_name, " PASSED"
        else
            print *, test_name, " FAILED"
            print *, "Expected: ", expected
            print *, "Got: ", actual
            stop 1
        end if

    end subroutine assert_test

end program tests_ternary_search_array
