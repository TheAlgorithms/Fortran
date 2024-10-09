!> Test program for the Linear Search algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #30
!!  https://github.com/TheAlgorithms/Fortran/pull/30
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!!  This program provides additional test cases to validate the linear_search_module.


program tests_linear_search
    use linear_search_module
    implicit none
    integer, dimension(:), allocatable :: array
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

    ! Test case 1: Target is found in the array
    subroutine test_found()
        array = (/30, 10, 20, 40, 55, 61, 72, 86, 97, 101/)
        target = 97
        expected = 9
        index = linear_search(array, target)
        call assert_test(index, expected, "Test 1: Target found in the array")
    end subroutine test_found

    ! Test case 2: Target is not found in the array
    subroutine test_not_found()
        array = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11/)
        target = 66
        expected = -1
        index = linear_search(array, target)
        call assert_test(index, expected, "Test 2: Target not found in the array")
    end subroutine test_not_found

    ! Test case 3: Target is the first element
    subroutine test_first_element()
        array = (/10, 20, 30, 40, 50/)
        target = array(1)
        expected = 1
        index = linear_search(array, target)
        call assert_test(index, expected, "Test 3: Target is the first element")
    end subroutine test_first_element

    ! Test case 4: Target is the last element
    subroutine test_last_element()
        array = (/10, 20, 30, 40, 50, 60, 70, 80/)
        target = array(size(array))
        expected = size(array)
        index = linear_search(array, target)
        call assert_test(index, expected, "Test 4: Target is the last element")
    end subroutine test_last_element

    ! Test case 5: Multiple occurrences of the target
    subroutine test_multiple_occurrences()
        array = (/1, 2, 3, 2, 4, 2, 5, 2, 4/)
        target = 4
        expected = 5
        index = linear_search(array, target)
        call assert_test(index, expected, "Test 5: Target has multiple occurrences (first found)")
    end subroutine test_multiple_occurrences

    ! Test case 6: Single element found
    subroutine test_single_element_found()
        array = (/42/)
        target = 42
        expected = 1
        index = linear_search(array, target)
        call assert_test(index, expected, "Test 6: Single element found")
    end subroutine test_single_element_found

    ! Test case 7: Single element not found
    subroutine test_single_element_not_found()
        array = (/42/)
        target = 99
        expected = -1
        index = linear_search(array, target)
        call assert_test(index, expected, "Test 7: Single element not found")
    end subroutine test_single_element_not_found

    ! Test case 8: Empty array
    subroutine test_empty_array()
        if (allocated(array)) deallocate (array)
        allocate (array(0))         ! Empty array
        target = 1
        expected = -1
        index = linear_search(array, target)
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

end program tests_linear_search
