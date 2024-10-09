!> Test program for the Bubble Sort algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #29
!!  https://github.com/TheAlgorithms/Fortran/pull/29
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program tests the recursive_bubble_sort_module for correct sorting behavior.

program tests_recursive_bubble_sort
    use recursive_bubble_sort_module
    implicit none
    real, dimension(:), allocatable :: array, expected

    ! Run test cases
    call test_sorted_array()
    call test_reverse_sorted_array()
    call test_unsorted_array()
    call test_array_with_repeated_elements()
    call test_array_with_identical_elements()
    call test_single_element_array()
    call test_empty_array()

    print *, "All tests completed."

contains

    ! Test case 1: Already sorted array
    subroutine test_sorted_array()
        array = (/1.0, 2.0, 3.0, 4.0, 5.0/)
        expected = array
        call run_test(array, expected, "Test 1: Already sorted array")
    end subroutine test_sorted_array

    ! Test case 2: Reverse sorted array
    subroutine test_reverse_sorted_array()
        array = (/5.0, 4.0, 3.0, 2.0, 1.0/)
        expected = (/1.0, 2.0, 3.0, 4.0, 5.0/)
        call run_test(array, expected, "Test 2: Reverse sorted array")
    end subroutine test_reverse_sorted_array

    ! Test case 3: Unsorted array
    subroutine test_unsorted_array()
        array = (/3.5, 1.2, 4.8, 2.7, 5.0/)
        expected = (/1.2, 2.7, 3.5, 4.8, 5.0/)
        call run_test(array, expected, "Test 3: Unsorted array")
    end subroutine test_unsorted_array

    ! Test case 4: Array with repeated elements
    subroutine test_array_with_repeated_elements()
        array = (/3.0, 1.0, 2.0, 3.0, 4.0, 3.0/)
        expected = (/1.0, 2.0, 3.0, 3.0, 3.0, 4.0/)
        call run_test(array, expected, "Test 4: Array with repeated elements")
    end subroutine test_array_with_repeated_elements

    ! Test case 5: Array with identical elements
    subroutine test_array_with_identical_elements()
        array = (/7.0, 7.0, 7.0, 7.0, 7.0/)
        expected = array
        call run_test(array, expected, "Test 5: Array with identical elements")
    end subroutine test_array_with_identical_elements

    ! Test case 6: Single element array
    subroutine test_single_element_array()
        array = (/42.0/)
        expected = array
        call run_test(array, expected, "Test 6: Single element array")
    end subroutine test_single_element_array

    ! Test case 7: Empty array
    subroutine test_empty_array()
        if (allocated(array)) deallocate (array)
        if (allocated(expected)) deallocate (expected)
        allocate (array(0))
        allocate (expected(0))
        call run_test(array, expected, "Test 7: Empty array")
    end subroutine test_empty_array

    !> Subroutine to run the bubble sort test
    subroutine run_test(array, expected, test_name)
        real, dimension(:), intent(inout) :: array
        real, dimension(:), intent(in) :: expected
        character(len=*), intent(in) :: test_name
        real :: tolerance
        integer :: n

        n = size(array)

        ! Call bubble_sort in module
        call recursive_bubble_sort(array, n)

        ! Set an appropriate tolerance value
        tolerance = 1.0e-6

        ! Assert if the sorted values are sufficiently close to the expected array otherwise report failure
        if (all(abs(array - expected) < tolerance)) then
            print *, test_name, " PASSED"
        else
            print *, test_name, " FAILED"
            print *, "Expected: ", expected
            print *, "Got: ", array
            stop 1
        end if

    end subroutine run_test

end program tests_recursive_bubble_sort

