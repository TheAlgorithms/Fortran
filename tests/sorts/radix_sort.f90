!> Test program for the Radix Sort algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #11
!!  https://github.com/TheAlgorithms/Fortran/pull/11
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program provides additional test cases to validate the radix_sort_module.
!! The radix (base) parameter affects the internal digit processing for sorting, but the final output is always in decimal form.

program tests_radix_sort
    use radix_sort_module
    implicit none
    integer, dimension(:), allocatable :: array, expected
    integer, parameter :: base10 = 10, base2 = 2, base16 = 16

    ! Run test cases
    call test_base10()
    call test_base2()
    call test_base16()
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

    ! Test 1: sort with Base 10
    subroutine test_base10()
        array = (/170, 45, 75, 90, 802, 24, 2, 66, 15, 40/)
        expected = (/2, 15, 24, 40, 45, 66, 75, 90, 170, 802/)
        call run_test(array, expected, base10, "Test 1: Base 10")
    end subroutine test_base10

    ! Test 2: Sort with Base 2
    subroutine test_base2()
        array = (/10, 13, 9, 14, 2, 5, 15, 6, 8, 1/)  ! Binary values as decimal
        expected = (/1, 2, 5, 6, 8, 9, 10, 13, 14, 15/)
        call run_test(array, expected, base2, "Test 2 Base 2")
    end subroutine test_base2

    ! Test 3: Sorth with Base 16
    subroutine test_base16()
        array = (/171, 31, 61, 255, 16, 5, 211, 42, 180, 0/)  ! Hexadecimal values as decimal
        expected = (/0, 5, 16, 31, 42, 61, 171, 180, 211, 255/)
        call run_test(array, expected, base16, "Test 3: Base 16")
    end subroutine test_base16

    ! Test case 4: Array with repeated elements
    subroutine test_repeated_elements()
        array = (/5, 3, 8, 3, 1, 5, 7, 5, 10, 7, 3, 1/)
        expected = (/1, 1, 3, 3, 3, 5, 5, 5, 7, 7, 8, 10/)
        call run_test(array, expected, base10, "Test 4: Array with repeated elements")
    end subroutine test_repeated_elements

    ! Test case 5: Already sorted array
    subroutine test_already_sorted()
        array = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13/)
        expected = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13/)
        call run_test(array, expected, base10, "Test 5: Already sorted array")
    end subroutine test_already_sorted

    ! Test 6: Reverse sorted array
    subroutine test_reverse_sorted()
        array = (/11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1/)
        expected = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11/)
        call run_test(array, expected, base10, "Test 6: Reverse sorted array")
    end subroutine test_reverse_sorted

    ! Test 7: Array with all negative numbers (Note: Radix Sort only handles non-negative integers)
    subroutine test_negative_numbers()
        array = (/-1, -5, -3, -7, -2, -12, -15, -4/)
        expected = (/-1, -5, -3, -7, -2, -12, -15, -4/)
        call run_test(array, expected, base10, "Test 7: Array with all negative numbers (handled as base 10)")
    end subroutine test_negative_numbers

    ! Test 8: Single element array
    subroutine test_single_element()
        array = (/93/)
        expected = (/93/)
        call run_test(array, expected, base10, "Test 8: Single element array")
    end subroutine test_single_element

    ! Test 9: Array with identical elements
    subroutine test_identical_elements()
        array = (/8, 8, 8, 8, 8/)
        expected = (/8, 8, 8, 8, 8/)
        call run_test(array, expected, base10, "Test 9: Array with identical elements")
    end subroutine test_identical_elements

    ! Test 10: Array with alternating high and low values
    subroutine test_alternating_values()
        array = (/1, 999, 2, 600, 3, 950/)
        expected = (/1, 2, 3, 600, 950, 999/)
        call run_test(array, expected, base10, "Test 10: Array with alternating high and low values")
    end subroutine test_alternating_values

    ! Test 11: Empty array
    subroutine test_empty_array()
        if (allocated(array)) deallocate (array)
        if (allocated(expected)) deallocate (expected)
        allocate (array(0))
        allocate (expected(0))
        call run_test(array, expected, base10, "Test 11: Empty array")
    end subroutine test_empty_array

    !> Subroutine to run the radix sort test
    subroutine run_test(array, expected, base, test_name)
        integer, dimension(:), intent(inout) :: array
        integer, dimension(:), intent(in) :: expected
        integer, intent(in) :: base
        character(len=*), intent(in) :: test_name
        integer :: n

        n = size(array)

        ! Call radix_sort in module
        ! The parameters specify the array to sort, its size, and the base for sorting.
        call radix_sort(array, n, base)

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

end program tests_radix_sort
