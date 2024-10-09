!> Test program for the GCD module
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #31
!!  https://github.com/TheAlgorithms/Fortran/pull/31
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!!  This program provides test cases to validate the GCD function in the gcd_module.

program tests_gcd_module
    use gcd_module
    implicit none
    integer :: result, expected

    ! Run test cases
    call test_gcd_positive_numbers()
    call test_gcd_with_zero()
    call test_gcd_negative_numbers()
    call test_gcd_same_numbers()
    call test_gcd_large_numbers()

    print *, "All tests completed."

contains

    ! Test case 1: GCD of two positive numbers
    subroutine test_gcd_positive_numbers()
        integer :: a, b
        a = 48
        b = 18
        expected = 6
        result = gcd(a, b)
        call assert_test(result, expected, "Test 1: GCD of two positive numbers")
    end subroutine test_gcd_positive_numbers

    ! Test case 2: GCD with one number as zero
    subroutine test_gcd_with_zero()
        integer :: a, b
        a = 0
        b = 5
        expected = 5
        result = gcd(a, b)
        call assert_test(result, expected, "Test 2: GCD with one number as zero")
    end subroutine test_gcd_with_zero

    ! Test case 3: GCD of two negative numbers
    subroutine test_gcd_negative_numbers()
        integer :: a, b
        a = -48
        b = -18
        expected = 6
        result = gcd(a, b)
        call assert_test(result, expected, "Test 3: GCD of two negative numbers")
    end subroutine test_gcd_negative_numbers

    ! Test case 4: GCD of the same number
    subroutine test_gcd_same_numbers()
        integer :: a, b
        a = 42
        b = 42
        expected = 42
        result = gcd(a, b)
        call assert_test(result, expected, "Test 4: GCD of the same number")
    end subroutine test_gcd_same_numbers

    ! Test case 5: GCD of large numbers
    subroutine test_gcd_large_numbers()
        integer :: a, b
        a = 123456
        b = 789012
        expected = 12
        result = gcd(a, b)
        call assert_test(result, expected, "Test 5: GCD of large numbers")
    end subroutine test_gcd_large_numbers

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

end program tests_gcd_module
