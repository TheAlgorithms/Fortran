!> Test program for the square root function.
!!
!!  Created by: ITZ-NIHALPATEL
!!
!!  This program provides test cases to validate the square root function
!!  in the square_root_module.

program tests_square_root
    use square_root_module
    implicit none

    real :: result, expected
    real, parameter :: tolerance = 1e-6 ! Tolerance for floating-point comparison

    ! Run test cases
    call test_sqrt_zero()
    call test_sqrt_one()
    call test_sqrt_perfect_square()
    call test_sqrt_non_perfect_square()
    call test_sqrt_large_number()
    call test_sqrt_negative_input()

    print *, "All square root tests completed."

contains

    ! Test case 1: Square root of 0
    subroutine test_sqrt_zero()
        expected = 0.0
        result = calculate_sqrt(0.0)
        call assert_test(result, expected, "Test 1: Square root of 0", tolerance)
    end subroutine test_sqrt_zero

    ! Test case 2: Square root of 1
    subroutine test_sqrt_one()
        expected = 1.0
        result = calculate_sqrt(1.0)
        call assert_test(result, expected, "Test 2: Square root of 1", tolerance)
    end subroutine test_sqrt_one

    ! Test case 3: Square root of a perfect square (e.g., 16)
    subroutine test_sqrt_perfect_square()
        expected = 4.0
        result = calculate_sqrt(16.0)
        call assert_test(result, expected, "Test 3: Square root of 16", tolerance)
    end subroutine test_sqrt_perfect_square

    ! Test case 4: Square root of a non-perfect square (e.g., 2)
    subroutine test_sqrt_non_perfect_square()
        expected = 1.4142136
        result = calculate_sqrt(2.0)
        call assert_test(result, expected, "Test 4: Square root of 2", tolerance)
    end subroutine test_sqrt_non_perfect_square

    ! Test case 5: Square root of a larger number (e.g., 12345.0)
    subroutine test_sqrt_large_number()
        expected = 111.108055
        result = calculate_sqrt(12345.0)
        call assert_test(result, expected, "Test 5: Square root of 12345.0", tolerance)
    end subroutine test_sqrt_large_number

    ! Test case 6: Square root of a negative number
    subroutine test_sqrt_negative_input()
        expected = -1.0 ! Expected error indicator
        result = calculate_sqrt(-9.0)
        call assert_test(result, expected, "Test 6: Square root of -9 (Negative Input)", tolerance)
    end subroutine test_sqrt_negative_input

    !> Subroutine to assert the test results for real numbers
    !! Includes a tolerance for floating-point comparisons.
    subroutine assert_test(actual, expected, test_name, tol)
        real, intent(in) :: actual, expected, tol
        character(len=*), intent(in) :: test_name

        if (abs(actual - expected) <= tol) then
            print *, test_name, " PASSED"
        else
            print *, test_name, " FAILED"
            print *, "Expected: ", expected
            print *, "Got: ", actual
            stop 1 ! Stop execution on failure
        end if

    end subroutine assert_test

end program tests_square_root
