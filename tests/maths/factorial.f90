!> Test program for factorial functions
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #31
!!  https://github.com/TheAlgorithms/Fortran/pull/31
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!!  This program provides test cases to validate the two facotrial functions in the factorial_module.

program tests_factorial
    use factorial_module
    implicit none

    integer :: result, expected

    ! Run test cases
    call test_factorial()
    call test_recursive_factorial()

    print *, "All tests completed."

contains

    ! Test case for iterative factorial for known values
    subroutine test_factorial()
        expected = 120
        result = factorial(5)
        call assert_test(result, expected, "Test 1: Iterative Factorial of 5")

        expected = 1
        result = factorial(0)
        call assert_test(result, expected, "Test 2: Iterative Factorial of edge case: 0")

        expected = 1
        result = factorial(1)
        call assert_test(result, expected, "Test 3: Iterative Factorial of edge case: 1")

        expected = 40320
        result = factorial(8)
        call assert_test(result, expected, "Test 4: Iterative Factorial of 8")

        expected = 720
        result = factorial(6)
        call assert_test(result, expected, "Test 5: Iterative Factorial of 6")
    end subroutine test_factorial

    ! Test case for recursive factorial for known values
    subroutine test_recursive_factorial()
        expected = 120
        result = recursive_factorial(5)
        call assert_test(result, expected, "Test 1: Recursive Factorial of 5")

        expected = 1
        result = recursive_factorial(0)
        call assert_test(result, expected, "Test 2: Recursive Factorial of edge case: 0")

        expected = 1
        result = recursive_factorial(1)
        call assert_test(result, expected, "Test 3: Recursive Factorial of edge case: 1")

        expected = 40320
        result = recursive_factorial(8)
        call assert_test(result, expected, "Test 4: Recursive Factorial of 8")

        expected = 720
        result = recursive_factorial(6)
        call assert_test(result, expected, "Test 5: Recursive Factorial of 6")
    end subroutine test_recursive_factorial

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

end program tests_factorial
