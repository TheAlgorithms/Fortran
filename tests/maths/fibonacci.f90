!> Test program for Fibonacci functions
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #31
!!  https://github.com/TheAlgorithms/Fortran/pull/31
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!!  This program provides test cases to validate the two fibonacci functions in the fibonacci_module.

program tests_fibonacci
    use fibonacci_module
    implicit none

    integer :: result, expected

    ! Run test cases
    call test_fib_zero()
    call test_fib_one()
    call test_fib_two()
    call test_fib_three()
    call test_fib_five()
    call test_fib_thirty()
    call test_fib_negative_one()
    call test_fib_negative_five()

    print *, "All tests completed."

contains

    ! Test case 1: Fibonacci of 0
    subroutine test_fib_zero()
        expected = 0
        result = fib_rec(0)
        call assert_test(result, expected, "Test 1: Fibonacci of 0 (Recursive)")
        result = fib_itr(0)
        call assert_test(result, expected, "Test 1: Fibonacci of 0 (Iterative)")
    end subroutine test_fib_zero

    ! Test case 2: Fibonacci of 1
    subroutine test_fib_one()
        expected = 1
        result = fib_rec(1)
        call assert_test(result, expected, "Test 2: Fibonacci of 1 (Recursive)")
        result = fib_itr(1)
        call assert_test(result, expected, "Test 2: Fibonacci of 1 (Iterative)")
    end subroutine test_fib_one

    ! Test case 3: Fibonacci of 2
    subroutine test_fib_two()
        expected = 1
        result = fib_rec(2)
        call assert_test(result, expected, "Test 3: Fibonacci of 2 (Recursive)")
        result = fib_itr(2)
        call assert_test(result, expected, "Test 3: Fibonacci of 2 (Iterative)")
    end subroutine test_fib_two

    ! Test case 4: Fibonacci of 3
    subroutine test_fib_three()
        expected = 2
        result = fib_rec(3)
        call assert_test(result, expected, "Test 4: Fibonacci of 3 (Recursive)")
        result = fib_itr(3)
        call assert_test(result, expected, "Test 4: Fibonacci of 3 (Iterative)")
    end subroutine test_fib_three

    ! Test case 5: Fibonacci of 5
    subroutine test_fib_five()
        expected = 5
        result = fib_rec(5)
        call assert_test(result, expected, "Test 5: Fibonacci of 5 (Recursive)")
        result = fib_itr(5)
        call assert_test(result, expected, "Test 5: Fibonacci of 5 (Iterative)")
    end subroutine test_fib_five

    ! Test case 6: Fibonacci of 30
    subroutine test_fib_thirty()
        expected = 832040
        result = fib_rec(30)
        call assert_test(result, expected, "Test 5: Fibonacci of 30 (Recursive)")
        result = fib_itr(30)
        call assert_test(result, expected, "Test 5: Fibonacci of 30 (Iterative)")
    end subroutine test_fib_thirty

    ! Test case 7: Fibonacci of negative input
    subroutine test_fib_negative_one()
        expected = -1
        result = fib_rec(-9)
        call assert_test(result, expected, "Test 6: Fibonacci of -1 (Recursive)")
        result = fib_itr(-9)
        call assert_test(result, expected, "Test 6: Fibonacci of -1 (Iterative)")
    end subroutine test_fib_negative_one

    ! Test case 8: Fibonacci of negative input
    subroutine test_fib_negative_five()
        expected = -1
        result = fib_rec(-9)
        call assert_test(result, expected, "Test 7: Fibonacci of -5 (Recursive)")
        result = fib_itr(-9)
        call assert_test(result, expected, "Test 7: Fibonacci of -5 (Iterative)")
    end subroutine test_fib_negative_five

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

end program tests_fibonacci

