!> Test program for the Function-Based Ternary Search algorithm
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #30
!!  https://github.com/TheAlgorithms/Fortran/pull/30
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!!  This program provides test cases to validate the function-based ternary search algorithms for known functions.

program tests_ternary_search_function
    use ternary_search
    implicit none
    real(8) :: tol, left, right, result, expected

    tol = 1.0d-6
    left = -100.0d0
    right = 100.0d0

    ! Run test cases
    call test_find_min_parabola()
    call test_find_max_negative_parabola()
    call test_find_min_custom_function()

    print *, "All tests completed."

contains

    ! Test case 1: Find minimum of a parabola (f(x) = x^2)
    subroutine test_find_min_parabola()
        result = ternary_search_minimum(parabola, left, right, tol)
        expected = 0.0d0
        call assert_test(abs(result), expected, "Test 1: Find minimum of f(x) = x^2")
    end subroutine test_find_min_parabola

    ! Test case 2: Find maximum of a negative parabola (f(x) = -x^2)
    subroutine test_find_max_negative_parabola()
        result = ternary_search_maximum(negative_parabola, left, right, tol)
        expected = 0.0d0
        call assert_test(abs(result), expected, "Test 2: Find maximum of f(x) = -x^2")
    end subroutine test_find_max_negative_parabola

    ! Test case 3: Find minimum of a custom unimodal function
    subroutine test_find_min_custom_function()
        result = ternary_search_minimum(custom_unimodal_function, left, right, tol)
        expected = 50.0d0
        call assert_test(result, 50.0d0, "Test 3: Find minimum of custom unimodal function")
    end subroutine test_find_min_custom_function

    !> Subroutine to assert the test results
    subroutine assert_test(actual, expected, test_name)
        real(8), intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name

        if (abs(actual - expected) < tol) then
            print *, test_name, " PASSED"
        else
            print *, test_name, " FAILED"
            print *, "Expected: ", expected
            print *, "Got: ", actual
            stop 1
        end if

    end subroutine assert_test

    ! Parabola function: f(x) = x^2
    real(8) function parabola(x)
        real(8), intent(in) :: x
        parabola = x**2
    end function parabola

    ! Negative parabola function: f(x) = -x^2
    real(8) function negative_parabola(x)
        real(8), intent(in) :: x
        negative_parabola = -x**2
    end function negative_parabola

    ! Custom unimodal function: A function with a known minimum at x = 50
    real(8) function custom_unimodal_function(x)
        real(8), intent(in) :: x
        custom_unimodal_function = (x - 50.0d0)**2 + 100.0d0
    end function custom_unimodal_function

end program tests_ternary_search_function
