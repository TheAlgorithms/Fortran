! Example Program: Function-based Ternary Search for Minimum and Maximum
! This program demonstrates how to use the function-based ternary search algorithm
! from the `ternary_search` module to find the minimum and maximum of unimodal functions.

program ternary_search_function_based
    use ternary_search
    implicit none

    ! Define the variables
    real(8) :: result_min, result_max       ! Results for minimum and maximum values
    real(8) :: min_point, max_point         ! Points where minimum and maximum occur
    real(8) :: left, right, tol             ! Left and right bounds, and tolerance

    interface
        ! Function with a minimum (example function - defined externally)
        real(8) function f_min(x)
            real(8), intent(in) :: x
        end function f_min

        ! Function with a maximum (example function - defined externally)
        real(8) function f_max(x)
            real(8), intent(in) :: x
        end function f_max
    end interface

    ! The boundary values can vary depending on the problem context.
    ! In this example, they are chosen arbitrarily.
    left = 0.0
    right = 10.0

    ! The tolerance value defines how close the left and right bounds must be for the search to terminate.
    tol = 1.0e-6

    ! Call the ternary search to find the minimum point of f_min
    min_point = ternary_search_minimum(f_min, left, right, tol)
    result_min = f_min(min_point)

    ! Call the ternary search to find the maximum point of f_max
    max_point = ternary_search_maximum(f_max, left, right, tol)
    result_max = f_max(max_point)

    print *, "Minimum of the function f_min is at x =", min_point, "with value =", result_min
    print *, "Maximum of the function f_max is at x =", max_point, "with value =", result_max

end program ternary_search_function_based

! Define the unimodal function f_min with a minimum near x = 5.0
! The quadratic term (x - 5.0)**2 defines a parabola that is concave upward with a minimum at x = 5.0
! and values increasing as x moves away from 5.
! The cosine term introduces oscillations, affecting the exact location of the minimum slightly away from 5.0.

real(8) function f_min(x)
    real(8), intent(in) :: x
    f_min = (x - 5.0)**2 + cos(x)      ! Example of a quadratic function with a cosine oscillation
end function f_min

! Define the unimodal function f_max with a maximum near x = 5.0
! The quadratic term -(x - 5.0)**2 defines a parabola that is concave downward with a maximum at x = 5.0
! and values decreasing as x moves away from 5.
! The cosine term introduces oscillations, affecting the exact location of the maximum slightly away from 5.0.

real(8) function f_max(x)
    real(8), intent(in) :: x
    f_max = -(x - 5.0)**2 + cos(x)     ! Example of a quadratic function with a cosine oscillation
end function f_max
