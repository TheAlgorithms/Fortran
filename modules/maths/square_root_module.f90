!> Module containing functions related to square root calculation.
!!
!!  Created by: ITZ-NIHALPATEL
!!
!!  This module provides a function to calculate the square root of a number
!!  using Fortran's intrinsic SQRT function.
!!
module square_root_module
    implicit none
    private ! Default module entities to private

    public :: calculate_sqrt ! Make the function public

contains

    !> Calculates the square root of a non-negative number.
    !! Uses the intrinsic SQRT function.
    !!
    !! @param number The number (real) for which to calculate the square root.
    !!               Must be non-negative.
    !! @return The square root of the number (real). Returns -1.0 for negative input.
    function calculate_sqrt(number) result(sqrt_val)
        real, intent(in) :: number
        real :: sqrt_val

        if (number < 0.0) then
            print *, "Error: Input to calculate_sqrt must be non-negative."
            sqrt_val = -1.0 ! Indicate error for negative input
        else
            sqrt_val = sqrt(number) ! Use Fortran's intrinsic sqrt function
        end if
    end function calculate_sqrt

end module square_root_module
