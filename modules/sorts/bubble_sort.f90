!> Bubble Sort Module
!!
!! This module contains a subroutine for sorting a collection using the bubble sort algorithm.

module bubble_sort_module
    implicit none

contains

    !! This subroutine sorts the collection using bubble sort.
    subroutine bubble_sort(collection)
        real, dimension(:), intent(inout) :: collection !! A collection of real numbers to be sorted

        integer :: i, j, collection_size
        real :: temp
        logical :: swapped

        !! Determine the size of the collection
        collection_size = size(collection)

        !! Perform bubble sort
        do j = collection_size - 1, 1, -1

            swapped = .false.

            do i = 1, j
                if (collection(i) .gt. collection(i + 1)) then
                    !! Swap values if they are out of order in [i, i+1] region
                    temp = collection(i)
                    collection(i) = collection(i + 1)
                    collection(i + 1) = temp

                    swapped = .true. !! Set swapped flag to true
                end if
            end do

            !! Stop iterating if collection is sorted.
            if (.not. swapped) exit
        end do

    end subroutine bubble_sort

end module bubble_sort_module
