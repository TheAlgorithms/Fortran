!> Recursive Bubble Sort Module
!!
!! This module contains a subroutine for sorting a collection using the recursive bubble sort algorithm.

module recursive_bubble_sort_module
    implicit none
contains

    !! This subroutine sorts the collection recursively using bubble sort.
    !! Recursive keyword at the start declares that the function is recursive.
    recursive subroutine recursive_bubble_sort(collection, collection_size)
        real, dimension(:), intent(inout) :: collection !! A collection for elements of type real.
        integer, intent(in) :: collection_size          !! Collection's size.

        integer :: i
        real :: temp

        !! Perform bubble sort on the collection.
        do i = 1, collection_size - 1
            if (collection(i) .gt. collection(i + 1)) then
                !! Swap values if they are out of order in [i, i + 1] region.
                temp = collection(i)
                collection(i) = collection(i + 1)
                collection(i + 1) = temp
            end if
        end do

        !! Recursively call the subroutine with (collection_size - 1) if size > 2.
        if (collection_size .gt. 2) then
            call recursive_bubble_sort(collection, collection_size - 1)
        end if

    end subroutine recursive_bubble_sort

end module recursive_bubble_sort_module
