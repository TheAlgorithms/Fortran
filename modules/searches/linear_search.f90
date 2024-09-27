!> Linear Search
!> Module implementing the linear search algorithm in Fortran.

module linear_search_module
    implicit none

contains

    !! This function searches for a target in a given collection.
    !! Returns the index of the found target or -1 if target is not found.
    function linear_search(collection, target) result(target_index)
        integer, dimension(:), intent(in) :: collection   !! A collection for elements of type integer
        integer, intent(in)               :: target       !! Target value to be searched.
        integer                           :: target_index !! Target's index in the collection to return.

        integer :: i, collection_size

        collection_size = size(collection)

        do i = 1, collection_size
            if (collection(i) .eq. target) then
                target_index = i !! Set the target index if target found.
                return !! Exit the function.
            end if
        end do

        target_index = -1 !! Set the index to -1 if target not found.

    end function linear_search

end module linear_search_module
