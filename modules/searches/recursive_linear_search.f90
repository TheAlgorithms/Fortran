!> Recursive Linear Search Module
!!
!! This module contains a recursive implementation of the linear search algorithm.
!! It includes the `recursive_linear_search` function to search for a target value within a given collection.

module recursive_linear_search_module
    implicit none
contains

    !! This recursive function searches for a target in a collection.
    !! Returns the index of the found target or -1 if target is not found.
    recursive function recursive_linear_search(collection, index, target) result(target_index)
        integer, dimension(:), intent(in) :: collection  !! A collection of integer elements
        integer, intent(in)               :: index       !! The current index in the collection
        integer, intent(in)               :: target      !! The target value to be searched
        integer                           :: target_index !! The index where the target is found

        !! Base case: if index is 0 and target is not found, return -1
        if (index .eq. 0) then
            target_index = -1
            return
        end if

        !! Check if the target is at the current index
        if (collection(index) .eq. target) then
            target_index = index
        else
            !! Recursively search in the remaining part of the collection
            target_index = recursive_linear_search(collection, index-1, target)
        end if

    end function recursive_linear_search

end module recursive_linear_search_module