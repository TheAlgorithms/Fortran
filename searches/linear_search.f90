!> Linear Search
!!
!! This is an implementation of the linear search algorithm in Fortran.

program linear_search_program
    implicit none

    integer, dimension(5) :: array

    array = (/ 540, 6, 10, 100, 3 /)

    !! Search for the number 6 in array.
    print*, "Target = 6: ", linear_search(array, 6) !! Prints 2.

    !! Search for the number 5 in array.
    print*, "Target = 5: ", linear_search(array, 5) !! Prints -1 because item 5 is not found.

contains 

    !! This function searches for a target in a given collection.
    !! Returns the index of the found target or -1 if target is not found.
    function linear_search (collection, target) result(target_index) !! result keyword used to specify function result.
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

end program linear_search_program

