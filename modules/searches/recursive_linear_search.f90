!> Recursive Linear Search
!!
!! This is the recursive implementation of the linear search algorithm in Fortran.

program recursive_linear_search_program
    implicit none

    integer, dimension(5) :: array

    array = (/ 306, 1005, 5, 62, 0 /)

    !! Search for the number 62 in array.
    print*, "Target = 62: ", recursive_linear_search(array, size(array), 62) !! Prints 4.

    !! Search for the number 10 in array.
    print*, "Target = 10: ", recursive_linear_search(array, size(array), 10) !! Prints -1 because item 10 is not found.

contains 

    !! This function recursively searches for a target in a given collection.
    !! Returns the index of the found target or -1 if target is not found.
    recursive function recursive_linear_search(collection, index, target) result(target_index)
    !! recursive keyword declares that the function is recursive.
    !! result keyword specifies the function result.
    
        integer, dimension(:), intent(in) :: collection      !! A collection for elements of type integer
        integer, intent(in)               :: index           !! Initially collection size.
        integer, intent(in)               :: target          !! Target value to be searched.
        integer                           :: target_index    !! Target's index in the collection to return.

        !! Stop recursive calls with "if size is 0 and item not found" base condition.
        if (index .eq. 0) then
            target_index = -1
            return
        end if

        if (collection(index) .eq. target) then
            !! Set target index if target found.
            target_index = index
        else 
            !! Recursively call the function with (index - 1) for index argument.
            target_index = recursive_linear_search(collection, index-1, target)
        end if

    end function recursive_linear_search

end program recursive_linear_search_program