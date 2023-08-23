!> Simple Hello World Program
!! 
!! A "hello world" program is most programmer's
!! first Fortran program. The goal is to print out
!! "Hello, World. Hello, Fortran!" on the terminal.
!! Refer - https://en.wikipedia.org/wiki/%22Hello,_World!%22_program

!! Fortran naming convention for program is small camel case
program helloWorld
    !! Fortran print statement starts with print *,
    Print *, 'Hello, World. Hello, Fortran!'

!! A single end statement can also build with a pass
!! Yet `end program [program_name]` is recommended!
end program helloWorld