module MESSAGE

    use GLOBALMODULEVARS
    implicit none

    contains

    subroutine MessageExit()
        print*, '...exiting program...'
        call exit()
    end subroutine MessageExit

end module
