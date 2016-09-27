module CHECKS
!MODULE HEADER
!
!
!===========================================================
    use GLOBALMODULEVARS
    implicit none


    contains
!===========================================================



    subroutine Checks2D(x_N,x_min,x_max,flag_2D)
        !Dummy vars:____________________________
        real, dimension(:,:)    :: x_min, x_max
        integer,dimension(:,:)  :: x_N
        logical                 :: flag_2D

        !Checking:______________________________
        flag_2D = .FALSE.
        do d=1,3
            if(x_min(1,d) .EQ. x_max(1,d) .OR. x_N(1,d) .EQ. 1) then
                flag_2D = .TRUE.
            end if
        end do

    end subroutine Checks2D
!===========================================================





end module
