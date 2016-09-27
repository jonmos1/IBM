module BODY
!MODULE HEADER
!
!
!
!===========================================================
    use INPUT
    use FILES
    use GLOBALMODULEVARS
    implicit none


    contains
!==================================================================

    subroutine BodyGen(file_name, b)
        !TODO(JON): maxval(shape(b)) is not generic for 3 dimensional bodies... reevaluate when that time comes
        character(len=*)            :: file_name
        real                        :: b(:,:,:)
        integer, parameter          :: is_2D = 1


        call FilesZoneInit(file_name, 'BODY', 'BLUE', size(b,1), 1, 1)
        open(unit=101, file=file_name, action='write', position='append')

        do i = 1, maxval(shape(b))
            write(101,*) b(i,:,is_2D) , u_zero , p_zero
        end do

        close(101)

    end subroutine BodyGen


end module
