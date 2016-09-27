module FILES
!MODULE HEADER
!
!
!
!===========================================================
    use GLOBALMODULEVARS
    implicit none


    contains
!===========================================================

    subroutine FilesInit(file_name)
    !----------------------------------------------------------------------
    character(len=*)            :: file_name
    character(len=4), parameter :: file_extension = '.dat'

    logical(4)          :: file_exists  !For file inquiries about existence
    character           :: answer       !for user decisions
    !----------------------------------------------------------------------

    !Read user input
    print*, 'Please give a filename:   '
    read(*,*) file_name
    print*, 'Thanks! Directory and data files will now be established.'
    print*, '---------------------------------------------------------'


    !Make a directory for the files too keep program files and data files separated
    call system('mkdir '//file_name)
    !print*, 'New directory made: ...\'//file_name !TODO(JON):Show this message only if directory is actually made


    !Create file in new directory
    file_name = trim(file_name)//'\'//trim(file_name)//file_extension
        !Find out if file exists, and ask user whether he wants to overwrite or not
        inquire(file = file_name, exist = file_exists)
        if(file_exists) then
            print*, '---------------------------------------------------------'
            print*, 'File already exists. Do you want to overwrite? (y/n)'
            print*, '. . . . . . . . . . . . . . . . . . . . . . . . . . '
900         read(*,*) answer
            !Processing user decision:
            if(answer.eq.('y')) then
                goto 910    !overwrites the file
            elseif(answer.eq.('n')) then
                print*, 'No files created. Exiting program'
                call exit()
            else
                !Asks for user input again if format is wrong
                print*, 'Please answer "y" or "n"'
                goto 900
            end if
    end if


910 print*, '---------------------------------------------------------'
    open(unit=101, file=file_name)
    write(101,80,advance='no') 'TITLE = '           !
    write(101,80) file_name                         !
    write(101,80) 'VARIABLES = "X", "Y", "Z", "U", "V", "W", "P"'       !
    write(101,80) ' '                               !
80  format(A)
        !...............................................!

    close(101)
    print*, 'New file created at '//file_name
    print*, '---------------------------------------------------------'

    end subroutine FilesInit
!===========================================================




    subroutine FilesBody(file_name, b)  !Appends the body array to the tecplot .dat-file
        !TODO(JON): maxval(shape(b)) is not generic for 3 dimensional bodies... reevaluate when that time comes
        character(len=*)            :: file_name
        real                        :: b(:,:,:)
        integer, parameter          :: is_2D = 1


        open(unit=101, file=file_name, action='write', position='append')

        !TODO(JON): add the new tecplot zone here
        write(101,80) ''
        write(101,80) 'ZONE'
        write(101,80) 'T= "BODY"'
        write(101,80) 'C= "BLUE"'
        write(101,80) 'F = "POINT"'
        write(101,910) 'I = ', maxval(shape(b)), ', J= ', 1, ', K= ', 1


80 format(A)
910 format(A,I12,A,I12,A,I12)


        do i = 1, maxval(shape(b))
            write(101,*) b(i,:,is_2D) , u_zero , p_zero
        end do

        close(101)

    end subroutine FilesBody
!===========================================================


    subroutine FilesZoneInit(file_name, zone_title, zone_colour, Imax,Jmax,Kmax )
        character(len=*)    :: file_name
        character(len=*)    :: zone_title
        character(len=*)    :: zone_colour  !RED, .. See tec-plot colour chart
        integer             :: Imax,Jmax,Kmax

    open(unit=101, file=file_name, action='write', position='append')
        write(101,80) ' '                                                                            !
        write(101,80) 'ZONE'                                                                            !
        write(101,90) 'T="', zone_title, '"'                                                                    !
        write(101,90) 'C="', zone_colour, '"'                                                                         !
        write(101,80) 'F="POINT"'
        write(101,910) 'I = ', Imax, ', J= ', Jmax, ', K= ', Kmax

80  format(A)
90  format(A,A,A)
910 format(A,I12,A,I12,A,I12)


    close(101)
    end subroutine FilesZoneInit
















!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!SUBROUTINE ARCHIVE FOR THIS MODULE_____________________________________________________________!
!                                                                                               !
!_______________________________________________________________________________________________!




end module
