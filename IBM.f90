module IBM
!Module that contains subroutines for:
!   1) detecting intersection between body and grid-lines
!   2) implement boundary conditions in intersections by interpolation
!   3) ...
!resulting in a flow field modified for the boundary
!===========================================================
    use GLOBALMODULEVARS
    use MESSAGE
    use FILES
    implicit none


    contains
!===========================================================
    !Detect intersection between body and underlying grid lines
    !TODO(JON): This only works for 2D figures, as a 3D figure must work with surfaces and not single vectors.
    !TODO(JON): maxval(shape(b)) is not generic for 3_d bodies...

    subroutine IBMInterSect_2D(file_name,flag_2D,x_min,dx,x_N,b)
        !Dummy vars
        character(len=*)        ::     file_name
        logical                 ::     flag_2D
        real, dimension(:,:)    ::     x_min
        real, dimension(:,:)    ::     dx
        integer, dimension(:,:) ::     x_N
        real, dimension(:,:,:)  ::     b
        !Internal vars
        integer, parameter      ::     is_2D = 1
        integer                 ::     flag_excluded_dim   !To keep track of which dimension is excluded

        integer                 ::     dir
        integer                 ::     intersections_N
        real                    ::     active_line
        real                    ::     segment_slope
        real                    ::     intersection


        real    ::     b_comp(size(b,1)      , 2)      !TODO(JON): rewrite more elegantly

        real    ::     b_c(size(b,1)      -1 , 2)      !TODO(JON): noe smør på flesk her?
        real    ::     b_cplus1(size(b,1) -1 , 2)

        integer ::     b_check(size(b,1)  -1 , 2)      !Note - should be one entry shorter than b_comp
        real    ::     b_dir(size(b,1)    -1 , 2)
        real    ::     b_normal(size(b,1) -1 , 2)


    !Exit program if this subroutine is called for a 3D-problem
    if(.NOT.flag_2D) then
        print*,'Error in IBMInterSect2D : problem not in 2D - try adjusting domain input'
        call MessageExit()
    end if




    !STEP 1_______________________________________________________________________________________________________

    !Make a computational matrix for the body coordinates (b_comp) where all values are scaled to the integer grid
    !The dimension that is not in use is put to x_min(1,d) everywhere.
    flag_excluded_dim = 0
    do d=1,3 !for all directions
        if(dx(1,d) .EQ. 0) then
            if(flag_excluded_dim /= 0) then
                print*, 'Error in input - at least two dimensions must be defined'
                call MessageExit()
            end if
            flag_excluded_dim = d
        else
            b_comp(:,d) = (b(:,d, is_2D) - x_min(1,d))/dx(1,d) +1 !as x = x_min + (i-1)dx, then (i-1) = (x-x_min)/dx
        end if
    end do
    !b_comp should now be a Nx2 matrix containing the body's points in the computational domain.


    !STEP 2_______________________________________________________________________________________________________
    !Make the direction vector matrix and the normal vector matrix, and:...
    !Make a matrix which have non-zero entries on body-points with intersecting grid lines before the succeeding body point
    !(checks if two neighbouring points are crossing any whole numbers)
    !TODO(JON): size(b_comp,1) this only works for 2D-bodies
    b_c         = b_comp(1:size(b_comp,1)-1,:)      ! b(1:end-1, :)
    b_cplus1    = b_comp(2:size(b_comp,1),:)        ! b(2:end  , :)

    !Normal and directional vector
    !See p67 in project thesis: b_dir = b(c+1)-b(c):
    b_dir = b_cplus1 - b_c

    b_normal(:,1)=  b_dir(:,2)
    b_normal(:,2)= -b_dir(:,1)

    !Vector for checking number of gridlines crossed between each body point
    b_check = ceiling(b_cplus1) - ceiling(b_c)

    !NOW: b_check is now a matrix that contains information about:___________________________________________________________________
    !           i)  how many lines are crossed between two body points in 1,2 and 3 direction respectively                          !
    !           ii) which way the next point on the body lies in 1, 2 and 3 direction (positive is up,right, negative is down,left) !
    !     so the number "(sign)N" in entry number (c,d) should be read as:                                                          !
    !           From body point number "c" in direction "d" there will be "N" gridlines crossing normally to direction "d".         !
    !           The body point number "c+1" lies in the (sign) "d"-direction from point number "c"                                  !
    !     Body vector direction is positive counter-clockwise --> dir of normal vector is allways to the right of body vector       !
    !                                                                                                                               !
    do d=1,2                                                                                                                       !
        print*, b_check(:,d)                                                                                                       !
        print*, '================================================================================================================='!
    end do                                                                                                                         !
    !                                                                                                                               !
    !...............................................................................................................................!


    !STEP 3_______________________________________________________________________________________________________
    !Find the intersections of each line from the body vector line

    !intersection vector - must be of same length as the number of intersections (which are contained in the b_check-vector
    !allocate(intersect_x( sum of absolute values of b_check(:,1) , 1)
    !allocate(intersect_y( sum of absolute values of b_check(:,2) , 2)
    intersections_N = sum(abs(b_check)) !Numberr of intersections

    call FilesZoneInit(file_name, 'Intersections', 'BLACK', intersections_N ,1,1)
    open(unit = 101, file=file_name, action='write', position='append')


    do d=1,2
        ! as d=1, we are checking the vertical lines (i) for intersections
        ! as d=2, we are checking the horizontal lines (k) for intersection
!---------------------------------------------------------------------------
        !To use for the interpolation:
        if(d==1) then
            dd = 2
        else
            dd = 1
        end if
!---------------------------------------------------------------------------
        do c=1,size(b_check,1)    !Going through the b_check vector

            !if any inte rsections:
            if(b_check(c,d) /= 0) then
                dir = sign(1,b_check(c,d))
                segment_slope = b_dir(c,dd)/b_dir(c,d)      !Dy/Dx for d=1, and Dx/Dy for d=2
                !write(*,*) '/////////////////////////////////////////////////'
                !write(*,*) 'd= :', d
                !write(*,*) 'c= :', c
                !write(*,*) 'direction       :',   dir
                !write(*,*) 'segment_slope   :',   segment_slope
                !write(*,*) '.................'
                !write(*,*) 'intersections   :'
                !write(*,*) '.................'
!---------------------------------------------------------------------------
                do cc=0, (abs(b_check(c,d))-1)  !Go through all the intersections on this point
                    !intersection of vertical lines:

                        active_line  = nint(b_comp(c,d)+dir*(cc+0.5)) !use round(b_comp + cc +-0.5) instead of floor and ceiling (round(+- a + 0.5) is ceil if + and floor if -
                        intersection = dx(1,dd)*( b_comp(c,dd) + segment_slope*(active_line-b_comp(c,d))-1) + x_min(1,dd)
                        active_line  = dx(1,d)*(nint(b_comp(c,d)+dir*(cc+0.5)) - 1) + x_min(1,d)
!---------------------------------------------------------------------------
                    if(d==1) then
                        write(101,*) active_line, intersection, 0 , u_zero,p_zero
                    else if(d==2) then
                        write(101,*) intersection, active_line, 0 , u_zero,p_zero
                    end if
                    !write(*,*) 'cc              :',   cc
                    !write(*,*) 'active line     :',   active_line
                    !write(*,*) 'intersection    :',   intersection
                    !write(*,*) 'active_l_comp   :', nint(b_comp(c,d)+dir*(cc+0.5))
!---------------------------------------------------------------------------
                end do
!---------------------------------------------------------------------------
            end if
!---------------------------------------------------------------------------
        end do
!---------------------------------------------------------------------------
    end do
!---------------------------------------------------------------------------


    close(101)

    end subroutine IBMInterSect_2d
!===========================================================


    !Implement boundary conditions on intersection points by interpolation and manipulation of neighbouring points
    subroutine ImplementIBM()

    end subroutine
!===========================================================























!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!SUBROUTINE ARCHIVE FOR THIS MODULE_____________________________________________________________!
!                                                                                               !
!_______________________________________________________________________________________________!

    !Detect intersection between body and underlying grid lines
    subroutine IBMInterSectOld(x_min,y_min,z_min,dx,dy,dz,x_N,y_N,z_N,b)
        real    ::     x_min,y_min,z_min
        real    ::     dx,dy,dz
        integer ::     x_N,y_N,z_N
        real    ::     b(:,:)
        real    ::     b_comp(maxval(shape(b)),3)               !TODO(JON): rewrite more elegant
        real    ::     b_intersect_check(maxval(shape(b))-1,3)  !one index shorter than the others

    !1  Make a computational matrix for the body coordinates (b_comp) where all values are scaled to the integer grid
    !b_comp must be shifted wrt the domain minimums so that comp grid starts at 1 in lower left corner
    b_comp(:,1) = (b(:,1)-x_min)/dx !maybe use elementwise matrix operations instead (with dx(1,2,3) as a vector) - vectorization to be done by a subroutine
    b_comp(:,2) = (b(:,2)-y_min)/dy
    b_comp(:,3) = (b(:,3)-z_min)/dz

    print*, b_comp(:,1)


    !2  Make a matrix which have nonzero entries on body-points with intersecting grid lines before the succeeding body point
    b_intersect_check = ceiling(b_comp(2:size(b_comp,1),:)) - ceiling(b_comp(1:size(b_comp,1)-1,:))       !size(b_comp,1) represents the indice of the last entry (analogous to "end" in matlab

    !do i=1,size(b_comp,2)
    !    print*, b_intersect_check(:,i)
    !    print*, '=========================='
    !end do

    end subroutine IBMInterSectOld
!===========================================================



end module
