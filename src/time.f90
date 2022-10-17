module time
    
    use iso_fortran_env, only : sp => real32, dp => real64

    implicit none


    interface print_time
        module procedure print_timeR4
        module procedure print_timeR8
    end interface print_time


    private
    public :: get_time, print_time

    contains
    
        real(kind=dp) function get_time()

#ifdef _OPENMP
            use omp_lib

            get_time = omp_get_wtime()
#else
            call cpu_time(get_time)
#endif
        end function get_time


        subroutine print_timeR4(time, id)

            real(kind=sp), intent(IN) :: time
            integer,       intent(IN) :: id

            if(id == 0)then
                if(time >= 60._sp)then
                   print*, floor((time)/60._sp),"mins", mod(time, 60._sp)/100._sp,"s"
                else
                   print*, 'time taken ~',time,'s'
                end if
            end if
        end subroutine print_timeR4

        subroutine print_timeR8(time, id)

            real(kind=dp), intent(IN) :: time
            integer,       intent(IN) :: id

            if(id == 0)then
                if(time >= 60._dp)then
                   print*, floor((time)/60._dp),"mins", mod(time, 60._dp)/100._dp,"s"
                else
                   print*, 'time taken ~',time,'s'
                end if
            end if
        end subroutine print_timeR8

end module time