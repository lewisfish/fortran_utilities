module time
    
    use iso_fortran_env, only : sp => real32, dp => real64

    implicit none

    private
    public :: get_time, print_time

    contains
    
        real function get_time()

#ifdef _OPENMP
            use omp_lib

            get_time = omp_get_wtime()
#else
            call cpu_time(get_time)
#endif
        end function get_time


        subroutine print_time(time, id)

            real(kind=dp), intent(IN) :: time
            integer,       intent(IN) :: id

            if(id == 0)then
                if(time >= 60._dp)then
                   print*, floor((time)/60._dp),"mins", mod(time, 60._dp)/100._dp,"s"
                else
                   print*, 'time taken ~',time,'s'
                end if
            end if
        end subroutine print_time
end module time