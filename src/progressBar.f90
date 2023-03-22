module progressBar
    
    use iso_fortran_env, only : sp => real32, dp => real64, int64, int32

    implicit none
    
    type :: pbar
        integer(kind=int64) :: iters, current_iter, time_remaing(3), time_taken(3), threads
        real(kind=dp)       :: percentage, start_t, start_tt, finish_t, average
        logical             :: first
        contains
            procedure :: progress => progress_sub
    end type pbar

    interface pbar
        module procedure :: init_pbar_func32, init_pbar_func64
    end interface pbar

    private
    public :: pbar

    contains
    
        type(pbar) function init_pbar_func32(n)

            use omp_lib

            implicit none

            integer(kind=int32), intent(IN) :: n

#ifdef _OPENMP
            init_pbar_func32%threads = omp_get_max_threads()
#else
            init_pbar_func32%threads = 1_int64
#endif
            init_pbar_func32%iters = int(n, kind=int64)
            init_pbar_func32%current_iter = 0_int64
            init_pbar_func32%time_remaing = 0_int64
            init_pbar_func32%time_taken = 0_int64
            init_pbar_func32%percentage = 0.0 
            init_pbar_func32%start_t = 0.0
            init_pbar_func32%start_tt = 0.0
            init_pbar_func32%finish_t = 0.0  
            init_pbar_func32%average = 0.0
            init_pbar_func32%first = .true.

        end function init_pbar_func32


        type(pbar) function init_pbar_func64(n)

        use omp_lib

        implicit none

        integer(kind=int64), intent(IN) :: n

#ifdef _OPENMP
        init_pbar_func64%threads = omp_get_max_threads()
#else
        init_pbar_func64%threads = 1_int64
#endif
        init_pbar_func64%iters = n
        init_pbar_func64%current_iter = 0_int64
        init_pbar_func64%time_remaing = 0_int64
        init_pbar_func64%time_taken = 0_int64
        init_pbar_func64%percentage = 0.0 
        init_pbar_func64%start_t = 0.0
        init_pbar_func64%start_tt = 0.0
        init_pbar_func64%finish_t = 0.0  
        init_pbar_func64%average = 0.0
        init_pbar_func64%first = .true.

    end function init_pbar_func64

        subroutine progress_sub(this)

            use iso_fortran_env, only : output_unit
            use stringUtils,     only : str
            use colours,         only : start_code, end_code

            class(pbar) :: this
            integer           :: width
            character(len=52) :: line
            real(kind=dp)     :: time

!$omp critical
            if(.not. this%first)then
                call cpu_time(this%finish_t)
                this%average = this%average + (this%finish_t - this%start_t)
                time = this%average / real(this%threads * this%current_iter)
                time = time * (this%iters - this%current_iter)
                this%time_remaing(1) = floor(time / (60*60))
                this%time_remaing(2) = floor(mod(time / 60, 60._dp))
                this%time_remaing(3) = int(mod(time, 60._dp))

                time = (this%finish_t - this%start_tt) / this%threads
                this%time_taken(1) = floor(time / (60*60))
                this%time_taken(2) = floor(mod(time / 60, 60._dp))
                this%time_taken(3) = int(mod(time, 60._dp))
            else    
                this%first = .false.
                call cpu_time(this%start_tt)
            end if


            this%current_iter = this%current_iter + 1
            if(this%current_iter <= this%iters)then
                this%percentage = 100._dp*real(this%current_iter) / real(this%iters)

                width = int(this%percentage/ 2._dp)
                line = "[" // repeat("#", width) // repeat(" ", 50 - width) // "]"

                write(unit=output_unit,fmt='(A)',advance="no") start_code//"1000D"//line//&
                " "//str(int(this%percentage),3)//"%  ["//str(this%time_taken)//"<"//&
                str(this%time_remaing)//"]"

                if(this%percentage >= 100._dp)write(unit=output_unit,fmt='(A)')new_line("a")
                flush(output_unit)
            end if
!$omp end critical
            call cpu_time(this%start_t)

        end subroutine progress_sub
end module progressBar