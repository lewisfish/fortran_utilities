module progressBar
    
    use iso_fortran_env, only : sp => real32, dp => real64

    implicit none
    
    type :: pbar
        integer       :: iters, current_iter, time_remaing(3), time_taken(3), threads
        real(kind=dp) :: percentage, start_t, start_tt, finish_t, average
        logical       :: first
        contains
            procedure :: progress => progress_sub
    end type pbar

    interface pbar
        module procedure :: init_pbar_func
    end interface pbar

    private
    public :: pbar

    contains
    
        type(pbar) function init_pbar_func(n)

            use omp_lib

            implicit none

            integer, intent(IN) :: n

#ifdef _OPENMP
            init_pbar_func%threads = omp_get_max_threads()
#else
            init_pbar_func%threads = 1
#endif
            init_pbar_func%iters = n
            init_pbar_func%current_iter = 0
            init_pbar_func%time_remaing = 0 
            init_pbar_func%time_taken = 0
            init_pbar_func%percentage = 0.0 
            init_pbar_func%start_t = 0.0
            init_pbar_func%start_tt = 0.0
            init_pbar_func%finish_t = 0.0  
            init_pbar_func%average = 0.0
            init_pbar_func%first = .true.

        end function init_pbar_func


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