module OS
    
    implicit none
    
    interface
        function c_chdir(path) bind(C, name="chdir")
                
            use iso_c_binding

            character(kind=c_char), intent(IN) :: path(*)
            integer(kind=C_int) :: c_chdir
        end function c_chdir
    end interface
    
    contains
    
        subroutine chdir(path, error)

            use iso_c_binding, only : c_null_char

            implicit none

            character(*), intent(IN)       :: path
            integer, optional, intent(OUT) :: error

            integer :: err

            err = c_chdir(trim(path)//c_null_char)
            if(present(error))error = err
        end subroutine chdir

        function mem_free()

            use iso_fortran_env, only : int64 !as numbers are large

            implicit none

            integer(int64) :: mem_free

            integer(int64)    :: i
            character(len=15) :: tmp
            integer           :: u

            open(newunit=u,file='/proc/meminfo',status='old')

            read(u,*)tmp, i
            read(u,*)tmp, i
            read(u,*)tmp, i
            
            mem_free = i * 1024_int64 !convert from Kib to b 
        end function mem_free
end module OS