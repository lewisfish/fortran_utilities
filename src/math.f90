module math
    
    use iso_fortran_env, only : sp => real32, dp => real64

    implicit none

    real(kind=dp), parameter :: PI=4._dp*atan(1._dp)
    real(kind=dp), parameter :: TWOPI=2._dp*4._dp*atan(1._dp)

    !subroutines to swap variables
    interface swap
        module procedure swap_I
        module procedure swap_R4
        module procedure swap_R8
    end interface swap

    !subroutines to clamp variables
    interface clamp
        module procedure clamp_R4
        module procedure clamp_R8
        ! module procedure clamp_vector
    end interface clamp

    !subroutines to mix variables
    interface mix
        module procedure mix_R4
        module procedure mix_R8
    end interface mix

    !subroutines to deg2rad variables
    interface deg2rad
        module procedure deg2rad_R4
        module procedure deg2rad_R8
    end interface deg2rad

    !subroutines to rad2deg variables
    interface rad2deg
        module procedure rad2deg_R4
        module procedure rad2deg_R8
    end interface rad2deg


    private
    public :: PI, TWOPI, clamp, swap, deg2rad, rad2deg, mix

    contains
    
        pure function lerp(t, v1, v2) result(res)

            real(kind=dp) :: res
            real(kind=dp), intent(IN) :: v1, v2, t

            res = (1._dp - t) * v1 + t * v2

        end function lerp

        pure function rad2deg_R4(angle) result(res)

            real(kind=sp), intent(IN) :: angle
            real(kind=sp) :: res

            res = angle/real(PI,kind=sp)*180._sp

        end function rad2deg_R4

        pure function rad2deg_R8(angle) result(res)

            real(kind=dp), intent(IN) :: angle
            real(kind=dp) :: res

            res = angle/PI*180._dp

        end function rad2deg_R8


        pure function deg2rad_R4(angle) result(res)

            real(kind=sp), intent(IN) :: angle
            real(kind=sp) :: res

            res = angle*PI/180._sp

        end function deg2rad_R4

        pure function deg2rad_R8(angle) result(res)

            real(kind=dp), intent(IN) :: angle
            real(kind=dp) :: res

            res = angle*PI/180._dp

        end function deg2rad_R8

        pure function clamp_R4(val, lo, hi) result(res)

            implicit none

            real(kind=dp) :: res
            real(kind=dp), intent(IN) :: val, hi, lo

            if(val < lo)then
                res = lo
            elseif(val > hi)then
                res = hi
            else
                res = val
            end if

        end function clamp_R4

        pure function clamp_R8(val, lo, hi) result(res)

            implicit none

            real(kind=sp) :: res
            real(kind=sp), intent(IN) :: val, hi, lo

            if(val < lo)then
                res = lo
            elseif(val > hi)then
                res = hi
            else
                res = val
            end if

        end function clamp_R8


        pure function mix_R4(x, y, a) result(res)

            implicit none

            real(kind=sp) :: res
            real(kind=sp), intent(IN) :: x, y, a

            res = x*(1._sp - a) + y*a

        end function mix_R4

        pure function mix_R8(x, y, a) result(res)

            implicit none

            real(kind=dp) :: res
            real(kind=dp), intent(IN) :: x, y, a

            res = x*(1._dp - a) + y*a

        end function mix_R8


        subroutine swap_I(a, b)

            implicit none

            integer, intent(INOUT) :: a, b
            integer :: tmp

            tmp = a
            a = b
            b = tmp

        end subroutine swap_I


        subroutine swap_R4(a, b)

            implicit none

            real(kind=sp), intent(INOUT) :: a, b
            real(kind=sp) :: tmp

            tmp = a
            a = b
            b = tmp

        end subroutine swap_R4


        subroutine swap_R8(a, b)

            implicit none

            real(kind=dp), intent(INOUT) :: a, b
            real(kind=dp) :: tmp

            tmp = a
            a = b
            b = tmp

        end subroutine swap_R8


end module math