module colours

    use iso_fortran_env, only : sp => real32, dp => real64

    implicit none
    
     !foreground colours
    character(len=2), parameter :: black   = '30', &
                                   red     = '31', &
                                   green   = '32', &
                                   yellow  = '33', &
                                   blue    = '34', &
                                   magenta = '35', &
                                   cyan    = '36', &
                                   white   = '37'

    !background colours
    character(len=2), parameter :: black_b   = '40', &
                                   red_b     = '41', &
                                   green_b   = '42', &
                                   yellow_b  = '43', &
                                   blue_b    = '44', &
                                   magenta_b = '45', &
                                   cyan_b    = '46', &
                                   white_b   = '47'

    !styles
    character(len=2), parameter :: bold          = '01', &
                                   italic        = '03', &
                                   underline     = '04', &
                                   inverse       = '07', &
                                   strikethrough = '09'

    !ANSI control characters                               
    character(len=2), parameter :: start_code = achar(27)//'['
    character(len=3), parameter :: end_code = '[0m'


    !functions to add colour to output via ANSI colour codes
    interface colour
        module procedure colour_char
        module procedure colour_int
        module procedure colour_real4
        module procedure colour_real8
    end interface

    private
    public :: colour
    public :: black, red, green, yellow, blue, magenta, cyan, white
    public :: black_b, red_b, green_b, yellow_b, blue_b, magenta_b, cyan_b, white_b
    public :: bold, italic, underline, inverse, strikethrough
    public :: start_code, end_code

contains

    function colour_char(string, fmt1, fmt2, fmt3, fmt4, fmt5) result(colourised)

            implicit none

            character(*),           intent(IN) :: string
            character(*), optional, intent(IN) :: fmt1, fmt2, fmt3, fmt4, fmt5
            character(len=:), allocatable      :: colourised

            colourised = string

            if(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4) .and. present(fmt5))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//';'//fmt5//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2))then
                colourised = start_code//fmt1//';'//fmt2//'m'//string//achar(27)//end_code
            elseif(present(fmt1))then
                colourised = start_code//fmt1//'m'//string//achar(27)//end_code
            end if
        end function colour_char


        function colour_int(inte, fmt1, fmt2, fmt3, fmt4, fmt5) result(colourised)

            implicit none

            integer,                intent(IN) :: inte
            character(*), optional, intent(IN) :: fmt1, fmt2, fmt3, fmt4, fmt5

            character(len=:), allocatable :: colourised, string
            character(len=50)             :: tmp

            write(tmp,'(I50.1)') inte
            string = trim(adjustl(tmp))
            colourised = trim(adjustl(string))

            if(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4) .and. present(fmt5))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//';'//fmt5//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2))then
                colourised = start_code//fmt1//';'//fmt2//'m'//string//achar(27)//end_code
            elseif(present(fmt1))then
                colourised = start_code//fmt1//'m'//string//achar(27)//end_code
            end if
        end function colour_int


        function colour_real4(inte, fmt1, fmt2, fmt3, fmt4, fmt5) result(colourised)

            implicit none

            real(kind=sp),          intent(IN) :: inte
            character(*), optional, intent(IN) :: fmt1, fmt2, fmt3, fmt4, fmt5

            character(len=:), allocatable :: colourised, string
            character(len=50)             :: tmp

            write(tmp,'(F50.8)') inte
            string = trim(adjustl(tmp))
            colourised = trim(adjustl(string))

            if(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4) .and. present(fmt5))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//';'//fmt5//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2))then
                colourised = start_code//fmt1//';'//fmt2//'m'//string//achar(27)//end_code
            elseif(present(fmt1))then
                colourised = start_code//fmt1//'m'//string//achar(27)//end_code
            end if
        end function colour_real4


        function colour_real8(inte, fmt1, fmt2, fmt3, fmt4, fmt5) result(colourised)

            implicit none

            real(kind=dp),          intent(IN) :: inte
            character(*), optional, intent(IN) :: fmt1, fmt2, fmt3, fmt4, fmt5

            character(len=:), allocatable :: colourised, string
            character(len=50)             :: tmp

            write(tmp,'(F50.16)') inte
            string = trim(adjustl(tmp))
            colourised = trim(adjustl(string))

            if(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4) .and. present(fmt5))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//';'//fmt5//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3))then
                colourised = start_code//fmt1//';'//fmt2//';'//fmt3//'m'//string//achar(27)//end_code
            elseif(present(fmt1) .and. present(fmt2))then
                colourised = start_code//fmt1//';'//fmt2//'m'//string//achar(27)//end_code
            elseif(present(fmt1))then
                colourised = start_code//fmt1//'m'//string//achar(27)//end_code
            end if
        end function colour_real8
end module colours