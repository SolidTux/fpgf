module mod_plotattr
    implicit none
    private

    type, public :: PlotAttr
        character(:), allocatable :: name
        character(:), allocatable :: value
    contains
        procedure :: write => plotattr_write
        procedure :: copy => plotattr_copy
        generic :: assignment(=) => copy
    end type PlotAttr

    type, public, extends(PlotAttr) :: NoMarker
    contains
    end type NoMarker

    interface NoMarker
        module procedure new_nomarker
    end interface NoMarker
contains
    subroutine plotattr_write(this, u)
        class(PlotAttr), intent(in) :: this
        integer, intent(in) :: u

        if (this%value .eq. '') then
            write (u,*) this%name, ','
        else
            write (u,*) this%name, '=', this%value, ','
        end if
    end subroutine plotattr_write

    function new_nomarker()
        type(NoMarker) :: new_nomarker

        new_nomarker%name = 'no markers'
        new_nomarker%value = ''
    end function new_nomarker

    subroutine plotattr_copy(to, from)
        class(PlotAttr), intent(out) :: to
        class(PlotAttr), intent(in) :: from

        to%name = from%name
        to%value = from%value
    end subroutine plotattr_copy
end module mod_plotattr
