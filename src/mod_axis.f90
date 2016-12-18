module mod_axis
    implicit none
    private

    type, public :: Axis
    contains
        procedure :: head => axis_head
        procedure :: foot => axis_head
    end type Axis
contains
    subroutine axis_head(this, u)
        class(Axis) :: this
        integer, intent(in) :: u
        write (u,*) '\begin{axis}'
    end subroutine axis_head

    subroutine write_foot(this, u)
        class(Axis) :: this
        integer, intent(in) :: u
        write (u,*) '\end{axis}'
    end subroutine write_foot
end module mod_axis
