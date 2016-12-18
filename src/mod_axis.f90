module mod_axis
    implicit none
    private

    type, public :: Axis
    contains
        procedure :: head => axis_head
        procedure :: foot => axis_foot
    end type Axis
contains
    subroutine axis_head(this, u)
        class(Axis) :: this
        integer, intent(in) :: u
        write (u,*) '\begin{axis}'
    end subroutine axis_head

    subroutine axis_foot(this, u)
        class(Axis) :: this
        integer, intent(in) :: u
        write (u,*) '\end{axis}'
    end subroutine axis_foot
end module mod_axis
