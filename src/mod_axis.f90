module mod_axis
    implicit none
    private

    type, public :: Axis
        character(:), allocatable :: xlabel
        character(:), allocatable :: ylabel
    contains
        procedure :: head => axis_head
        procedure :: foot => axis_foot
        procedure :: close => axis_close
    end type Axis
contains
    subroutine axis_head(this, u)
        class(Axis), intent(in) :: this
        integer, intent(in) :: u
        write (u,*) '\begin{axis}['
        write (u,*) 'xlabel=',trim(this%xlabel),','
        write (u,*) 'ylabel=',trim(this%ylabel),''
        write (u,*) ']'
    end subroutine axis_head

    subroutine axis_foot(this, u)
        class(Axis), intent(in) :: this
        integer, intent(in) :: u
        write (u,*) '\end{axis}'
    end subroutine axis_foot

    subroutine axis_close(this)
        class(Axis), intent(inout) :: this

        if (allocated(this%xlabel)) deallocate(this%xlabel)
        if (allocated(this%ylabel)) deallocate(this%ylabel)
    end subroutine axis_close
end module mod_axis
