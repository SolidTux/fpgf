module mod_plot2d
    use filestructure
    use mod_axis
    implicit none
    private

    type :: Data2D
        real, allocatable :: x(:), y(:)
    contains
        procedure :: write => data2d_write
    end type Data2D

    type, public :: Plot2D
        integer :: n
        character(len=100) :: filename
        type(Axis) :: axis
        type(Data2D), allocatable :: data2d(:)
    contains
        procedure :: add => plot2d_add
        procedure :: write => plot2d_write
        procedure :: close => plot2d_close
    end type Plot2D

    interface Plot2D
        module procedure new_plot2d
    end interface Plot2D
contains
    subroutine data2d_write(this, u)
        class(Data2D), intent(in) :: this
        integer, intent(in) :: u

        integer :: i

        write (u,*) '\addplot'
        write (u,*) 'table{'
        do i = 1,min(size(this%x), size(this%y))
            write (u,*) this%x(i), ',', this%y(i)
        end do
        write (u,*) '};'
    end subroutine data2d_write

    subroutine plot2d_write(this)
        class(Plot2D), intent(in) :: this

        integer :: u, i

        open (newunit=u, file=this%filename)
        call write_head(u)
        call this%axis%foot(u)

        do i = 1,size(this%data2d)
            call this%data2d(i)%write(u)
        end do

        call this%axis%head(u)
        call write_foot(u)
        close (u)
    end subroutine plot2d_write

    subroutine plot2d_add(this, x, y)
        class(Plot2D), intent(inout) :: this
        real, intent(in) :: x(:), y(:)

        if (allocated(this%data2d)) then
        else
            allocate(this%data2d(1))
            allocate(this%data2d(1)%x(size(x)))
            allocate(this%data2d(1)%y(size(y)))
            this%data2d(1)%x = x
            this%data2d(1)%y = y
        end if
    end subroutine plot2d_add

    subroutine plot2d_close(this)
        class(Plot2D), intent(inout) :: this

        if (allocated(this%data2d)) deallocate(this%data2d)
    end subroutine plot2d_close

    function new_plot2d(fn)
        character(len=*), intent(in) :: fn
        type(Plot2D) :: new_plot2d

        new_plot2d%filename = fn
    end function new_plot2d
end module mod_plot2d
