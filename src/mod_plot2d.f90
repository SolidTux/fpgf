module mod_plot2d
    use filestructure
    use mod_plotattr
    use mod_axis
    implicit none
    private

    type :: Data2D
        real, allocatable :: x(:), y(:)
        class(PlotAttr), allocatable :: attrs(:)
    contains
        procedure :: write => data2d_write
    end type Data2D

    type, public :: Plot2D
        integer :: n
        character(:), allocatable :: filename
        type(Axis) :: axis
        type(Data2D), allocatable :: data2d(:)
    contains
        procedure :: add => plot2d_add
        procedure :: addAttr => plot2d_addattr
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

        write (u,*) '\addplot+['
        if (allocated(this%attrs)) then
            do i = 1,size(this%attrs)
                call this%attrs(i)%write(u)
            end do
        end if
        write (u,*) '] coordinates {'
        do i = 1,min(size(this%x), size(this%y))
            write (u,*) '(',this%x(i), ',', this%y(i), ')'
        end do
        write (u,*) '};'
    end subroutine data2d_write

    subroutine plot2d_addattr(this, id, attr)
        class(Plot2D), intent(inout) :: this
        integer, intent(in) :: id
        class(PlotAttr), intent(in) :: attr

        type(PlotAttr), allocatable :: attrtmp(:)

        if (.not. allocated(this%data2d)) then
            stop "ERROR: Trying to add attribute, but no plots present"
        end if

        if (size(this%data2d) < id) then
            stop "ERROR: Trying to add attribute to not existent plot."
        end if

        if (allocated(this%data2d(id)%attrs)) then
            allocate(attrtmp(size(this%data2d(id)%attrs)+1))
            attrtmp(:size(this%data2d(id)%attrs)) = this%data2d(id)%attrs
            attrtmp(size(attrtmp)) = attr
            deallocate(this%data2d(id)%attrs)
            allocate(this%data2d(id)%attrs, source=attrtmp)
            deallocate(attrtmp)
        else
            allocate(this%data2d(id)%attrs(1))
            this%data2d(id)%attrs(1) = attr
        end if
    end subroutine plot2d_addattr

    subroutine plot2d_write(this)
        class(Plot2D), intent(in) :: this

        integer :: u, i

        open (newunit=u, file=this%filename)
        call write_head(u)
        call this%axis%head(u)

        do i = 1,size(this%data2d)
            call this%data2d(i)%write(u)
        end do

        call this%axis%foot(u)
        call write_foot(u)
        close (u)
    end subroutine plot2d_write

    subroutine plot2d_add(this, x, y, attrs)
        class(Plot2D), intent(inout) :: this
        class(PlotAttr), intent(in), optional :: attrs(:)
        real, intent(in) :: x(:), y(:)

        type(Data2D), allocatable :: datatmp(:)

        if (allocated(this%data2d)) then
            allocate(datatmp(size(this%data2d)+1))
            datatmp(:size(this%data2d)) = this%data2d
            datatmp(size(datatmp))%x = x
            datatmp(size(datatmp))%y = y
            if (present(attrs)) then
                allocate(datatmp(size(this%data2d)+1)%attrs, source=attrs)
            end if
            deallocate(this%data2d)
            allocate(this%data2d, source=datatmp)
            deallocate(datatmp)
        else
            allocate(this%data2d(1))
            allocate(this%data2d(1)%x, source=x)
            allocate(this%data2d(1)%y, source=y)
            if (present(attrs)) then
                allocate(this%data2d(1)%attrs, source=attrs)
            end if
        end if
    end subroutine plot2d_add

    subroutine plot2d_close(this)
        class(Plot2D), intent(inout) :: this

        if (allocated(this%data2d)) deallocate(this%data2d)
        if (allocated(this%filename)) deallocate(this%filename)
    end subroutine plot2d_close

    function new_plot2d(fn)
        character(len=*), intent(in) :: fn
        type(Plot2D) :: new_plot2d

        new_plot2d%filename = fn
    end function new_plot2d
end module mod_plot2d
