module filestructure
    implicit none

contains
    subroutine write_head(u)
        integer, intent(in) :: u
        write (u,*) '\begin{tikzpicture}'
    end subroutine write_head

    subroutine write_foot(u)
        integer, intent(in) :: u
        write (u,*) '\end{tikzpicture}'
    end subroutine write_foot
end module filestructure
