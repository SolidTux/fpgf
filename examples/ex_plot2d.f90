program ex_plot2d
    use fpgf
    implicit none

    type(Plot2D) :: testplot
    real :: x(3) = (/1, 2, 3/)
    real :: y(3) = (/1, 2, 0/)
    real :: x2(3) = (/1, 2, 3/)
    real :: y2(3) = (/2, 1, 1/)

    testplot = Plot2D("test.pgf")
    testplot%axis%xlabel = "$x$"
    testplot%axis%ylabel = "$y$"
    call testplot%add(x, y, style_only_markers())
    call testplot%add(x2, y2)
    call testplot%addAttr(2, NoMarker())
    call testplot%write()
end program ex_plot2d
