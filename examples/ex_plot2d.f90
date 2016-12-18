program ex_plot2d
    use fpgf
    implicit none

    type(Plot2D) :: testplot
    real :: x(3) = (/1, 2, 3/)
    real :: y(3) = (/1, 2, 0/)

    testplot = Plot2D("test.pgf")
    testplot%axis%xlabel = "x"
    testplot%axis%ylabel = "$y\ /\ \int$"
    call testplot%add(x,y)
    call testplot%write()
end program ex_plot2d
