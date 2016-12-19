module default_styles
    use mod_plotattr
    implicit none
contains
    function style_no_markers()
        type(PlotAttr), allocatable :: style_no_markers(:)

        allocate(style_no_markers(1))
        style_no_markers(1) = PlotAttr('no markers', '')
    end function style_no_markers

    function style_only_markers()
        type(PlotAttr), allocatable :: style_only_markers(:)

        allocate(style_only_markers(1))
        style_only_markers(1) = PlotAttr('only marks', '')
    end function style_only_markers
end module default_styles
