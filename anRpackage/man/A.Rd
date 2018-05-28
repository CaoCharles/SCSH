\name{A}
\alias{A}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("A")}
\format{
  The format is:
List of 9
 $ data       :function (..., list = character(), package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
    envir = .GlobalEnv)  
 $ layers     :List of 2
  ..$ :Classes 'LayerInstance', 'Layer', 'ggproto' <ggproto object: Class LayerInstance, Layer>
    aes_params: list
    compute_aesthetics: function
    compute_geom_1: function
    compute_geom_2: function
    compute_position: function
    compute_statistic: function
    data: waiver
    draw_geom: function
    finish_statistics: function
    geom: <ggproto object: Class GeomPoint, Geom>
        aesthetics: function
        default_aes: uneval
        draw_group: function
        draw_key: function
        draw_layer: function
        draw_panel: function
        extra_params: na.rm
        handle_na: function
        non_missing_aes: size shape colour
        optional_aes: 
        parameters: function
        required_aes: x y
        setup_data: function
        use_defaults: function
        super:  <ggproto object: Class Geom>
    geom_params: list
    inherit.aes: TRUE
    layer_data: function
    map_statistic: function
    mapping: uneval
    position: <ggproto object: Class PositionIdentity, Position>
        compute_layer: function
        compute_panel: function
        required_aes: 
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Position>
    print: function
    show.legend: NA
    stat: <ggproto object: Class StatIdentity, Stat>
        aesthetics: function
        compute_group: function
        compute_layer: function
        compute_panel: function
        default_aes: uneval
        extra_params: na.rm
        finish_layer: function
        non_missing_aes: 
        parameters: function
        required_aes: 
        retransform: TRUE
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Stat>
    stat_params: list
    subset: NULL
    super:  <ggproto object: Class Layer> 
  ..$ :Classes 'LayerInstance', 'Layer', 'ggproto' <ggproto object: Class LayerInstance, Layer>
    aes_params: list
    compute_aesthetics: function
    compute_geom_1: function
    compute_geom_2: function
    compute_position: function
    compute_statistic: function
    data: waiver
    draw_geom: function
    finish_statistics: function
    geom: <ggproto object: Class GeomDensity2d, GeomPath, Geom>
        aesthetics: function
        default_aes: uneval
        draw_group: function
        draw_key: function
        draw_layer: function
        draw_panel: function
        extra_params: na.rm
        handle_na: function
        non_missing_aes: 
        optional_aes: 
        parameters: function
        required_aes: x y
        setup_data: function
        use_defaults: function
        super:  <ggproto object: Class GeomPath, Geom>
    geom_params: list
    inherit.aes: TRUE
    layer_data: function
    map_statistic: function
    mapping: uneval
    position: <ggproto object: Class PositionIdentity, Position>
        compute_layer: function
        compute_panel: function
        required_aes: 
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Position>
    print: function
    show.legend: NA
    stat: <ggproto object: Class StatDensity2d, Stat>
        aesthetics: function
        compute_group: function
        compute_layer: function
        compute_panel: function
        default_aes: uneval
        extra_params: na.rm
        finish_layer: function
        non_missing_aes: 
        parameters: function
        required_aes: x y
        retransform: TRUE
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Stat>
    stat_params: list
    subset: NULL
    super:  <ggproto object: Class Layer> 
 $ scales     :Classes 'ScalesList', 'ggproto' <ggproto object: Class ScalesList>
    add: function
    clone: function
    find: function
    get_scales: function
    has_scale: function
    input: function
    n: function
    non_position_scales: function
    scales: list
    super:  <ggproto object: Class ScalesList> 
 $ mapping    : list()
 $ theme      :List of 2
  ..$ title           :List of 11
  .. ..$ family       : NULL
  .. ..$ face         : chr "bold"
  .. ..$ colour       : NULL
  .. ..$ size         : NULL
  .. ..$ hjust        : NULL
  .. ..$ vjust        : NULL
  .. ..$ angle        : NULL
  .. ..$ lineheight   : NULL
  .. ..$ margin       : NULL
  .. ..$ debug        : NULL
  .. ..$ inherit.blank: logi FALSE
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..$ panel.background:List of 5
  .. ..$ fill         : chr "grey92"
  .. ..$ colour       : chr "black"
  .. ..$ size         : num 2
  .. ..$ linetype     : NULL
  .. ..$ inherit.blank: logi FALSE
  .. ..- attr(*, "class")= chr [1:2] "element_rect" "element"
  ..- attr(*, "class")= chr [1:2] "theme" "gg"
  ..- attr(*, "complete")= logi FALSE
  ..- attr(*, "validate")= logi FALSE
 $ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto' <ggproto object: Class CoordCartesian, Coord>
    aspect: function
    distance: function
    expand: TRUE
    is_linear: function
    labels: function
    limits: list
    range: function
    render_axis_h: function
    render_axis_v: function
    render_bg: function
    render_fg: function
    train: function
    transform: function
    super:  <ggproto object: Class CoordCartesian, Coord> 
 $ facet      :Classes 'FacetNull', 'Facet', 'ggproto' <ggproto object: Class FacetNull, Facet>
    compute_layout: function
    draw_back: function
    draw_front: function
    draw_labels: function
    draw_panels: function
    finish_data: function
    init_scales: function
    map: function
    map_data: function
    params: list
    render_back: function
    render_front: function
    render_panels: function
    setup_data: function
    setup_params: function
    shrink: TRUE
    train: function
    train_positions: function
    train_scales: function
    vars: function
    super:  <ggproto object: Class FacetNull, Facet> 
 $ plot_env   :<environment: R_GlobalEnv> 
 $ labels     :List of 5
  ..$ title : chr " Waiting time vs Eruption time of old faithful geyser"
  ..$ x     : chr "Eruptions time (min)"
  ..$ y     : chr "Time to next eruption (min)"
  ..$ colour: chr "colour"
  ..$ size  : chr "0.5"
 - attr(*, "class")= chr [1:2] "gg" "ggplot"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(A)
## maybe str(A) ; plot(A) ...
}
\keyword{datasets}
