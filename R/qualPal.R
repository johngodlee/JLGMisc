#' Qualitative colour palettes
#' @return named list of qualitative colour palettes as vectors of hex-codes
#' 
#' @details \code{color_brewer_*} come from \url{https://colorbrewer2.org/}.
#'     \code{seaborn_*} come from \url{https://seaborn.pydata.org/}.
#'     \code{carto_colors_*} come from \url{https://carto.com/carto-colors/}.
#'     \code{solarized_accent} comes from \url{https://ethanschoonover.com/solarized/}.
#'     \code{coloropt_*} come from \url{http://tsitsul.in/blog/coloropt/}.
#'     \code{rainbow_12bit} comes from \url{https://iamkate.com/data/12-bit-rainbow/}.
#'     \code{oksolar} comes from \url{https://meat.io/oksolar}.
#'     \code{xgfs} comes from \url{http://tsitsul.in/blog/coloropt/}.
#'     \code{nightfly} comes from \url{https://github.com/bluz71/vim-nightfly-colors}.
#'     Other palettes are original.
#' 
#' @examples
#' qualPal()[[1]]
#' 
#' @export
#' 
qualPal <- function() {
  list(
    red_blue = c("#2166AC", "#B2182B"),
    movie_4 = c("#117733", "#b58900", "#855C75", "#ED645A"),
    highlighter = c("#bbdf20", "#20dfa3", "#4420df", "#df205c"),
    color_brewer_set1 = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999"),
    color_brewer_dark2 = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666"), 
    color_brewer_set3 = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5"),
    seaborn_default = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"),
    seaborn_muted = c("#4878D0", "#EE854A", "#6ACC64", "#D65F5F", "#956CB4", "#8C613C", "#DC7EC0", "#797979", "#D5BB67", "#82C6E2"),
    seaborn_colorblind = c("#0173B2", "#DE8F05", "#029E73", "#D55E00", "#CC78BC", "#CA9161", "#FBAFE4", "#949494", "#ECE133", "#58B4E9"),
    seaborn_deep = c("#4C72B0", "#DD8452", "#55A868", "#C44E52", "#8172B3", "#937860", "#DA8BC3", "#8C8C8C", "#CCB974", "#64B5CD"),
    solarized_accent = c("#b58900", "#cb4b16", "#dc322f", "#d33682", "#6c71c4", "#268bd2", "#2aa198", "#859900"),
    oksolar = c("#ac8300", "#d56500", "#f23749", "#dd459d", "#7d80d1", "#2b90d8",
      "#259d94", "#819500"),
    carto_colors_antique = c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C"),
    carto_colors_bold = c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A", "#E68310", "#008695", "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99"),
    carto_colors_pastel = c("#66C5CC", "#F6CF71", "#F89C74", "#DCB0F2", "#87C55F", "#9EB9F3", "#FE88B1", "#C9DB74", "#8BE0A4", "#B497E7", "#D3B484", "#B3B3B3"),
    carto_colors_prism = c("#5F4690", "#1D6996", "#38A6A5", "#0F8554", "#73AF48", "#EDAD08", "#E17C05", "#CC503E", "#94346E", "#6F4070", "#994E95", "#666666"),
    carto_colors_safe = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
    carto_colors_vivid = c("#E58606", "#5D69B1", "#52BCA3", "#99C945", "#CC61B0", "#24796C", "#DAA51B", "#2F8AC4", "#764E9F", "#ED645A", "#CC3A8E", "#A5AA99"),
    coloropt_normal6 = c("#4053D3", "#DDB310", "#B51D14", "#00BEFF", "#FB49B0", "#00B25D", "#CACACA"),
    coloropt_bright6 = c("#EFE645", "#E935A1", "#00E3FF", "#E1562C", "#537EFF", "#00CB85", "#EEEEEE"),
    coloropt_dark6 = c("#005900", "#000078", "#490D00", "#8A034F", "#005A8A", "#443500", "#585858"),
    coloropt_fancy6 = c("#56641A", "#C0AFFB", "#E6A176", "#00678A", "#984464", "#5ECCAB", "#CDCDCD"),
    coloropt_tarnish6 = c("#274D52", "#C7A2A6", "#818B70", "#604E3C", "#8C9FB7", "#796880", "#C0C0C0"),
    coloropt_normal12 = c("#EBAC23", "#B80058", "#008CF9", "#006E00", "#00BBAD", "#D163E6", "#B24502", "#FF9287", "#5954D6", "#00C6F8", "#878500", "#00A76C", "#BDBDBD"),
    rainbow_12bit = c("#881177", "#aa3355", "#cc6666", "#ee9944", "#eedd00", "#99dd55", "#44dd88", "#22ccbb", "#00bbcc", "#0099cc", "#3366bb", "#663399"),
    xgfs_normal6 = c("#4053D3", "#DDB310", "#B51D14", "#00BEFF", "#FB49B0", "#00B25D", "#CACACA"),
    xgfs_normal12 = c("#EBAC23", "#B80058", "#008CF9", "#006E00", "#00BBAD", "#D163E6", "#B24502", "#FF9287", "#5954D6", "#00C6F8", "#878500", "#00A76C", "#BDBDBD"),
    xgfs_bright6 = c("#EFE645", "#E935A1", "#00E3FF", "#E1562C", "#537EFF", "#00CB85", "#EEEEEE"),
    xgfs_dark6 = c("#005900", "#000078", "#490D00", "#8A034F", "#005A8A", "#443500", "#585858"),
    xgfs_fancy6 = c("#56641A", "#C0AFFB", "#E6A176", "#00678A", "#984464", "#5ECCAB", "#CDCDCD"),
    xgfs_tarnish6 = c("#274D52", "#C7A2A6", "#818B70", "#604E3C", "#8C9FB7", "#796880", "#C0C0C0"),
    nightfly = c("#fc514e", "#a1cd5e", "#e3d18a", "#82aaff", "#c792ea", "#7fdbca", "#a1aab8"),
    nightfly_bright = c("#7c8f8f", "#ff5874", "#21c7a8", "#ecc48d", "#82aaff", "#ae81ff", "#7fdbca", "#d6deeb")
  )
}
