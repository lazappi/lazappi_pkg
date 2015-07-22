#' Default Chunk Options
#' 
#' Set default chunk options for knitr
#' 
#' @details
#' Sets my preferred options for how knitr displays code and figures
#' 
#' @export
#' 
#' @examples
#' defaultChunkOpts()
#' 
#' @seealso
#' \code{\link[knitr]{opts_chunk}}
#' 
#' @keywords
#' rmarkdown
defaultChunkOpts <- function() {
       
    knitr::opts_chunk$set(
        # CODE EVALUATION
        eval           = TRUE,         # Whether to evaluate the chunk
                            
        # TEXT RESULTS
        collapse       = TRUE,         # Collapse results to single block
        echo           = FALSE,        # Whether to include source
        error          = FALSE,        # Continue after error
        include        = TRUE,         # Whether to include ouput
        message        = FALSE,        # Whether to include messages
        results        = "markup",     # How to display results
        split          = FALSE,        # Split output into multiple files
        strip.white    = TRUE,         # Remove start/end white lines
        warning        = FALSE,        # Whether to include warnings
        
        # CODE DECORATION
        background     = "F7F7F7",     # Chunk background in LaTeX
        comment        = "##",         # Prefix before output
        highlight      = TRUE,         # Highlight source
        indent         = "  ",         # Indent for markdown output
        prompt         = FALSE,        # Whether to add prompt character
        size           = "normalsize", # Font size for LaTeX output
        tidy           = FALSE,        # Tidy source using formatR
        
        # CACHE
        autodep        = TRUE,         # Automatically set dependencies
        cache          = FALSE,        # Whether to cache output
        cache.comments = FALSE,        # Comments change cache
        cache.lazy     = TRUE,         # Whether to lazy load cache
        cache.path     = "cache/",     # Prefix used for cache files
        cache.rebuild  = FALSE,        # Rebuild cache
        cache.vars     = NULL,         # Variable to save in cache
        
        # PLOTS
        # dev           =               # Plotting device
        dev.args       = NULL,         # Arguments to device
        dpi            = 72,           # Dots Per Inch
        external       = TRUE,         # Whether to externalise tikz
        fig.align      = "default",    # Output figure alignments
        fig.cap        = NULL,         # Figure caption for LaTeX
        fig.env        = "figure",     # LaTeX environment for figures
        fig.ext        = NULL,         # Extension for figure output
        fig.height     = 7,            # Figure height, in inches
        fig.keep       = "high",       # How plots are kept
        fig.lp         = "fig:",       # Prefix for figure labels
        fig.path       = "figs/",      # Prefix for figure filenames
        fig.pos        = "",           # Figure position arrangement
        fig.process    = NULL,         # Function to post-process figure file
        fig.retina     = 1,            # Adjustment for retina displays
        fig.scap       = NULL,         # Short caption for LaTeX
        fig.show       = "asis",       # How to show plots
        fig.showtext   = NULL,         # Whether to call show.text before plot
        fig.subcap     = NULL,         # Captions for subfigures
        fig.width      = 7,            # Figure width, in inches
        out.extra      = NULL,         # Extra options for figure output
        out.height     = NULL,         # Figure height in output file
        out.width      = NULL,         # Figure width in output file
        resize.height  = NULL,         # Resize height for tikz output in LaTeX 
        resize.width   = NULL,         # Resize width for tikz output in LaTeX
        sanitise       = FALSE,        # Whether to sanitise tikz
         
        # ANIMATION
        aniopts        = "controls, loop", # Extra options of animations
        interval       = 1,            # Seconds between animation frames
        
        # CODE CHUNK
        code           = NULL,         # Override code in chunk
        ref.label      = NULL,         # Inherit code from other chunks
        
        # CHILD DOCUMENTS
        child          = NULL,         # Filenames of child documents to input
        
        # LANGUAGE ENGINES
        engine         = "R",          # Language of the chunk
        
        # OPTION TEMPLATES
        opts.label     = NULL,         # The label of options in opts_template
        
        # EXTRACT SOURCE
        purl           = TRUE,         # Whether to include chunk in purl
        
        # OTHER
        R.options      = NULL          # Local R options
    )
    
}
