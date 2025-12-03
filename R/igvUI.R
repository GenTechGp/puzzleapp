igvInputUI <- function(ns) {
  tagList(
    fluidRow(
      column(2, numericInput(ns("igv_max_window"), "Max window size:", 10000, 0)),
      column(1, numericInput(ns("igv_flanking"), "Flanking size:", 200, 0)),
      column(2, textInput(ns("igv_var_id"), "Variant ID:", value = "")),
      column(1, actionButton(ns("search_button"), "Search"), style = "margin-top: 25px;"),
      column(1, textInput(ns("sort_tag_input"), "Tag:", value = "HP", placeholder = "e.g., HP")),
      column(1, actionButton(ns("sort_tag_button"), "Sort by Tag"), style = "margin-top: 25px;"),
      #empty column for spacing
      column(1,),
      column(2, textInput(ns("genome_coords"), "Genome coordinates:", placeholder = "chr:start-end or chr:pos")),
      column(1, actionButton(ns("update_button"), "Show"), style = "margin-top: 25px;"),
      tags$script(HTML(sprintf("
        $(document).on('keypress', function(e) {
          if(e.which == 13 && $('#%s').is(':focus')) {
            $('#%s').click();
          }
        });
      ", ns("igv_var_id"), ns("search_button")))),
      tags$script(HTML(sprintf("
        $(document).on('keypress', function(e) {
          if(e.which == 13 && $('#%s').is(':focus')) {
            $('#%s').click();
          }
        });
      ", ns("genome_coords"), ns("update_button")))),

      # Client-side JS: button handler and sorting helpers
      tags$script(HTML(sprintf("
        (function(){
          // Helper: get igvBrowser from this module instance
          function getIgvBrowser(){
            var el = document.querySelector('[id$=\"%s\"]');
            if (!el || !el.igvBrowser) throw new Error('igvBrowser not found for %s');
            return el.igvBrowser;
          }

          function sortBamByTagAtAlignmentCenter(bamTrack, tag, direction){
            if (!bamTrack || (bamTrack.type !== 'alignment' && !(bamTrack.config && bamTrack.config.type === 'alignment'))) {
              console.warn('Track is not alignment:', bamTrack && bamTrack.name);
              return;
            }
            var at = bamTrack.alignmentTrack;
            var ac = (at && at.featureSource && at.featureSource.alignmentContainer) || (at && at.alignmentContainer) || null;
            if (!ac || ac.start == null || ac.end == null || !ac.chr) {
              console.warn('AlignmentContainer not available. Zoom to a region with reads.');
              return;
            }
            var chr = ac.chr;
            var pos = Math.floor((ac.start + ac.end) / 2);
            bamTrack.sort({ chr: chr, position: pos, option: 'TAG', tag: tag, direction: direction });
            var browser = bamTrack.browser;
            var tv = (browser && browser.trackViews) ? browser.trackViews.find(function(v){ return v.track === bamTrack; }) : null;
            if (tv && typeof tv.repaint === 'function') tv.repaint();
            console.log('Sorted \"' + (bamTrack.name || bamTrack.id) + '\" by tag ' + tag + ' at ' + chr + ':' + pos + ' (' + direction + ')');
          }

          function sortAllBamsByTag(tag, direction){
            var browser;
            try { browser = getIgvBrowser(); } catch(e){ console.error(e.message); return; }
            var bamTVs = (browser.trackViews || []).filter(function(tv){
              var t = tv.track;
              return t && (t.type === 'alignment' || (t.config && t.config.type === 'alignment'));
            });
            bamTVs.forEach(function(tv){
              sortBamByTagAtAlignmentCenter(tv.track, tag, direction);
            });
            console.log('Requested sort-by-tag ' + tag + ' (' + direction + ') for ' + bamTVs.length + ' alignment track(s).');
          }

          // Wire up the button: read tag from input, default to 'HP' if empty
          $(document).on('click', '#%s', function(){
            var tagVal = ($('#%s').val() || 'HP').trim();
            sortAllBamsByTag(tagVal, 'ASC');
          });
        })();
      ", ns("igvShiny_0"), ns("igvShiny_0"), ns("sort_tag_button"), ns("sort_tag_input"))))
    )
  )
}

#' IGV Tab UI
#' @param id Module ID
#' @param tab_label Label for the tab
#' @return Shiny UI object
#' @export
#' @import shiny
igvUI <- function(id, tab_label) {
  ns <- NS(id)
  tagList(
    igvInputUI(ns),
    igvShinyOutput(ns("igvShiny_0"))
  )
}
