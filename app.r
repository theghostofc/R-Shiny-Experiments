library(shiny)
library(rhandsontable)

change_hook <- "
    function(instance, td, row, col, prop, value, cellProperties) {
        var options = {
            '': [''],
            'Q1': ['', 'Jan', 'Feb', 'Mar'],
            'Q2': ['', 'Apr', 'May', 'Jun'],
            'Q3': ['', 'Jul ', 'Aug', 'Sep'],
            'Q4': ['', 'Oct', 'Nov', 'Dec']
        };
        Handsontable.hooks.add('afterChange', function(changes, source) {
            if (source === 'edit' || source === 'undo' || source === 'autofill' || source === 'paste') {
                row = changes[0][0];
                col = changes[0][1];
                oldval = changes[0][2];
                newval = changes[0][3];
                if (oldval !== newval) {
                    if(col == 0) {
                        this.setDataAtCell(row, col + 1, '');
                    }
                }
            }
        }, instance);
        Handsontable.hooks.add('afterOnCellMouseDown', function(event, coords, element) {
            if(coords.row > -1) {
                if(coords.col == 1) {
                    var prevCell = this.getCell(coords.row, coords.col - 1);
                    instance.setCellMeta(coords.row, coords.col, 'source', options[prevCell.textContent])
                }
            }
        }, instance);
        Handsontable.renderers.TextRenderer.apply(this, arguments);
    }
    "

ui <- div(width = 300, rHandsontableOutput(outputId = "hTable"))
server <- function(input, output, session) {

df <- data.frame(Quarter = c('Q1', 'Q2'),
        Month = c('Jan', 'Apr'),
        stringsAsFactors = FALSE)

hTable <- reactiveVal(df)

observeEvent(input$hTable, {
    withProgress(message = "Saving changes to database...", value = 0.5, {
        Sys.sleep(1)
        incProgress(1, detail = "done")
        input_hTable <- hot_to_r(input$hTable)
        hTable(input_hTable)
    })
})

output$hTable <- renderRHandsontable({
    rhandsontable(hTable(), stretchH = "all", height = 300) %>%
        hot_col(col = "Quarter", type = "dropdown", source = c('Q1', 'Q2', 'Q3', 'Q4'), renderer = change_hook) %>%
        hot_col(col = "Month", type = "dropdown", strict = FALSE)
    })
}
shinyApp(ui, server)
