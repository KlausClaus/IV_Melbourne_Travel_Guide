library(shiny)

ui <- fluidPage(
  tags$head(
    tags$script(
      HTML('
                $(document).ready(function() {
                    // 使用标签标题选择要隐藏的标签
                    var tabToHide = $("a[data-value=\'Hidden Tab\']");
                    // 隐藏标签
                    tabToHide.hide();
                });
            ')
    )
  ),
  navbarPage("NavbarPage Example", id = "navbar",
             tabPanel("Visible Tab",
                      h1("This is the visible tab")
             ),
             tabPanel("Hidden Tab",
                      h1("This tab is hidden")
             )
  )
)

server <- function(input, output, session) {
  # 无需服务器端代码
}

shinyApp(ui, server)