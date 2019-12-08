
module_modal_dialog_server <- function(input, output, session, text) {
  showModal(modalDialog(
    title = "Important message",
    text
  ))
}

