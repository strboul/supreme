
module_modal_dialog <- function(input, output, session, text) {
  showModal(modalDialog(
    title = "Important message",
    text
  ))
}

