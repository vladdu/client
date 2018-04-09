module.exports = function(dialog, mockChoice) {
  dialog.showMessageBox = (options) => {
    return mockChoice//, options.button[mockChoice]]
  }
}
