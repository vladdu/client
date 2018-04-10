module.exports = function(dialog, mockChoice, mockSavePath) {
  dialog.showMessageBox = (options) => {
    return mockChoice
  }

  dialog.showSaveDialog = (options) => {
    return mockSavePath
  }
}
