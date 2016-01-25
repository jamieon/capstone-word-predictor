function focusOnTextBox() {
    textbox = document.getElementById("sentence")
    textbox.focus();
    textbox.selectionStart = textbox.selectionEnd = 10000;
}
