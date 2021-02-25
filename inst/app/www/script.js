

$( document ).ready(function() {
  
  Shiny.addCustomMessageHandler('updateButtons', function(arg) {
    $(':button_submit').prop('disabled', arg.value);
  });
  
  Shiny.addCustomMessageHandler('updateSelectedRowButton', function(arg) {
    $('#' + arg.button).prop('disabled', arg.value);
  });
  
  Shiny.addCustomMessageHandler('updateButton', function(arg) {
    Shiny.setInputValue(arg.button, null);
  });
  
  Shiny.addCustomMessageHandler('submitAlert', function(arg) {
    alert(arg.message);
  });
  
});
