Shiny.addCustomMessageHandler("toggleChecker", function(message) {
  var el = document.getElementById(message.id);
  var eldiv = document.getElementById(message.divId);
  
   if (!el || !eldiv) {
    console.warn("toggleChecker: element not found", message.id, message.divId);
    return;
  }
  
  // Determine the color based on validation status
  var iconColor;

  if (!message.complete) {
    iconColor = "grey";  // Red if validation failed
  } else {
    iconColor = "black";  // Black if validation passed
  }
  
  // Apply the color to the icon
  document.getElementById(message.divId).style.color = iconColor;
  
  // If validation failed, set the error message as tooltip
  if (!message.complete) {
    $(el).removeClass('fa fa-check').addClass('fa fa-exclamation-circle');
    eldiv.title = message.error_message;  // Set the error message in the tooltip
  } 
  // If validation passed, remove the error and display checkmark
  else {
    $(el).removeClass('fa fa-exclamation-circle').addClass('fa fa-check');
    eldiv.removeAttribute('title');  // Remove tooltip
  }
});
