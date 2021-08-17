shinyjs.accordion = function() {
  
  $(document).ready(function() {
  $('.accordion').on('click',  function(e) {
  e.preventDefault();
  $(this).next('.panel').toggle();
  }) ;
  })
}

         