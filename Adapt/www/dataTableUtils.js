//function get_id(clicked_id) {
//  Shiny.setInputValue('expr_row',
//                      clicked_id.split('_')[1],
//                      {priority: 'event'});
//}

function set_row_id(clicked_id) {
  var el = document.getElementById(clicked_id);
  if (el.checked) {
      Shiny.setInputValue('expr_row',
                      clicked_id.split('_')[1],
                      {priority: 'event'});
  } else {
      Shiny.setInputValue('expr_row',
                      null,
                      {priority: 'event'});
  }

}


// https://stackoverflow.com/questions/43377773/if-check-box-checked-disable-other-if-unchecked-enable-all-javascript 

function ckChange(el) {
  var ckName = document.getElementsByName(el.name);
  for (var i = 0, c; c = ckName[i]; i++) {
    c.disabled = !(!el.checked || c === el);
  }
}

function checkboxProperties(el) {
  //get_id(el.id) ;
  set_row_id(el.id) ;
  ckChange(el) ;
}