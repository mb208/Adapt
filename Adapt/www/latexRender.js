
// https://stackoverflow.com/questions/9642205/how-to-force-a-script-reload-and-re-execute
function reloadJs(src) {
  src = $('script[src$="' + src + '"]').attr("src");
  $('script[src$="' + src + '"]').remove();
  $('<script/>').attr('src', src).appendTo('head');
}

/*
$(document).ready(function() {
  $('#gen_var').on('click', function(){
    reloadJs('https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js');
  }) ;
});*/