export function register_custom_click_handler(el, x, data) {
  if (!data.input_id || !data.off_event) return;
  
  function update_shiny(value){
    Shiny.setInputValue(data.input_id, value);
  };
  
  el.on('plotly_click', function(d) {
    //TODO; figure out how to capture all selected regions
    if (d.points.length > 0){
      let point = d.points[0];
      let label = point.data.key[0];
      
      update_shiny(label);
    }
  });
  
  el.on(data.off_event, function(){
    update_shiny(null);
  })
}
