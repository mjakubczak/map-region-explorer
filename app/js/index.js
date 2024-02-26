export function register_custom_click_handler(el, x, data) {
  if (!data.input_id || !data.off_event) return;
  
  let selected_regions = [];
  
  function update_shiny(){
    let unique_regions = [...new Set(selected_regions)]; // just in case of some weird plotly behavior
    Shiny.setInputValue(data.input_id, unique_regions);
  };
  
  function reset(){
    selected_regions = [];
    update_shiny(); 
  }
  
  reset(); // reset on update
  
  el.on('plotly_click', function(d) {
    if (d.points.length > 0){
      let point = d.points[0];
      let label = point.data.key[0];
      
      if (label){
        selected_regions.push(label);
        update_shiny();
      }
    }
  });
  
  el.on(data.off_event, function(){
    reset();
  });
  
  $(el).closest('.map-container').on('dblclick', function(evt){
    // force event when clicking on container
    el.emit(data.off_event, null);
  });
}
