export function registerCustomClickHandler(el, x, data) {
  if (!data.input_id || !data.off_event) return;

  let selectedRegions = [];

  function updateShiny() {
    // just in case of some weird plotly behavior
    const uniqueRegions = [...new Set(selectedRegions)];
    Shiny.setInputValue(data.input_id, uniqueRegions);
  }

  function reset() {
    selectedRegions = [];
    updateShiny();
  }

  reset(); // reset on update

  el.on('plotly_click', (d) => {
    if (d.points.length > 0) {
      const point = d.points[0];
      const label = point.data.key[0];

      if (label) {
        if (d.event.shiftKey) {
          selectedRegions.push(label);
        } else {
          selectedRegions = [label];
        }

        updateShiny();
      }
    }
  });

  el.on(data.off_event, () => {
    reset();
  });

  $(el).closest('.map-container').on('dblclick', () => {
    // force event when clicking on container
    el.emit(data.off_event, null);
  });
}
