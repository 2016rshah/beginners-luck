const verticalLinePlugin = {
  getLinePosition: function (chart, pointIndex) {
    const meta = chart.getDatasetMeta(0); // first dataset is used to discover X coordinate of a point
    const data = meta.data;
    return data[pointIndex]._model.x;
  },
  renderVerticalLine: function (chartInstance, pointer, active) {
    const type = pointer[1];
    if ((type === 'bought' && active.bought === false) || (type === 'sold' && active.sold === false)) {
      return
    }
    const pointIndex = pointer[0];
    const color = type === 'sold' ? '#b667a0' : '#8cbaf0'
    const lineLeftOffset = this.getLinePosition(chartInstance, pointIndex);
    const scale = chartInstance.scales['y-axis-0'];
    const context = chartInstance.chart.ctx;

    // render vertical line
    context.beginPath();
    context.strokeStyle = color;
    context.moveTo(lineLeftOffset, scale.top);
    context.lineTo(lineLeftOffset, scale.bottom);
    context.stroke();
  }
}

export default verticalLinePlugin;
