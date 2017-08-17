const verticalLinePlugin = {
  getLinePosition: function (chart, pointIndex) {
    const meta = chart.getDatasetMeta(0); // first dataset is used to discover X coordinate of a point
    const data = meta.data;
    return data[pointIndex]._model.x;
  },
  renderVerticalLine: function (chartInstance, pointIndex) {
    const lineLeftOffset = this.getLinePosition(chartInstance, pointIndex);
    const scale = chartInstance.scales['y-axis-0'];
    const context = chartInstance.chart.ctx;

    // render vertical line
    context.beginPath();
    context.strokeStyle = '#ff0000';
    context.moveTo(lineLeftOffset, scale.top);
    context.lineTo(lineLeftOffset, scale.bottom);
    context.stroke();

    // write label
    context.save()
    context.rotate(-Math.PI/2);
    //context.moveTo(-scale.top, lineLeftOffset);
    context.fillStyle = "#ff0000";
    context.textAlign = 'center';
    context.fillText('Bought', -(scale.top - scale.bottom / 2 + scale.bottom), lineLeftOffset);
    context.restore();
  }
}

export default verticalLinePlugin;
