// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil(height / data.length);
var barWidth = function(d) {return d * width};
var y_position = function(d,i) {return i * barHeight};
var text_y_position = function(d,i) {return i * barHeight + 0.5 * barHeight};

svg.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('width', barWidth)
    .attr('height', barHeight)
    .attr('y', y_position)
    .attr('fill', 'steelblue');

var text = svg.selectAll("text")
  .data(data)
  .enter()
  .append("text");

var textLabels = text
                 .attr("x", barWidth)
                 .attr('y', text_y_position)
                 .text(function (d) { return d.toFixed(2); })
                 .attr("font-family", "sans-serif")
                 .attr("font-size", "20px")
.attr("fill", "red");