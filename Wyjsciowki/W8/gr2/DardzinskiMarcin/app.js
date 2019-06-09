// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil((height - 30) / data.length);
var scale = d3.scaleLinear()
  .range([0, width])
  .domain([0, 1]).nice();

var xAxis = d3.axisBottom()
  .scale(scale);


svg.selectAll('rect')
  .data(data)
  .enter().append('rect')
  .attr('width', function (d) { return d * width; })
  .attr('height', barHeight)
  .attr('y', function (d, i) { return i * barHeight; })
  .attr('fill', 'steelblue');



var text = svg.selectAll("text")
  .data(data)
  .enter()
  .append("text");

var textLabels = text
  .attr("x", function (d) { return d * width / 2; })
  .attr('y', function (d, i) { return i * barHeight + 50; })
  .text(function (d) { return d; })
  .attr("font-family", "sans-serif")
  .attr("font-size", "20px")
  .attr("fill", "red");

svg
  .append('g')
  .attr("transform", "translate(0," + data.length * barHeight + ")")
  .call(xAxis);