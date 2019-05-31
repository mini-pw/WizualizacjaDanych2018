// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//


var barHeight = Math.ceil(height / data.length);

var y = d3.scaleLinear()
    .range([20, width])

var x = d3.scaleBand()
  .domain(data.map(function(d, i) { return i; }))
  .range([ 0, height ]). padding(0.1);

var yAxis = d3.axisBottom(y);
var xAxis = d3.axisLeft(x);

svg.append("g").call(yAxis);
svg.append("g").call(xAxis);

svg.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('width', function(d) { return y(d); })
    .attr('height', x.bandwidth())
    .attr('y', function(d, i) { return x(i); })
	.attr("x", 20)
    .attr('fill', 'steelblue');

var text = svg.append("g").selectAll("text")
  .data(data)
  .enter()
  .append("text");

var textLabels = text
                 .attr("x", function(d) { return y(d); })
    			.attr('y', function(d, i) { return x(i) + x.bandwidth()/2  + 10; })
                 .text(function (d) { return d; })
                 .attr("font-family", "sans-serif")
                 .attr("font-size", "20px").attr("text-anchor", "end")
                 .attr("fill", "red");

