// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil(height / data.length);

var x = d3.scaleLinear()
          .domain([0, d3.max(data, function(d) { return d; })])
          .range([0, width ]);
        
var xAxis = d3.axisBottom(x);


svg.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('width', function(d) { return d * width; })
    .attr('height', barHeight)
    .attr('y', function(d, i) { return i * barHeight; })
    .attr('fill', 'steelblue');

var text = svg.selectAll("text")
  .data(data)
  .enter()
  .append("text");

var textLabels = text
                 .attr("x", function(d) { return (d * width) - 50; })
                 .attr('y', function(d, i) { return i * barHeight + barHeight / 2; })
                 .text(function (d) { return d; })
                 .attr("font-family", "sans-serif")
                 .attr("font-size", "20px")
                 .attr("fill", "red");

svg.append('g')
	.attr('class', 'main axis date')
	.call(xAxis);