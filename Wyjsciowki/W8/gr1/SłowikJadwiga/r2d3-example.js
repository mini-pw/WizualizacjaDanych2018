// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil(height / data.length);

var generateWidth = function(d) {
	return d * width;
}

var generateHeight = function(d, i)  {
	return i * barHeight;
}

//var widths = data.map((c) => generateWidth(c));

//var heights = data.map((c, i, arr) => generateHeight(c, i));

svg.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('width', function(d, i) { return generateWidth(d); })
    .attr('height', barHeight)
    .attr('y', function(d, i) { return generateHeight(d, i); })
.attr('fill', 'steelblue');

var text = svg.selectAll("text")
  .data(data)
  .enter()
	.append("text");
	
const barPadding = barHeight / 2;

var textLabels = text
                 .attr("x", function(d) { return generateWidth(d) - barPadding; })
                 .attr('y', function(d, i) { return generateHeight(d, i) + barPadding; })
                 .text(function (d) { return d; })
                 .attr("font-family", "sans-serif")
                 .attr("font-size", "20px")
.attr("fill", "red");
