// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil(height / data.length);
svg.selectAll('rect').remove();
svg.selectAll('text').remove();

function factory(selection) {
  selection.append("text")
    .attr("x", function(d) { return d * (width - 100) + width / 40; })
    .attr("y", function(d, i) { return i * barHeight + barHeight / 2; })
    .text(function(d) { return d; })
    .style("fill", "black")
    .style("alignment-baseline", "middle");
    
  selection.append('rect')
    .attr('width', function(d) { return d * (width - 100); })
    .attr('height', barHeight)
    .attr('y', function(d, i) { return i * barHeight; })
    .attr('fill', 'steelblue');
}

svg.selectAll('rect')
  .data(data)
  .enter().call(factory);
    
