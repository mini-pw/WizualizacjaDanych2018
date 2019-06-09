// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil(height / data.length);

var scaleY = d3.scaleLinear()
              .domain([d3.min(data), d3.max(data)])
              .range([0, width - 100]);

var scaleX = d3.scaleOrdinal()
               .domain(data)
               .range(data);

svg.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('width', function(d) { return d * width; })
    .attr('height', barHeight)
    .attr('y', function(d, i) { return i * barHeight; })
    .attr('fill', 'steelblue')


var text = svg.selectAll('text')
              .data(data)
              .enter()
              .append("text")


// var textLabels = text
//     .attr("x", (function(d, i) { return d * width - 50  }))
//     .attr("y", function(d, i) { return scaleY(d) })
//     .text(function(d) { return d; });



var textLabels = text
    .attr("x", (function(d, i) { return d * width * 0.5 }))
    .attr("y", function(d, i) { return  i * barHeight + barHeight * 0.5 })
    .text(function(d) { return d; }); 

