// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil(height / data.length);
var maxW = Math.max.apply(Math,data); 

var barMargin = 1/0.95;
svg.selectAll("*").remove();
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

// X axis
var x = d3.scaleLinear()
  .domain([1,0])
  .range([width,0])
svg.append("g")
  .attr("transform", "translate(0," + 0 + ")")
  .call(d3.axisBottom(x))

// var y = d3.scaleOrdinal()
//   .domain(data.map(function(d){return d;}))
//   .range([height,0]);
// svg.append("g")
//   .attr("transform", "translate(100," + 0 + ")")
//   .call(d3.axisLeft(y))

var textLabels = text
                 .attr("x", function(d) { return d * width - 50; })
                 .attr('y', function(d, i) { return (i + 0.5) * barHeight ; })
                 .text(function (d) { return d; })
                 .attr("font-family", "sans-serif")
                 .attr("font-size", "20px")
                 .attr("fill", "red");
