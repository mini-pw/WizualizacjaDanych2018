var data = {
  armies: null,
  countries: null
};

const side = 1000;

var mySlider = new Slider("input.slider");
mySlider.on('change', function() {
  d3.select('#year-min').html(mySlider.getValue()[0])
  d3.select('#year-max').html(mySlider.getValue()[1])
  update(data.countries, data.armies);
});

const svg = d3.select('svg')
  .attr("width", '100%')
  .attr("height", '100%')
  .attr("viewBox", "0 0 " + side + " " + side);

const FLAGS_URL = 'https://raw.githubusercontent.com/hjnilsson/country-flags/master/png100px/'

console.log(isoCountries);
svg.append("defs")
  .selectAll('pattern')
  .data(isoCountries)
    .join('pattern')
      .attr('id', d => d.cname.replace(/\s/g,'') + '-flag')
      .attr('x', '0')
      .attr('y', '0')
      .attr('width', '100%')
      .attr('height', '100%')
      .attr('viewBox', '0 0 100 100')
     .append("image")
      .attr("xlink:href", d => FLAGS_URL + d.ccode.toLowerCase() + '.png')
      .attr('preserveAspectRatio', 'xMidYMid slice')
      .attr('x', '0')
      .attr('y', '0')
      .attr('width', '100px')
      .attr('height', '100px');

flags = _(isoCountries).map(d => [d.cname, 'url(#' + d.cname.replace(/\s/g,'') + '-flag)'])
  .fromPairs().value();

const graph = svg.append('g');

const linksGroup = graph.append("g")
  .attr("stroke", "#999")
  .attr("stroke-opacity", 0.6)
  .attr('stroke-width', 1);

const nodesGroup = graph.append("g")
  .attr("stroke", "#999")
  .attr("stroke-opacity", 0.6)
  .attr('stroke-width', 1);

svg.call(d3.zoom().on('zoom', function() {
  graph.attr('transform', d3.event.transform);
}));

const armyActorLinks = d3.forceLink().strength(1).distance(() => 100);
const armyArmyLinks = d3.forceLink().strength(2).distance(() => 0);

const actorRadius = 15;
const armyRadius = 5;
const stroke = 1.5;
const colors = d3.scaleOrdinal(d3.schemePaired);

const simulation = d3.forceSimulation()
  .force('collide', d3.forceCollide().radius(d => d.radius + stroke))
  .force('center', d3.forceCenter(side / 2, side / 2))
  .force('charge', d3.forceManyBody().strength(-4))
  .force('armyActor', armyActorLinks)
  .force('armyArmy', armyArmyLinks);

var armyTooltip = d3.select("#army-tooltip");
var countryTooltip = d3.select("#country-tooltip");

function update(countries, armies) {
  const dates = _.map(mySlider.getValue(), d => new Date(d, 0, 0));
  armies = _.filter(armies, c => (dates[0] < new Date(c.datetime_min) && dates[1] > new Date(c.datetime_min)));
  countries = _.filter(countries, c => !_.isEmpty(_.find(armies, {actor: c.name})));
  armies = _.map(armies, army => _.assign(army, {radius: armyRadius}));

  const countriesIndex = _.keyBy(countries, 'name');
  const armiesIndex = _.keyBy(armies, 'id');

  simulation.nodes(_.concat(countries, armies));

  armyActorLinks.links(_.map(armies, army => ({
    source: countriesIndex[army.actor],
    target: army
  })));

  armyArmyLinks.links(_.map(armies, army => ({
    source: army,
    target: _.find(armies, {isqno: army.isqno})
  })));

  const battleLink = linksGroup.selectAll(".battle")
    .data(armyArmyLinks.links())
    .join("line")
      .attr("stroke", "red")
      .classed("battle", true);

  const forceLink = linksGroup.selectAll(".force")
    .data(armyActorLinks.links())
    .join("line")
      .classed("force", true);

  const countryNode = nodesGroup.selectAll('.country')
    .data(countries, d => d.name)
    .join('circle')
      .classed("country", true)
      .attr('r', d => actorRadius)
      .attr('fill', c => _.get(flags, c.name, colors(c.name)))
      .on("mouseover", function(d) {
        countryTooltip.select('.name').html(d.name);
        countryTooltip
            .style("left", (d3.event.pageX + 5) + "px")
            .style("top", (d3.event.pageY + 5) + "px")
            .style("display", 'block')
          .transition()
            .duration(400)
            .style("opacity", .8)
            .style("display", 'block');
      })
      .on("mouseout", function(d) {
        countryTooltip
        .transition()
          .duration(400)
          .style("opacity", 0)
          .style("display", 'none');
      });

  const armyNode = nodesGroup.selectAll('.army')
    .data(armies, d => d.id)
    .join('circle')
      .classed("army", true)
      .attr('r', d => armyRadius)
      .attr('fill', army => _.get(flags, army.actor, colors(army.actor)))
      .on("mouseover", function(d) {
        armyTooltip.select('.actor').html(d.actor);
        armyTooltip.select('.name').html(d.nam);
        armyTooltip.select('.str').html(d.str);
        armyTooltip.select('.cas').html(d.cas);
        armyTooltip
            .style("left", (d3.event.pageX + 5) + "px")
            .style("top", (d3.event.pageY + 5) + "px")
            .style("display", 'block')
          .transition()
            .duration(400)
            .style("opacity", .8)
            .style("display", 'block');
      })
      .on("mouseout", function(d) {
        armyTooltip
        .transition()
          .duration(400)
          .style("opacity", 0)
          .style("display", 'none');
      });

  function dragmove(d) {
    d.fx = d3.event.x;
    d.fy = d3.event.y;
    simulation.alpha(Math.max(simulation.alpha(), 0.1)).restart()
  }

  function dragend(d) {
    delete d.fx;
    delete d.fy;
  }

  countryNode.call(d3.drag()
          .on("drag", dragmove)
          .on("end", dragend));

  simulation.on("tick", () => {
    countryNode
      .attr("cx", d => d.x)
      .attr("cy", d => d.y);
    armyNode
      .attr("cx", d => d.x)
      .attr("cy", d => d.y);
    battleLink
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);
    forceLink
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);
  });

  simulation.alpha(Math.max(simulation.alpha(), 0.5)).restart()
}

Promise.all([
  d3.csv('data/battles.csv'),
  d3.csv('data/battle_actors.csv'),
  d3.csv('data/battle_durations.csv'),
  d3.csv('data/belligerents.csv')
]).then(function([battles, actors, durations, belligerents]) {
  data.armies = _.map(belligerents, (army, i) => _(army)
    .assign(_.find(actors, {isqno: army.isqno, attacker: army.attacker}))
    .assign(_.find(battles, {isqno: army.isqno}))
    .assign(_.find(durations, {isqno: army.isqno}))
    .assign({id: i})
    .value()
  )
  data.countries = _(data.armies)
    .map(army => army.actor)
    .uniq()
    .map(country => ({
      name: country,
      radius: actorRadius,
      x: Math.random() * side/2 - side/4,
      y: Math.random() * side/2 - side/4
    }))
    .value();
  update(data.countries, data.armies);
});
