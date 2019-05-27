// Homework shows graph grom Tymoteusz Makowski modified and presented in d3
// https://github.com/mini-pw/WizualizacjaDanych2018/tree/master/PraceDomowe/PD2/gr2/MakowskiTymoteusz
window.onload = function() {
    const countries = ["Korea Południowa", "USA", "Niemcy", "Francja", "UK", "Hiszpania", "Włochy", "RPA", "Polska"];
    const continents = ["Azja", "Ameryka Północna", "Europa", "Europa", "Europa", "Europa", "Europa", "Afryka", "Europa"];
    const values = [5500, 5000, 4600, 4000, 4000, 3700, 3500, 3500, 3000];
    const colors = ['#191a5c', '#9c2c6a','#ec6f57','#ffcf54'];
    const distinct_continents = [...new Set(continents)];
    var legend = {};
    distinct_continents.forEach((element, i) => {
        legend[element] = colors[i];
    });
    
    const wagesData = countries.map(function(c, i) {
        return {
            "country": c,
            "continent": continents[i],
            "value": values[i]
        }
    });

    const svg = d3.select('svg');
    const svgContainer = d3.select('#container');
    
    const margin = 80;
    const width = 1000 - 4 * margin;
    const height = 600 - 2 * margin;

    const chart = svg.append('g')
        .attr('transform', `translate(${margin}, ${margin})`);

    const xScale = d3.scaleBand()
        .range([0, width])
        .domain(wagesData.map((s) => s.country))
        .padding(0.4)
    
    console.log(Math.max(1,2,5,4))
    console.log(Math.max(...values))
    const yScale = d3.scaleLinear()
        .range([height, 0])
        .domain([0, Math.max(...values)]);


    const makeYLines = () => d3.axisLeft()
        .scale(yScale)

    chart.append('g')
        .attr('transform', `translate(0, ${height})`)
        .call(d3.axisBottom(xScale));

    chart.append('g')
        .call(d3.axisLeft(yScale));

    chart.append('g')
        .attr('class', 'grid')
        .call(makeYLines()
        .tickSize(-width, 0, 0)
        .tickFormat('')
        )
    
    const legendX = width + 0.5 * margin;
    const legendY = 0;
    const textWidth = Math.max(...distinct_continents.map(s => Math.max(...s.split(' ').map(ss => ss.length)))) * 8
    const legendBackground = chart.append('rect')
        .attr('class', 'legend')
        .attr('x', legendX)
        .attr('height', distinct_continents.length * 50)
        .attr('width', 200 - 20)
        .attr('fill', '#a6a6a6')

    distinct_continents.forEach((element, i) => {
        chart.append('rect')
            .attr('class', 'legendVal')
            .attr('x', width + 0.5 * margin + textWidth + 11)
            .attr('y', 50 * i + 12)
            .attr('height', 20)
            .attr('width', 100)
            .attr('fill', legend[element])    
        element.split(' ').forEach((el, j) => {
            chart.append('text')
                .attr('class', 'legendText')
                .attr('dy', 1.5 + j + "em")
                .attr('dx', "0.5em")
                .attr('x', width + 0.5 * margin)
                .attr('y', 50 * i)
                .text(el)
            
        });
    });


    const barGroups = chart.selectAll()
        .data(wagesData)
        .enter()
        .append('g')

    barGroups
        .append('rect')
        .attr('class', 'bar')
        .attr('x', (g) => xScale(g.country))
        .attr('y', (g) => yScale(g.value))
        .attr('height', (g) => height - yScale(g.value))
        .attr('width', xScale.bandwidth())
        .attr('fill', (g) => legend[g.continent])
        .on('mouseenter', function (actual, i) {
        d3.selectAll('.value')
            .attr('opacity', 0)

        d3.select(this)
            .transition()
            .duration(300)
            .attr('opacity', 0.6)
            .attr('x', (a) => xScale(a.country) - 5)
            .attr('width', xScale.bandwidth() + 10)

        const y = yScale(actual.value)

        line = chart.append('line')
            .attr('id', 'limit')
            .attr('x1', 0)
            .attr('y1', y)
            .attr('x2', width)
            .attr('y2', y)

        barGroups.append('text')
            .attr('class', 'divergence')
            .attr('x', (a) => xScale(a.country) + xScale.bandwidth() / 2)
            .attr('y', (a) => yScale(a.value) + 30)
            .attr('fill', 'white')
            .attr('text-anchor', 'middle')
            .text((a, idx) => {
            const divergence = (100*(a.value - actual.value)/actual.value).toFixed(1)
            
            let text = ''
            if (divergence > 0) text += '+'
            text += `${divergence}%`

            return idx !== i ? text : '';
            })

        })
        .on('mouseleave', function () {
        d3.selectAll('.value')
            .attr('opacity', 1)

        d3.select(this)
            .transition()
            .duration(300)
            .attr('opacity', 1)
            .attr('x', (a) => xScale(a.country))
            .attr('width', xScale.bandwidth())

        chart.selectAll('#limit').remove()
        chart.selectAll('.divergence').remove()
        })

    barGroups 
        .append('text')
        .attr('class', 'value')
        .attr('x', (a) => xScale(a.country) + xScale.bandwidth() / 2)
        .attr('y', (a) => yScale(a.value) + 30)
        .attr('text-anchor', 'middle')
        .text((a) => `${a.value}$`)
    
    svg
        .append('text')
        .attr('class', 'label')
        .attr('x', -(height / 2) - margin)
        .attr('y', margin / 2.4)
        .attr('transform', 'rotate(-90)')
        .attr('text-anchor', 'middle')
        .text('Przewidywana wysokość średniego wynagrodzenia')

    svg.append('text')
        .attr('class', 'label')
        .attr('x', width / 2 + margin)
        .attr('y', height + margin * 1.7)
        .attr('text-anchor', 'middle')
        .text('Kraje')

    svg.append('text')
        .attr('class', 'title')
        .attr('x', width / 2 + margin)
        .attr('y', 40)
        .attr('text-anchor', 'middle')
        .text('Średnie wynagrodzenie w 2040 roku')

    svg.append('text')
        .attr('class', 'source')
        .attr('x', width - margin / 2)
        .attr('y', height + margin * 1.7)
        .attr('text-anchor', 'start')
        .text('Źródło: bezprawnik.pl/wzrost-wynagrodzen-w-polsce/')
}