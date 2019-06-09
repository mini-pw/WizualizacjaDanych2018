!(function () {
    const Solution = {
        create_df: function () {
            const names = ['Eisenbichler M.', 'Geiger K.', 'Peier K.', 'Kobayashi R.', 'Stoch K.',
                'Kraft S.', 'Forfang J.', 'Johansson R.', 'Freitag R.', 'Zajc T.',
                'Huber D.', 'Kubacki D.', 'Aschenwald P.', 'Hayboeck M.', 'Ammann S.',
                'Prevc P.', 'Kobayashi J.', 'Klimov E.', 'Zyla P.', 'Ito D.', 'Sato Y.',
                'Koudelka R.', 'Jelar Z.', 'Fettner M.', 'Polasek V.', 'Schuler A.',
                'Boyd-Clowes M.', 'Learoyd J.', 'Stjernen A.', 'Zografski V.'];
            const nationalities = ['GER', 'GER', 'SUI', 'JPN', 'POL', 'AUT', 'NOR', 'NOR', 'GER',
                'SLO', 'AUT', 'POL', 'AUT', 'AUT', 'SUI', 'SLO', 'JPN', 'RUS',
                'POL', 'JPN', 'JPN', 'CZE', 'SLO', 'AUT', 'CZE', 'SUI', 'CAN',
                'FRA', 'NOR', 'BUL'];
            const series_1 = [131.5, 131.0, 131.0, 133.5, 128.5, 130.0, 132.5, 128.0, 125.5, 127.0, 126.0,
                128.5, 120.0, 122.0, 122.5, 123.5, 116.0, 126.5, 128.5, 119.0, 120.0, 120.5,
                118.5, 117.5, 120.0, 117.5, 117.0, 116.5, 124.5, 117.0];
            const series_2 = [135.5, 130.5, 129.5, 126.5, 129.5, 126.5, 125.5, 129.0, 129.5, 124.0, 125.5,
                125.5, 128.0, 125.5, 126.0, 122.0, 132.0, 121.0, 121.0, 126.0, 124.0, 120.5,
                121.0, 122.5, 117.5, 119.0, 118.5, 116.5, 102.0, 0];
            const total = [279.4, 267.3, 266.1, 262.0, 259.4, 256.1, 250.9, 248.9, 248.7, 245.5, 242.0,
                240.2, 239.9, 233.7, 230.6, 230.5, 230.0, 229.1, 228.7, 225.7, 221.4, 220.1,
                219.8, 219.0, 218.2, 212.6, 212.1, 205.9, 185.5, 117.0];

            const df = [];
            for (let i = 0; i < names.length; i++) {
                df.push({
                    name: names[i],
                    nationality: nationalities[i],
                    series_1: series_1[i],
                    series_2: series_2[i],
                    total: total[i]
                })
            }
            Solution.base_df = df;
            Solution.unique_nationalities = [...new Set(nationalities)].sort();
            Solution.series = ['series_1', 'series_2', 'total'];
        },
        init_chart: function () {

        },
        draw_df: function (type) {
            const margin = { top: 20, right: 60, bottom: 60, left: 60 },
                width = 1800 - margin.left - margin.right,
                height = 900 - margin.top - margin.bottom;

            d3.select('div#plot>svg').remove();
            d3.select('div#plot>#tooltip').remove();

            const chart = d3.select('div#plot')
                .append('svg')
                .attr('width', width + margin.right + margin.left)
                .attr('height', height + margin.top + margin.bottom)
                .append('g')
                .attr('transform', `translate(${margin.left}, ${margin.top})`);

            const tooltip = d3.select('div#plot').append('div')
                .attr('id', 'tooltip')
                .style('opacity', 0);

            const df = Solution.base_df.map(x => x);
            df.sort((a, b) => a[type] === b[type] ? 0 : a[type] < b[type]);

            const xScale = d3.scaleBand()
                .range([0, width])
                .domain(df.map(x => x.name))
                .padding(0.2);

            const yScale = d3.scaleLinear()
                .domain([0, Math.max(...df.map(x => x[type]))])
                .range([height, 0]);

            const colorScale = d3.scaleOrdinal(d3.schemeCategory10);

            chart.append('g')
                .attr('transform', 'translate(0,' + height + ')')
                .call(d3.axisBottom(xScale))

            chart.append('g')
                .call(d3.axisLeft(yScale));

            chart.append('text')
                .attr('class', 'label')
                .attr('x', 0)
                .attr('y', -6)
                .style('text-anchor', 'end')
                .text('Points');

            const cursor = chart.selectAll('rect').data(df)

            cursor.enter()
                .append('rect')
                .merge(cursor)
                .transition()
                .duration(1000)
                .attr('x', e => xScale(e.name))
                .attr('y', e => yScale(e[type]))
                .attr('width', xScale.bandwidth())
                .attr('height', e => height - yScale(e[type]))
                .attr('fill', e => colorScale(Solution.unique_nationalities.indexOf(e.nationality)));

            chart.selectAll('rect')
                .on('mouseover', (d, i, e, n) => {
                    const boundingRect = d3.event.originalTarget.getBoundingClientRect();
                    tooltip.transition()
                        .duration(200)
                        .style('opacity', .9);
                    tooltip.html(`<span>${d.name}</span></br>${d[type]}`)
                        .style('left', boundingRect.left + 'px')
                        .style('top', (boundingRect.top - 40) + 'px');
                })

            const legend = chart.selectAll('.legend')
                .data(colorScale.domain())
                .enter().append('g')
                .attr('class', 'legend')
                .attr('transform', (d, i) => `translate(60, ${i * 20})`);

            legend.append('rect')
                .attr('x', width - 18)
                .attr('width', 18)
                .attr('height', 18)
                .style('fill', colorScale);

            legend.append('text')
                .attr('x', width - 24)
                .attr('y', 9)
                .attr('dy', '.35em')
                .style('text-anchor', 'end')
                .text(i => Solution.unique_nationalities[i]);

        },
        create_gui() {
            const select_list = document.querySelector('#series');
            for (const value of Solution.series) {
                let option = document.createElement('option');
                option.value = value;
                option.innerText = value.replace('_', ' ').toUpperCase();
                select_list.add(option);
            }
            select_list.addEventListener('change', e => {
                Solution.draw_df(e.target.value);
            });
        },
    }
    Solution.create_df();
    setTimeout(() => {
        Solution.create_gui();
        Solution.draw_df(document.querySelector('#series').value);
    });
})();
