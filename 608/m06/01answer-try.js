d3.csv('ue_industry.csv', data => {

    const xScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.index))
        .range([1180, 20]);
    
    const yScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.Agriculture))
        .range([580, 20]);
    
    let line_ans1 = d3.line()
            .x(d => xScale(121 - d.index))
            .y(d => yScale(d.Agriculture));
        
    d3.select('#answer1')
          .append('path')
          .attr('d', line_ans1(data))
          .attr('stroke', '#2e2928')

});
