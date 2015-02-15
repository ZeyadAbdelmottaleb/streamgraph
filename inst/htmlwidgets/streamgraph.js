HTMLWidgets.widget({

  name: 'streamgraph',

  type: 'output',

  initialize: function(el, width, height) {
    return { }
  },

  renderValue: function(el, params, instance) {
    instance.params = params;
    this.drawGraphic(el, params, el.offsetWidth, el.offsetHeight);
  },

  drawGraphic: function(el, params, width, height) {

    // remove existing children
    while (el.firstChild)
    el.removeChild(el.firstChild);

    var format = d3.time.format("%Y-%m-%d");

    // reformat the data
    var data = HTMLWidgets.dataframeToD3(params.data) ;

    data.forEach(function(d) {
      d.date = format.parse(d.date);
      d.value = +d.value;
    });

    // assign colors

    var colorrange = [];
    var tooltip ;
    var opacity = 0.33 ;

    var ncols = d3.map(data, function(d) { return(d.key) }).keys().length;
    if (ncols > 9) ncols = 9

    colorrange = colorbrewer[params.palette][ncols].reverse();
    strokecolor = colorrange[0];

    // setup size, scales and axes

    var margin = { top: params.top, right: params.right,
                   bottom: params.bottom, left: params.left };
    width = width - margin.left - margin.right;
    height = height - margin.top - margin.bottom;

    var x = d3.time.scale().range([0, width]);
    var y = d3.scale.linear().range([height-10, 0]);
    var z = d3.scale.ordinal().range(colorrange)
              .domain(d3.set(data.map(function(d) { return(d.key) })).values());
    var bisectDate = d3.bisector(function(d) { return d.date; }).left;

    var xAxis = d3.svg.axis().scale(x)
      .orient("bottom")
      .ticks(d3.time[params.x_tick_units], params.x_tick_interval)
      .tickFormat(d3.time.format(params.x_tick_format))
      .tickPadding(8);

    var yAxis = d3.svg.axis().scale(y)
      .ticks(params.y_tick_count)
      .tickFormat(d3.format(params.y_tick_format))
      .orient("left");

    // all the magic is here

    var stack = d3.layout.stack()
      .offset(params.offset)
      .values(function(d) { return d.values; })
      .x(function(d) { return d.date; })
      .y(function(d) { return d.value; });

    var nest = d3.nest()
      .key(function(d) { return d.key; });

    var area = d3.svg.area()
      .interpolate(params.interpolate)
      .x(function(d) { return x(d.date); })
      .y0(function(d) { return y(d.y0); })
      .y1(function(d) { return y(d.y0 + d.y); });

    // build the final svg

    var svg = d3.select("#" + el.id).append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var layers = stack(nest.entries(data));

    x.domain(d3.extent(data, function(d) { return d.date; }));
    y.domain([0, d3.max(data, function(d) { return d.y0 + d.y; })]);

    svg.selectAll(".layer")
    .data(layers)
    .enter().append("path")
    .attr("class", "layer")
    .attr("d", function(d) { return area(d.values); })
    .style("fill", function(d, i) { return z(d.key); });

    // TODO legends for non-interactive
    // TODO add tracker vertical line

    if (params.interactive) {

      tooltip = svg.append("g")
      .attr("transform", "translate(30,10)")
      .append("text");

      svg.selectAll(".layer")
      .attr("opacity", 1)
      .on("mouseover", function(d, i) {
        svg.selectAll(".layer").transition()
        .duration(150)
        .attr("opacity", function(d, j) {
          return j != i ? opacity : 1;
        })})

      .on("mousemove", function(dd, i) {

        d3.select("#" + el.id + "-select")
        .selectAll("option")
        .attr("selected", function(d, i) { if (i==0) { return("selected") } })

        function iskey(key) {
          return(function(element) {
            return(element.key==key)
          })
        }

        var subset = data.filter(iskey(dd.key));

        var x0 = x.invert(d3.mouse(this)[0]),
            i = bisectDate(subset, x0, 1),
            d0 = subset[i - 1],
            d1 = subset[i],
            d = x0 - d0.date > d1.date - x0 ? d1 : d0;

        d3.select(this)
        .classed("hover", true)
        .attr("stroke", strokecolor)
        .attr("stroke-width", "0.5px")

        tooltip.text(dd.key + ": " + d.value)

      })

      .on("mouseout", function(d, i) {

        svg.selectAll(".layer")
        .transition()
        .duration(250)
        .attr("opacity", "1");

        d3.select(this)
        .classed("hover", false)
        .attr("stroke-width", "0px")

        tooltip.text("")

      })
    }

    svg.append("g")
    .attr("class", "x axis")
    .attr("transform", "translate(0," + height + ")")
    .call(xAxis);

    svg.append("g")
    .attr("class", "y axis")
    .call(yAxis);

    function onselchange() {

      var selected_value = d3.event.target.value;

      tooltip.text("");

      if (selected_value == "——— Select ———") {

        d3.selectAll("#" + el.id + " .layer")
        .transition()
        .duration(250)
        .attr("opacity", "1")
        .classed("hover", false)
        .attr("stroke-width", "0px")

      } else {

        d3.selectAll("#" + el.id + " .layer")
          .classed("hover", function(d) {
            return d.key != selected_value ? false : true;
          })
          .transition()
          .duration(150)
          .attr("opacity", function(d) {
            return d.key != selected_value ? opacity : 1;
          })
          .attr("stroke", strokecolor)
          .attr("stroke-width", function(d) {
            return d.key != selected_value ? "0px" : "0.5px";
          })
      }

    };

    if (params.legend && params.interactive) {

      if (params.legend_label != "") {
        d3.select("#" + el.id + "-legend label")
          .text(params.legend_label)
      }

      var select = d3.select("#" + el.id + "-select")
          .style("visibility", "visible")
          .on('change', onselchange)

      var selopts = d3.set(data.map(function(d) { return(d.key) })).values()
      selopts.unshift("——— Select ———")

      var options = d3.select("#" + el.id + "-select")
         .selectAll('option')
         .data(selopts).enter()
         .append('option')
           .text(function (d) { return d; })
           .attr("value", function (d) { return d; })
      }

    },

  resize: function(el, width, height, instance) {
    if (instance.params)
      this.drawGraphic(el, instance.params, width, height);
    }

});
