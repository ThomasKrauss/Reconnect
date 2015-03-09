function getSvgTextWidth(fontSize, obj) {
	var testBox, width;
	testBox = '<svg id="TestBoxGetSvgTextWidth" style="position:absolute;visibility:hidden;"><text id="TestGetSvgTextWidth" style="height:auto;width:auto;">'+obj+'</text></svg>';
	$('body').append(testBox);
	
	width = document.getElementById('TestGetSvgTextWidth').getComputedTextLength();
	
	$('#TestBoxGetSvgTextWidth').remove();
	return width+1;
};

function colorNotifyInterpolate(a, b) {
	var AtoB = d3.interpolateRgb(a,b), BtoA = d3.interpolateRgb(b,a);
	return function(t) {
		if(t < 0.5) {
			return AtoB(t);
		} else {
			return BtoA(t);
		}
	};
}

/**
	The bar chart dimensions are set by default through height and width and then the height and width of each bar
	will be computed to fit depending on how much is passed.
	
	If barHeight is set, the global height will be ignored.
	
	Another attribute that is not set is xScaleMaxValue.
	By default, the horizontal will spread across the range of the given data.
	If xScaleMaxValue is provided, it will instead spread across [0, xScaleMaxValue]
**/
function makeBarChart() {
	// Configuration
	var width = 700,
		labelMaxWidth = 150,
		height = 400,
		xValue = function(d) { return d[0]; },
		yValue = function(d) { return d[1]; },
		yValueAdapter = function(d) { return d3.round(yValue(d), 2); },
		dataBindingKey = function(d) { return d[0]; },
		tickBindingKey = function(d) { return d; },
		xLabelHeight = 18, // the height of the labels of the xAxis impacts the overall height
		xScale = d3.scale.linear(),
		yScale = d3.scale.ordinal(),
		tickCount = 10,
		showGridLines = false,
		maxFontSize = 20,
		barHeight,
		barHeightSet = false,
		xScaleMaxValue,
		legendWidth,
		legendWidthSet = false,
		showLabels = true,
		labelsInLayout = true;
	
	// Internal values
	var barLabelPadding = 5; // padding between bar and bar labels (left)
	var labelPadding = 10; // padding between bar labels and value labels
	var gridChartOffset = 3; // space between start of grid and first bar
	var barPadding = 3; // padding between bars
	var tickHeight = 3;
	
	// Internal visual parts
	var svg, labelsContainer, valueLabelsContainer, barsContainer, xAxisContainer, xAxisTickContainer, gridContainer;
	
	var refresh = function(data, transitionDuration) {
		var	fontSize, currentLegendWidth, maxBarWidth, yLabelWidth, maxTextLength, usedLabelPadding, valueLabelWidth;
		var labelsSelection, valueLabelsSelection, barsSelection, xAxisTickSelection, gridSelection, gridLineSelection;
		var exitTransition, updateTransition, enterTransition;
		
		var y = function(d, i) { return yScale(i) + barPadding * i; };
		var yText = function(d, i) { return y(d, i) + yScale.rangeBand() / 2; };
		var truncateText = function(value) {
			var text = xValue(value)+'';
			if(maxTextLength < text.length) {
				return text.slice(0, maxTextLength - 3)+'...';
			} else {
				return text;
			}
		};
		
		if(transitionDuration === undefined) {
			transitionDuration = 500;
		}
		
		fontSize = currentLegendWidth = maxBarWidth = yLabelWidth = maxTextLength = usedLabelPadding = valueLabelWidth =legendWidth = 0;
		
		if(data.length > 0) {
			// Compute the height: by default it's about the bar height but if it was set, it's the global chart height that should be adapted
			if(barHeightSet) {
				height = barHeight * data.length + xLabelHeight + gridChartOffset + tickHeight + barPadding * Math.max(0, (data.length - 1));
			} else {
				if(data.length > 0) {
					barHeight = Math.floor((height - xLabelHeight - tickHeight - gridChartOffset - barPadding * (data.length - 1)) / data.length);
				} else {
					barHeight = 0;
				}
			}
			
			// Derive the font size from the bar height and get the necessary width for label containers
			fontSize = Math.min(Math.floor(barHeight * 2 / 3), maxFontSize);
			// yLabelWidth = valueLabelWidth = maxTextLength = 0;
			usedLabelPadding = showLabels ? labelPadding : 0;
			if(data.length > 0) {
				if(showLabels) {
					myMap(data, function(value) {
						var text = xValue(value)+'', textWidth = getSvgTextWidth(fontSize, text);
						if(textWidth > yLabelWidth) {
							yLabelWidth = textWidth;
							maxTextLength = text.length;
						}
					});
					
					// Limit the width of the labels so as not to crush the chart itself
					if(yLabelWidth > labelMaxWidth) {
						maxTextLength = Math.floor(maxTextLength * labelMaxWidth / yLabelWidth);
						yLabelWidth = labelMaxWidth;
					}
				}
				valueLabelWidth = d3.max(data, function(value) { return getSvgTextWidth(fontSize, yValueAdapter(value)); });
			}
			
			// Update scales 
			yScale.domain(d3.range(0, data.length)).rangeBands([0, data.length * barHeight]);
			
			if(xScaleMaxValue) {
				xScale.domain([0, xScaleMaxValue]);
			} else {
				xScale.domain([0, d3.max(data, yValue)]);
			}
			xScale.nice(tickCount);
			
			// Compute the legend width: by default, we derive it
			// But if already set, we correct the label width to match the given legend width (value labels are not impacted).
			currentLegendWidth = yLabelWidth + valueLabelWidth + usedLabelPadding + barLabelPadding;
			if(legendWidthSet) {
				if(showLabels) {
					yLabelWidth += legendWidth - currentLegendWidth;
				} else {
					usedLabelPadding += legendWidth - currentLegendWidth;
				}
			} else {
				legendWidth = currentLegendWidth;
			}
			
			// Compute the maximum possible width of a bar and tie the xScale to this value
			if(data.length > 0) {
				maxBarWidth = width - yLabelWidth - usedLabelPadding - valueLabelWidth - 1 - getSvgTextWidth(xLabelHeight, xScale.domain()[1]);
			} else {
				maxBarWidth = 0;
			}
			xScale.range([0, maxBarWidth]);
		}
		
		labelsSelection = labelsContainer.selectAll('text').data(data, dataBindingKey);
		valueLabelsSelection = valueLabelsContainer.selectAll("text").data(data, dataBindingKey);
		barsSelection = barsContainer.selectAll("rect").data(data, dataBindingKey);
		xAxisTickSelection = xAxisTickContainer.selectAll("line").data(xScale.ticks(tickCount), tickBindingKey);
		gridSelection = gridContainer.selectAll("text").data(xScale.ticks(tickCount), tickBindingKey);
		if(showGridLines) {
			gridLineSelection = gridContainer.selectAll("line").data(xScale.ticks(tickCount), tickBindingKey);
		}
		
		if(transitionDuration === 0 && data.length > 0) {
			svg.attr('width', width).attr('height', height);
		}
		
		// Smooth transitioning from removing elements first, updating them and finally adding the new ones
		// In other words:
		// 1) free the space taken by removed elements
		// 2) replace the existing ones and resize things (bars, labels, axis, the whole graph position and dimensions...)
		//    to welcome new elements
		// 3) place the new elements
		exitTransition = d3.transition().duration(transitionDuration).each(function() {
			barsSelection.exit().remove();
			valueLabelsSelection.exit().remove();
			labelsSelection.exit().remove();
			xAxisTickSelection.exit().remove();
			gridSelection.exit().remove();
			if(showGridLines) {
				gridLineSelection.exit().remove();
			}
		});
		
		updateTransition = exitTransition.transition().each(function() {
			if(data.length > 0) {
				svg.transition()
					.attr('width', width)
					.attr('height', height);
			} else {
				svg.transition()
					.attr('width', 0)
					.attr('height', 0);
			}
			
			barsContainer.transition()
				.attr('transform', 'translate(' + Math.max(0, yLabelWidth + valueLabelWidth + usedLabelPadding + barLabelPadding - 1) + ',0)');// -1 account for the first tick width
			
			barsSelection.transition()
				.attr('y', y)
				.attr('height', yScale.rangeBand())
				.attr('width', function(d) { return xScale(yValue(d)); });
			
			valueLabelsContainer.transition()
				.attr('transform', 'translate(' + (yLabelWidth + usedLabelPadding) + ',0)');
			
			valueLabelsSelection.text(function(d) { return d3.round(yValue(d), 2); })
				.transition()
				.attr("y", yText)
				.attr('font-size', fontSize);
			
			labelsContainer.transition()
				.attr('transform', 'translate('+ yLabelWidth + ',0)');
			
			labelsSelection.transition()
				.attr('font-size', fontSize)
				.attr('y', yText);
			
			xAxisContainer.transition()
				.attr('transform', 'translate(' + legendWidth + ',' + Math.max(0, height - xLabelHeight - tickHeight) + ')');
			
			xAxisContainer.select('line').transition()
				.attr("x1", -1) // -1 account for the first tick width
				.attr("x2", xScale.range()[1] + 1); // + 1 account for the last tick width
			
			xAxisTickContainer.transition()
				.attr('transform', 'translate(' + legendWidth + ',' + Math.max(0, height - xLabelHeight - tickHeight) + ')');
			
			xAxisTickSelection.transition()
				.attr("x1", xScale)
				.attr("x2", xScale)
				.attr("y2", tickHeight);
			
			gridContainer.transition()
				.attr('transform', 'translate(' + legendWidth + ',0)');
			
			gridSelection.transition()
				.attr('font-size', xLabelHeight)
				.attr("x", xScale)
				.attr("y", height + tickHeight);
			
			if(showGridLines) {
				gridLineSelection.transition()
					.attr("x1", xScale)
					.attr("x2", xScale)
					.attr("y2", yScale.rangeExtent()[1] + barPadding * (data.length - 1) + gridChartOffset);
			}
		});
		
		enterTransition = updateTransition.transition().each(function() {
			barsSelection.enter()
				.append("rect")
				.attr('class', 'bar')
				.style('opacity', 0)
				.transition()
				.style('opacity', 1)
				.attr('y', y)
				.attr('height', yScale.rangeBand())
				.attr('width', function(d) { return xScale(yValue(d)); });
			
			valueLabelsSelection.enter().append("text")
				.style('opacity', 0)
				.attr('class', 'legend')
				.text(yValueAdapter)
				.attr("dy", ".35em") // vertical-align: middle
				.attr("text-anchor", "start") // text-align: left
				.transition()
				.style('opacity', 1)
				.attr("y", yText)
				.attr('font-size', fontSize);
			
			labelsSelection.enter().append('text')
				.attr('class', 'legend')
				.style('opacity', 0)
				.text(truncateText)
				.attr("dy", ".35em") // vertical-align: middle
				.attr('text-anchor', 'end') // text-align: right
				.transition()
				.style('opacity', 1)
				.attr('font-size', fontSize)
				.attr('y', yText);
			
			xAxisTickSelection.enter().append("line")
				.attr("y1", 0)
				.attr('class', 'axis')
				.transition()
				.attr("x1", xScale)
				.attr("x2", xScale)
				.attr("y2", tickHeight);
			
			gridSelection.enter().append("text")
				.text(String)
				.attr("dy", -3)
				.attr("text-anchor", "middle")
				.attr('class', 'legend')
				.transition()
				.attr('font-size', xLabelHeight)
				.attr("x", xScale)
				.attr("y", height + tickHeight);
			
			if(showGridLines) {
				gridLineSelection.enter().append("line")
					.attr("y1", 0)
					.attr('class', 'grid')
					.transition()
					.attr("x1", xScale)
					.attr("x2", xScale)
					.attr("y2", yScale.rangeExtent()[1] + barPadding * (data.length - 1) + gridChartOffset);
			}
		});
	};
	
	var chart = function(selection) {
		selection.each(function(data) {
			var className;
			
			// SVG element: add one if the given DOM element is not an SVG element
			if(this.tagName.toLowerCase() !== 'svg') {
				svg = d3.select(this).append("svg");
			} else {
				svg = d3.select(this);
			}
			
			// Add barchart class
			className = svg.attr('class');
			if(className && className !== '') {
				className += ' barchart';
			} else {
				className = 'barchart';
			}
			
			svg.attr('class', className).attr('width', 0).attr('height', 0);
			
			labelsContainer = svg.append('g');
			valueLabelsContainer = svg.append('g');
			barsContainer = svg.append('g');
			xAxisContainer = svg.append('g');
			xAxisTickContainer = svg.append('g');
			gridContainer = svg.append('g');
			
			xAxisContainer.append('line').attr('class', 'axis');
			
			if(data) {
				refresh(data, 0);
			}
		});
	};
	
	/**
		ACCESSORS
	**/
	chart.refresh = function(data) {
		if(data) {
			refresh(data);
		} else {
			refresh([], 0);
		}
	};
	
	chart.margin = function(_) {
		if (!arguments.length) return margin;
		margin = _;
		return chart;
	};

	chart.width = function(_) {
		if (!arguments.length) return width;
		width = _ * 1;
		return chart;
	};

	chart.height = function(_) {
		if (!arguments.length) return height;
		height = _ * 1;
		return chart;
	};

	chart.x = function(_) {
		if (!arguments.length) return xValue;
		xValue = _;
		return chart;
	};

	chart.y = function(_) {
		if (!arguments.length) return yValue;
		yValue = _;
		return chart;
	};

	chart.xLabelHeight = function(_) {
		if (!arguments.length) return xLabelHeight;
		xLabelHeight = _ * 1;
		return chart;
	};
	
	chart.showGridLines = function(_) {
		if (!arguments.length) return showGridLines;
		showGridLines = _;
		return chart;
	};
	
	chart.tickCount = function(_) {
		if (!arguments.length) return tickCount;
		tickCount = _ * 1;
		return chart;
	};
	
	chart.barHeight = function(_) {
		if (!arguments.length) return barHeight;
		if(_) {
			barHeight = _ * 1;
			barHeightSet = true;
		} else {
			barHeightSet = false;
		}
		return chart;
	};
	
	chart.xScaleMaxValue = function(_) {
		if (!arguments.length) return xScaleMaxValue;
		xScaleMaxValue = _ * 1;
		return chart;
	};
	
	chart.legendWidth = function(_) {
		if (!arguments.length) return legendWidth;
		if(_) {
			legendWidth = _ * 1;
			legendWidthSet = true;
		} else {
			legendWidthSet = false;
		}
		return chart;
	};
	
	chart.showLabels = function(_) {
		if (!arguments.length) return showLabels;
		if(_) {
			showLabels = true;
		} else {
			showLabels = false;
		}
		return chart;
	};
	
	chart.labelsInLayout = function(_) {
		if (!arguments.length) return labelsInLayout;
		if(_) {
			labelsInLayout = true;
		} else {
			labelsInLayout = false;
		}
		return chart;
	};
	
	return chart;
}