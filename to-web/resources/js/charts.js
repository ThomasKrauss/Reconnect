/** @jsx React.DOM */
var getBarChartData = function(data, valueKey) {
	var result = [];
	
	myMap(data, function(value) {
		result.push([value['name'], value['stats'][valueKey]]);
	})
	
	return result;
};

var BarChart = React.createClass({
		getInitialState: function() {
			var me = this, timeoutId, chart = makeBarChart();
			var refreshFn = function(props) {
				if(timeoutId) {
					window.clearTimeout(timeoutId);
				}
				timeoutId = window.setTimeout(function() {
					me.updateChartAttributes(props);
					chart.refresh(props.data);
					if(isFunction(me.props.onUpdate)) {
						me.props.onUpdate.apply(null, [chart]);
					}
					window.clearTimeout(timeoutId);
				}, 500);
			};
			
			return {'chart': chart, 'refreshFn': refreshFn};
		},
		
		componentDidMount: function() {
			this.state.chart
				.y(function(d) { return parseFloat(d[1]); })
				.width(this.props.width)
				.barHeight(this.props.barHeight)
				.tickCount(this.props.tickCount)
				.xLabelHeight(this.props.xLabelHeight);
			
			if(this.props.hasOwnProperty('showLabels')) {
				this.state.chart.showLabels(this.props.showLabels);
			}
			
			this.updateChartAttributes(this.props);
			
			d3.select($(this.getDOMNode()).get(0))
				.datum(this.props.data)
				.call(this.state.chart);
		},
		
		updateChartAttributes: function(props) {
			var me = this;
			myMap(props, function(property, value) {
				if(isFunction(me.state.chart[property])) {
					me.state.chart[property](value);
				}
			});
		},
		
		componentWillReceiveProps: function(nextProps) {
			this.state.refreshFn(nextProps);
		},
		
		render: function() {
			return <div className={this.props.chartClassName}></div>;
		}
});

var SmallMultipleBarChart = React.createClass({
	getInitialState: function() {
		return {'usedLegendWidth': 0}
	},
	
	recordUsedLegendWidth: function(legendWidth) {
		this.setState({'usedLegendWidth': legendWidth});
	},
	
	render: function() {
		var index = 0, me = this;
		var basicOptions = {'barHeight': this.props.barHeight,
							'tickCount': this.props.tickCount,
							'xLabelHeight': this.props.xLabelHeight};
		
		var chart = function(data, width, showLabels) {
			var chartOptions = clone(basicOptions);
			
			chartOptions.data = data;
			chartOptions.key = index;
			chartOptions.width = width;
			chartOptions.legendWidth = isArray(me.props.legendWidth) ? me.props.legendWidth[index] : null;
			
			if(arguments.length < 3) {
				chartOptions.showLabels = true;
			} else {
				chartOptions.showLabels = showLabels;
			}
			
			if(isArray(me.props.scaleFactors) && me.props.scaleFactors[index]) {
				chartOptions.xScaleMaxValue = me.props.scaleFactors[index];
			}
			
			if(isFunction(me.props.onUpdate) && chartOptions.showLabels) {
				chartOptions.onUpdate = function(chart) {
					me.recordUsedLegendWidth(chart.legendWidth());
					me.props.onUpdate(chart, chartOptions.key);
				};
				
			} else if(isFunction(me.props.onUpdate)) {
				chartOptions.onUpdate = function(chart) {
					me.props.onUpdate(chart, chartOptions.key);
				};
				
			} else if (chartOptions.showLabels) {
				chartOptions.onUpdate = function(chart) {
					me.recordUsedLegendWidth(chart.legendWidth());
				};
			}
			
			index++;
			return BarChart(chartOptions);
		};
		
		return enlistAsHorizontalList(
					chart(this.props.data[0], this.props.width),
					myMap(this.props.data.slice(1), function(value) {
						var width = me.props.width - me.state.usedLegendWidth;
						return chart(value, width, false);
					}));
	}
});

var totalPerKey = function(data, keys, title) {
	return enlistAsVerticalList(
				<p>{title}</p>,
				myMap(keys, function(key) {
					var value = data && data['stats'] ? data['stats'][key] : null;
					return <p><span className="label">{key}:</span>{value}</p>;
				}));
};

var ConnectedSmallMultipleBarChart = React.createClass({
	getInitialState: function() {
		return {'data': []};
	},
	
	componentDidMount: function () {
		EndPoints.on(this.props.endPointName, this.update);
	},
	
	getFirstKey: function(obj) {
		return obj.stats[this.props.dataKeys[0]];
	},
	
	compareFirstKey: function(a, b) {
		return d3.descending(this.getFirstKey(a), this.getFirstKey(b));
	},
	
	update: function(data) {
		var result = data.value, subData = isArray(data.value['in']) ? data.value['in'] : [];
		result['in'] = subData.sort(this.compareFirstKey);
		this.setState({'data': result});
	},
	
	render: function() {
		var me = this, chartData = [];
		myMap(this.props.dataKeys, function(key) {
			chartData.push(getBarChartData(me.state.data['in'], key));
		});
		
		return enlistAsHorizontalList(
					totalPerKey(this.state.data, this.props.dataKeys),
					<SmallMultipleBarChart key='mine' width={this.props.width} tickCount={this.props.tickCount}
										xLabelHeight={this.props.xLabelHeight} barHeight={this.props.barHeight}
										data={chartData} />);
	}
});

var NestedBarChart = React.createClass({
	getInitialState: function() {
		var me = this, totalChartLegendWidth = {}, allLegendWidth = {};
		var findMaximum = function(key) {
			var maxValue = totalChartLegendWidth[key];
			myMap(allLegendWidth[key], function(property, value) {
				if(!maxValue || value && maxValue < value) {
					maxValue = value;
				}
			});
			return maxValue;
		};
		
		var timeoutId;
		var recordLegendWidth = function(chart, keyIndex, name) {
			var key = me.props.dataKeys[keyIndex], legendWidth = me.state.legendWidth;
			var found = true, index = 0, updated = false;
			
			if(name) {
				if(!allLegendWidth[key]) {
					allLegendWidth[key] = {};
				}
				allLegendWidth[key][name] = chart.legendWidth();
			} else {
				totalChartLegendWidth[key] = chart.legendWidth();
			}
			
			while(found && index < me.props.dataKeys.length) {
				found = findMaximum(me.props.dataKeys[index]);
				if(legendWidth[me.props.dataKeys[index]] !== found) {
					updated = true;
					legendWidth[me.props.dataKeys[index]] = found;
				}
				index++;
			}
			
			if(found && updated) {
				if(timeoutId) {
					window.clearTimeout(timeoutId);
				}
				timeoutId = window.setTimeout(function() {
					me.setState({'legendWidth': legendWidth});
					window.clearTimeout(timeoutId);
				}, 250);
			}
		};
		
		return {'topData': [],'encompassedData': {},'recordLegendWidth': recordLegendWidth,'legendWidth': {}};
	},
	
	componentDidMount: function() {
		EndPoints.on(this.props.endPointName, this.updateTopData);
		EndPoints.on(this.props.endPointName+'/', this.updateEncompassedData);
	},
	
	getFirstKey: function(obj) {
		return obj.stats[this.props.dataKeys[0]];
	},
	
	compareFirstKey: function(a, b) {
		return d3.descending(this.getFirstKey(a), this.getFirstKey(b));
	},
	
	updateTopData: function(data) {
		var result = data.value, subData = isArray(data.value['in']) ? data.value['in'] : [];
		result['in'] = subData.sort(this.compareFirstKey);
		this.setState({'topData': result});
	},
	
	getMaxScaleValue: function() {
		var me = this, xScaleMaxValue = {};
		myMap(this.state.encompassedData, function(key, data) {
			myMap(data['in'], function(value) {
				myMap(me.props.dataKeys, function(key) {
					if(!xScaleMaxValue[key] || xScaleMaxValue[key] < value.stats[key]) {
						xScaleMaxValue[key] = value.stats[key];
					}
				});
			});
		});
		return xScaleMaxValue;
	},
	
	updateEncompassedData: function(data) {
		var result = data.value, subData = isArray(data.value['in']) ? data.value['in'] : [];
		result['in'] = subData.sort(this.compareFirstKeyFn);
		this.state.encompassedData[data.value.name] = result;
		this.setState({'encompassedData': this.state.encompassedData});
	},
	
	buildChartData: function(data) {
		var chartData = [];
		myMap(this.props.dataKeys, function(key) {
			chartData.push(getBarChartData(data, key));
		});
		return chartData;
	},
	
	render: function() {
		var me = this, xScaleMaxValue = this.getMaxScaleValue();
		var chart = function(data, scale, onUpdate) {
			var chartData = [], scaleFactors = [], barHeight, legendWidth = [];
			myMap(me.props.dataKeys, function(key) {
				chartData.push(getBarChartData(data, key));
				legendWidth.push(me.state.legendWidth[key]);
				if(scale) {
					scaleFactors.push(xScaleMaxValue[key]);
				}
			});
			
			barHeight = scale ? Math.floor(me.props.barHeight * 3/4) : me.props.barHeight;
			
			return (<SmallMultipleBarChart width={me.props.width} tickCount={me.props.tickCount}
											xLabelHeight={me.props.xLabelHeight}
											data={chartData} barHeight={barHeight} scaleFactors={scaleFactors}
											onUpdate={onUpdate} legendWidth={legendWidth} />);
		};
		
		var totalChart = function(data) {
			return chart(data, false, me.state.recordLegendWidth);
		};
		
		var detailChart = function(data, name) {
			return chart(data, true, function(chart, keyIndex) {
				me.state.recordLegendWidth(chart, me.props.dataKeys[keyIndex], name);
			});
		};
		var width = this.props.width;
		var result = enlistAsVerticalList(
				totalChart(this.state.topData['in']),
				myMap(this.state.topData['in'], function(item) {
					var detailData = me.state.encompassedData[item.name];
					if(detailData) {
						return detailChart(detailData['in'], item.name);
					}
				}));
		return result ? result : <ul></ul>;
	}
});