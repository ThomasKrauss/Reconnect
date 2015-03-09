function nil(val) {
	if(val === undefined || val === null) {
		return [];
	}
	return val;
}

function makeGraph() {
	// Internal values
	var scaling;
	var pointToPixel = function(nb) {
		return scaling * nb;
	};
	
	// Configuration
	var graphWidth,
		width,
		graphHeight,
		fontSize = 12,
		innerPadding = 5,
		nodeBindingKey = function(d) { return d[0]; },
		nodeTextValue = function(d) { return d[1]; },
		nodeDisplayClass = function(d) { return d[2]; },
		nodeClass = function(d) { return 'node '+nodeDisplayClass(d); },
		xValue = function(d) { return pointToPixel(d[3]); },
		yValue = function(d) { return pointToPixel(d[4]); },
		widthValue = function(d) { return pointToPixel(d[5]); },
		heightValue = function(d) { return pointToPixel(d[6]); },
		yNodeTextValue = function(d) { return yValue(d) + fontSize/4; },
		edgeClass = function(d) { return 'edge '+d[1]; },
		pathValue = function(d) {
			var i, arr = [];
			arr.push('M'+pointToPixel(d[2])+','+pointToPixel(d[3]));
			arr.push('C'+pointToPixel(d[4])+','+pointToPixel(d[5]));
			for(i=6; i+1<d.length; i+=2) {
				arr.push(pointToPixel(d[i])+','+pointToPixel(d[i+1]));
			}
			return arr.join(' ');
		},
		clusterClass = function(d) { return 'cluster '+d[1]; },
		clusterXValue = function(d) { return pointToPixel(d[2]); },
		clusterYValue = function(d) { return pointToPixel(d[3]); },
		clusterWidthValue = function(d) { return pointToPixel(d[4]); },
		clusterHeightValue = function(d) { return pointToPixel(d[5]); },
		xClusterTextValue = function(d) { return clusterXValue(d) + clusterWidthValue(d)/2; },
		yClusterTextValue = function(d) { return clusterYValue(d) + clusterHeightValue(d) - fontSize/2; },
		xScale = d3.scale.linear(),
		yScale = d3.scale.linear();
	
	var svg;
	
	var refresh = function(data, transitionDuration) {
		var nodeSelection, edgeSelection, labelSelection, clusterSelection;
		var exitTransition, updateTransition, enterTransition;
		
		if(transitionDuration === undefined) {
			transitionDuration = 500;
		}
		
		if(data.width && data.height) {
			graphWidth = data.width;
			graphHeight = data.height;
			scaling = fontSize / data['font-size'];
			
			if(width && width < pointToPixel(graphWidth)) {
				scaling = width / graphWidth;
			}
			
			// Do note that the '.edge' and '.label' classes are given on the SVG elements 'path' and 'text'
			// But the '.node' class is given to the graphical element 'g' which contain an ellipse and a text
			// Further selections are needed in this case to transform the nodes
			// The same goes for '.cluster'
			nodeSelection = svg.selectAll('.node').data(nil(data['nodes']), nodeBindingKey);
			edgeSelection = svg.selectAll('.edge').data(nil(data['edges']), nodeBindingKey);
			labelSelection = svg.selectAll('.label').data(nil(data['labels']), nodeBindingKey);
			if($.isArray(data['clusters'])) {
				clusterSelection = svg.selectAll('.cluster').data(nil(data['clusters']), nodeBindingKey);
			}
			
			exitTransition = d3.transition().duration(transitionDuration).each(function() {
				nodeSelection.exit().remove();
				edgeSelection.exit().remove();
				labelSelection.exit().remove();
				if(clusterSelection) {
					clusterSelection.exit().remove();
				}
			});
			
			updateTransition = exitTransition.transition().each(function() {
				svg
					.attr('width', pointToPixel(graphWidth))
					.attr('height', pointToPixel(graphHeight))
					.attr('style', 'padding:'+innerPadding+'px');
				
				if(clusterSelection) {
					clusterSelection.attr('class', clusterClass);
					
					clusterSelection.selectAll('rect')
						.attr('x', clusterXValue)
						.attr('y', clusterYValue)
						.attr('width', clusterWidthValue)
						.attr('height', clusterHeightValue);
						
					clusterSelection.selectAll('text')
						.text(nodeBindingKey)
						.attr('font-size', fontSize)
						.attr('text-anchor', 'middle')
						.attr('x', xClusterTextValue)
						.attr('y', yClusterTextValue);
				}
				
				nodeSelection.attr('class', nodeClass);
				
				nodeSelection.selectAll('title').data(nil(data['nodes']), nodeBindingKey)
					.text(nodeDisplayClass);
				
				nodeSelection.selectAll('ellipse').data(nil(data['nodes']), nodeBindingKey)
					.transition()
					.attr('cx', xValue)
					.attr('cy', yValue)
					.attr('rx', widthValue)
					.attr('ry', heightValue);
				
				nodeSelection.selectAll('text').data(nil(data['nodes']), nodeBindingKey)
					.text(nodeTextValue)
					.transition()
					.attr('font-size', fontSize)
					.attr('x', xValue)
					.attr('y', yNodeTextValue);
				
				edgeSelection
					.attr('class', edgeClass)
					.attr('d', pathValue);
				
				labelSelection
					.attr('font-size', fontSize)
					.attr('text-anchor', 'middle')
					.attr('x', xValue)
					.attr('y', yNodeTextValue);
			});
			
			enterTransition = updateTransition.transition().each(function() {
				var nodes, clusters;
				
				if(clusterSelection) {
					clusters = clusterSelection.enter()
						.append('g')
						.attr('class', clusterClass);
						
					clusters.append('rect')
						.attr('x', clusterXValue)
						.attr('y', clusterYValue)
						.attr('width', clusterWidthValue)
						.attr('height', clusterHeightValue);
						
					clusters.append('text')
						.text(nodeBindingKey)
						.attr('font-size', fontSize)
						.attr('text-anchor', 'middle')
						.attr('x', xClusterTextValue)
						.attr('y', yClusterTextValue);
				}
				
				nodes = nodeSelection.enter()
					.append('g')
					.attr('class', nodeClass);
					
				nodes.append('title')
					.text(nodeDisplayClass);
								
				nodes.append('ellipse')
					.attr('cx', xValue)
					.attr('cy', yValue)
					.attr('rx', widthValue)
					.attr('ry', heightValue);
					
				nodes.append('text')
					.text(nodeTextValue)
					.attr('font-size', fontSize)
					.attr('text-anchor', 'middle')
					.attr('x', xValue)
					.attr('y', yNodeTextValue);
				
				edgeSelection.enter()
					.append('g')
					.append('path')
					.attr('class', edgeClass)
					.attr('d', pathValue);
				
				labelSelection.enter()
					.append('g')
					.append('text')
					.attr('class', 'label')
					.attr('font-size', fontSize)
					.attr('text-anchor', 'middle')
					.attr('x', xValue)
					.attr('y', yNodeTextValue);
			});
		}
	};
	
	var graph = function(selection) {
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
				className += ' graph';
			} else {
				className = 'graph';
			}
			
			svg.attr('class', className);
			
			refresh(data, 0);
		});
	};
	
	/**
		ACCESSORS
	**/
	graph.refresh = refresh;

	graph.width = function(_) {
		if (!arguments.length) return width;
		width = _ * 1;
		return graph;
	};
	
	return graph;
}