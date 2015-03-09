/** @jsx React.DOM */
$.widget( "custom.mycomplete", $.ui.autocomplete, {
	_create: function() {
		this._super();
		this.widget().menu( "option", "items", "> :not(.systemItem)" );
	},
	_renderMenu: function(ul, items) {
		var renderedSystemName = null, mycompleteWidget = this;
		myMap(items, function(item) {
			var info, itemSystemName, itemModuleName, systemItem;
			
			info = item.value.split(' ');
			itemSystemName = info[0];
			itemModuleName = info[1];
			
			if(renderedSystemName === null || renderedSystemName !== itemSystemName) {
				systemItem = {'label': itemSystemName, 'value': itemSystemName, 'isSystemItem': true};
				renderedSystemName = itemSystemName;
				mycompleteWidget._renderItemData(ul, systemItem);
			}
			
			item.label = itemModuleName;
			mycompleteWidget._renderItemData(ul, item);
		});
	},
	_renderItem: function(ul, item) {
		var listItem = $('<li>')
							.attr('data-value', item.value )
							.append( item.label );
		
		if(item.isSystemItem) {
			listItem.attr('class', 'systemItem');
		}
		
		 return listItem.appendTo(ul);
	}
});
			
var CommandLine = React.createClass({
	componentDidMount: function() {
		var me = this, autoCompleteData, form, input;
		
		/** Internals **/
		var fireOnModuleSelected = function(filename) {
			var path = getPath(filename);
			if(path.systemName && path.moduleName) {
				me.props.options.select.give(path.moduleName, path.systemName);
			}
		};
		
		var getPath = function(filename) {
			var parts = filename.split(' ');
			return {'systemName': parts[0], 'moduleName': parts[1]};
		};
		
		/** Listen to any update of the systems and modules list **/
		EndPoints.on('systems-and-modules-list', function(data) {
			autoCompleteData = data.value;
			if(!isArray(autoCompleteData) || autoCompleteData.length === 0) {
				console.warn('No data for autocompletion in the command line!');
			}
		});
		
		/** Make the JQuery autocomplete widget **/
		form = $(this.getDOMNode());
		input = form.children('input:first');
		input.mycomplete({'autoFocus': true,
			'position': {'my': 'left bottom', 'at': 'left top', 'collision': 'fit'},
			'source': function(request, response) {
				var path = getPath(request.term), trimmedSystems, trimmedModules, result = [];
				
				trimmedSystems = getMatchingSymbols(path.systemName, autoCompleteData, function(data) {
					return data[0];
				});
				
				for(var i=0; i<trimmedSystems.length; i++) {
					trimmedModules = getMatchingSymbols(path.moduleName, trimmedSystems[i][1]);
					for(var j=0; j<trimmedModules.length; j++) {
						result.push(trimmedSystems[i][0]+' '+trimmedModules[j]);
					}
				}
				response(result);
			},
			'select': function() {
				form.submit();
			}});
		
		form.submit(function(e) {
			var timeoutId;
			
			e.preventDefault();
			e.stopPropagation();
			
			// this prevents Enter to be taken into account by other components
			timeoutId = window.setTimeout(function() {
				fireOnModuleSelected(input.val());
				window.clearTimeout(timeoutId);
			}, 10);
		});
		
		this.props.options.focus.take(function() {
			input.val('');
			input.focus();
		});
	},
	
	render: function() {
		return (<form id='commandLine'>
					<input />
				</form>);
	}
});

var makeReactiveMenu = function(endPointName, headerLabel, headerLink, urlGetter) {
	return {getInitialState: function() {
				return {'tree': []};
			},
			
			componentDidMount: function() {
				EndPoints.on(endPointName, this.update);
			},
			
			update: function(data) {
				this.setState({'tree': data.value});
			},
			
			handleClick: function(event) {
				var name, names = [], parentNode;
				
				if(event.target.nodeType === 1 && event.target.tagName.toLowerCase() === 'li') {
					names.push(event.target.firstChild.textContent);
				
					parentNode = event.target.parentNode;
					while(parentNode.id !== 'menu') {
						name = parentNode.nodeType === 1 ? parentNode.tagName.toLowerCase() : null;
						if(name && name === 'li') {
							names.push(parentNode.firstChild.textContent);
						}
						
						parentNode = parentNode.parentNode;
					}
					
					window.location = urlGetter(names.reverse());
				}
			},
			
			render: function() {
				var me = this, selected = this.props.selection ? this.props.selection : [];
				var renderBranch = function(data, previousItems, selected) {
					var result = [];
					myMap(data, function(item) {
						if(isArray(item)) {
							if(selected.length > 0 && item[0] === selected[0]) {
								result.push(<li className="no-hover"><a className="selected" href={urlGetter(previousItems.concat(item[0]))}>{item[0]}</a><ul>{renderBranch(item[1], previousItems.concat(item[0]), selected.slice(1))}</ul></li>);
							} else {
								result.push(<li><a href={urlGetter(previousItems.concat(item[0]))}>{item[0]}</a></li>);
							}
						} else {
							if(selected.length > 0 && item === selected[0]) {
								result.push(<li className="no-hover"><a className="selected" href={urlGetter(previousItems.concat(item))}>{item}</a></li>);
							} else {
								result.push(<li><a href={urlGetter(previousItems.concat(item))}>{item}</a></li>);
							}
						}
					});
					return result;
				};
				
				return (<div id="menu">
							<a href={headerLink}>{headerLabel}</a>
							<ul onClick={this.handleClick}>{renderBranch(this.state.tree, [], selected)}</ul>
						</div>);
			}};
};

var ReactiveGraph = React.createClass({
	getInitialState: function() {
		return {'graph': makeGraph()};
	},
	
	componentDidMount: function() {
		if(this.props.width) {
			this.state.graph.width(this.props.width);
		}
		
		d3.select($(this.getDOMNode()).get(0))
			.datum(this.props.data)
			.call(this.state.graph);
	},
	
	update: function(data) {
		if(this.props.width) {
			this.state.graph.width(this.props.width);
		}
		
		this.state.graph.refresh(data);
	},
	
	componentWillReceiveProps: function(nextProps) {
		this.update(nextProps.data);
	},
	
	render: function() {
		return <svg className={this.props.graphClassName}></svg>;
	}});

var makeConnectedGraph = function(endPointName) {
	return {getInitialState: function() {
				return {'data': {}};
			},
			
			componentDidMount: function() {
				EndPoints.on(endPointName, this.update);
			},
			
			update: function(data) {
				this.setState({'data': data.value});
			},
			
			render: function() {
				return (<div>
							<ReactiveGraph graphClassName="graph" key='0' data={this.state.data} width="1000" />
						</div>);
			}};
};

var sortNodesByDependencyClass = function(nodes) {
	var nodesMap = {};
	
	myMap(nodes, function(value) {
		var lastClass, position, usagesClass, dependenciesClasses;
		
		position = value[1].lastIndexOf(' ');
		lastClass = value[1].slice(position+1);
		if(lastClass === 'ok' || lastClass === 'error' || lastClass === 'warning') {
			usagesClass = lastClass;
			dependenciesClasses = value[1].slice(0, position);
		} else {
			usagesClass = '';
			dependenciesClasses = value[1];
		}
		
		if(!nodesMap[dependenciesClasses]) {
			nodesMap[dependenciesClasses] = [];
		}
		nodesMap[dependenciesClasses].push({'value': value[0], 'className': usagesClass});
	});
	
	return nodesMap;
};

var totalizeStatistics = function(a, b) {
	var result = {};
	var totalize = function(a) {
		myMap(a, function(key, value) {
			result[key] = (result[key] ? result[key] : 0) + value;
		});
	};
	totalize(a);
	totalize(b);
	return result;
};

var renderUsageStatistics = function(data) {
	var element = function(key) {
		var value;
		if(data.total > 0) {
			value = Math.round(data[key] / data.total * 100);
			if(value > 0) {
				return <span className={key}>{value+'%'}</span>;
			}
		}
	};	
	return <div className="usage">{myMap(['ok','error','warning'], element)}</div>;
};

var enlistAsHorizontalList = function() {
	var items = [];
	
	myMap(arguments, function(key, value) {
		if(isArray(value)) {
			myMap(value, function(value) {
				if(value) {
					items.push(React.DOM.li(null, value));
				}
			});
		} else {
			if(value) {
				items.push(React.DOM.li(null, value));
			}
		}
	});
	
	if(items.length > 0) {
		return React.DOM.ul.apply(null, [{"className":"horizontalList"}].concat(items));
	} else {
		return null;
	}
};

var enlistAsVerticalList = function() {
	var items = [];
	
	myMap(arguments, function(key, value) {
		if(isArray(value)) {
			myMap(value, function(value) {
				if(value) {
					items.push(React.DOM.li(null, value));
				}
			});
		} else {
			if(value) {
				items.push(React.DOM.li(null, value));
			}
		}
	});
	
	if(items.length > 0) {
		return React.DOM.ul.apply(null, [{"className":"verticalList"}].concat(items));
	} else {
		return null;
	}
};

var StatListSummary = React.createClass({
	render: function() {
		var me = this, result = enlistAsVerticalList(
				<p>{this.props.title}</p>,
				enlistAsHorizontalList(
					myMap(this.props.data.stats, function(key, value) {
						return enlistAsVerticalList(
							<p>{value + ' ' + key}</p>,
							myMap(me.props.data['in'], function(value) {
								var stat = value.stats[key];
								if(stat && stat > 0) {
									return <p>{value.name + ' ' + stat}</p>;
								}
							}));
					})));
		return result ? result : <ul></ul>;
	}
});

var TableSummary = React.createClass({
	getInitialState: function() {
		return {'data': {}};
	},
	
	componentDidMount: function() {
		var me = this;
		myMap(this.props.endPointNames, function(endPointName) {
			EndPoints.on(endPointName, function(data) {
				return me.update(data, endPointName);
			});
		});
	},
	
	update: function(data, key) {
		this.state.data[this.baseEndPointName(key)] = data.value;
		this.setState({'data': this.state.data});
	},
	
	isDataExcluded: function(endPointName, dataKey) {
		return this.props.exclude && isArray(this.props.exclude[endPointName]) && this.props.exclude[endPointName].indexOf(dataKey) !== -1;
	},
	
	baseEndPointName: function(endPointName) {
		var index = endPointName.indexOf('/');
		return index === -1 ? endPointName : endPointName.slice(0, index);
	},
	
	render: function() {
		var me = this, firstHeader = [], secondHeader = [], headerSpans = {}, dataKeys = [], endPointNames;
		
		endPointNames = myMap(this.props.endPointNames, this.baseEndPointName);
		myMap(endPointNames, function(endPointName) {
			var span = 0, headerKeys = [], display = false, data;
			
			if(me.state.data[endPointName]) {
				data = me.state.data[endPointName];
				
				myMap(data.stats, function(key, value) {
					if(!me.isDataExcluded(endPointName, key) && value > 0) {
						display = true;
						headerKeys.push([endPointName, key]);
						span++;
					}
				});
				
				if(display) {
					firstHeader.push(endPointName);
					secondHeader = secondHeader.concat(headerKeys);
					headerSpans[endPointName] = span;
					
					myMap(data['in'], function(value) {
						display = false;
						myMap(value.stats, function(key, value) {
							display = display || (!me.isDataExcluded(endPointName, key) && value > 0);
						});
						
						if(display && dataKeys.indexOf(value.name) === -1) {
							dataKeys.push(value.name);
						}
					});
				}
			}
		});
		
		var header = function() {
			var firstHeaderName;
			return (<thead>
						<tr>
							<th colSpan='1'></th>
							{myMap(firstHeader, function(value, index) {
								return <th colSpan={headerSpans[value]}>{value}</th>;
							})}
						</tr>
						<tr>
							<th></th>
							{myMap(secondHeader, function(value) {
								if(value[0] !== firstHeaderName) {
									className = "first-data-cell";
									firstHeaderName = value[0];
								}
								return <th>{value[1]}</th>;
							})}
						</tr>
					</thead>);
		};
		
		var dataRow = function(dataKey) {
			var firstHeaderName;
			return myMap(secondHeader, function(value) {
				var data, pos, i, endPointName, statKey, className;
				endPointName = value[0];
				statKey = value[1];
				data = me.state.data[endPointName]['in'];
				
				i = 0;
				pos = -1;
				while(pos === -1 && i < data.length) {
					if(data[i].name === dataKey) {
						pos = i;
					}
					i++;
				}
				
				if(pos > -1 && data[pos].stats && data[pos].stats[statKey] && data[pos].stats[statKey] > 0) {
					data = data[pos].stats[statKey];
				} else {
					data = null;
				}
				
				className = statKey;
				if(endPointName !== firstHeaderName) {
					className += " first-data-cell";
					firstHeaderName = endPointName;
				}
				return <td className={className}>{data}</td>;
			});
		};
		
		/*var result = enlistAsHorizontalList(myMap(this.state.data, function(endPointName, data) {
			return <StatListSummary title={endPointName} data={data} />;
		}));
		return result ? result : <table></table>;
		*/
		
		return (<table>
					{header()}
					<tbody>
						{myMap(dataKeys, function(dataKey) {
							return (<tr>
										<td className="first-cell">{dataKey}</td>
										{dataRow(dataKey)}
									</tr>);
						})}
					</tbody>
				</table>);
	}
});

var createNamedData = function(names, value) {
	var result, i, _names;
	
	_names = names.reverse();
	result = value;
	for(i=0; i<_names.length; i++) {
		result = {'name': names[i], 'in': [result]};
	}
	
	return result;
};

/*
	The given named data is merged into place, an array of named data, following these rules:
	- if no data of the same name appeared in place, it is appended to it.
	- else it is merged with the existing data of the same name:
	  - the value of any new property is inserted
	  - the value of any property not appearing in the given data is left untouched
	  - for values of existing properties that do appear in the given data, two cases:
		1) at least of value among the existing and the new ones is not an array: the existing value is overridden
		2) else it is assumed that both of them hold named objects and we merge them with dataMerge
*/
var dataMerge = function(data, place) {
	var position = -1, i = 0;
	var result, mergedObject;
	while(i < place.length && position === -1) {
		if(place[i].name === data.name) {
			position = i;
		}
		i++;
	}
	
	if(position === -1) {
		result = place.concat(data);
	} else {
		result = clone(place);
		mergedObject = {};
		// keep existing values as is when not appearing in the given data or override/merge (if array values)
		myMap(place[position], function(key, value) {
			if(data.hasOwnProperty(key)) {
				if(isArray(data[key]) && isArray(value)) {
					mergedObject[key] = clone(value);
					myMap(data[key], function(value) {
						mergedObject[key] = dataMerge(value, mergedObject[key]);
					});
				} else {
					mergedObject[key] = data[key];
				}
			} else {
				mergedObject[key] = value;
			}
		});
		// the rest should be the given data (new or override existing)
		myMap(data, function(key, value) {
			if(!place[position].hasOwnProperty(key)) {
				mergedObject[key] = data[key];
			}
		});
		result[position] = mergedObject;
	}
	return result;
};

var findNamedData = function(keys, place) {
	var result, i = 0, found;
	
	if(place && keys.length > 0) {
		while(!found && i<place.length) {
			if(place[i].name === keys[0]) {
				found = place[i];
			}
			i++;
		}
		
		if(found) {
			if(keys.length === 1) {
				result = found;
			} else {
				result = findNamedData(keys.slice(1), found['in']);
			}
		}
	}
	
	return result;
}

var flattenEntries = function(arr, insertDetailingEntry) {
	var result, tmp;
	
	if(isArray(arr) && arr.length > 0 && isArray(arr[0])) {
		result = [];
		myMap(arr, function(value) {
			var key = value[0];
			tmp = flattenEntries(value[1], insertDetailingEntry);
			if(insertDetailingEntry) {
				tmp = [[]].concat(tmp);
			}
			result = result.concat(myMap(tmp, function(value) {
				return [key].concat(value);
			}));
		});
	} else {
		result = arr;
	}
	
	return result;
};

var ProblemMap = React.createClass({
	getInitialState: function() {
		return {'endPointNames': ['compile-problems','usages'], 'exclude': {'usages':['ok']}, 'systems':[], 'data': [], 'focus': {}};
	},
	
	componentDidMount: function() {
		var me = this;
		EndPoints.on('systems-and-modules-list', function(data) {
			return me.updateSystems(data);
		});
		EndPoints.onAction('focus', function(params) {
			return me.updateFocus(params);
		});
		myMap(this.state.endPointNames, function(endPointName) {
			EndPoints.on(endPointName+'/1', function(data) {
				return me.update(data, endPointName);
			});
		});
	},
	
	updateSystems: function(data) {
		this.setState({'systems': flattenEntries(data.value, true)});
	},
	
	updateFocus: function(params) {
		this.setState({'focus': {'systemName': params.systemName, 'moduleName': params.moduleName}});
	},
	
	update: function(data, key) {
		if(data.keys.length > 0) {
			this.state.data = dataMerge( createNamedData([key].concat(data.keys.slice(0,data.keys.length-1)), data.value), this.state.data);
		} else {
			data.value.name = key;
			this.state.data = dataMerge( data.value, this.state.data);
		}
		this.setState({'data': this.state.data});
	},
	
	isDataExcluded: function(endPointName, dataKey) {
		return isArray(this.state.exclude[endPointName]) && this.state.exclude[endPointName].indexOf(dataKey) !== -1;
	},
	
	baseEndPointName: function(endPointName) {
		var index = endPointName.indexOf('/');
		return index === -1 ? endPointName : endPointName.slice(0, index);
	},
	
	render: function() {
		var me = this, firstHeader = [], secondHeader = [], headerSpans = {}, entries = [], rowNumber = 0;
		
		// end points eligibility
		myMap(this.state.endPointNames, function(endPointName) {
			var span = 0, headerKeys = [], display = false, data;
			
			data = findNamedData([endPointName],me.state.data);
			if(data) {
				myMap(data.stats, function(key, value) {
					if(!me.isDataExcluded(endPointName, key) && value > 0) {
						display = true;
						headerKeys.push([endPointName, key]);
						span++;
					}
				});
				
				if(display) {
					firstHeader.push(endPointName);
					secondHeader = secondHeader.concat(headerKeys);
					headerSpans[endPointName] = span;
				}
			}
		});
		
		// data eligibility
		var ignoreSystem;
		myMap(me.state.systems, function(dataKeys) {
			var display = false;
			if(!(ignoreSystem && ignoreSystem === dataKeys[0])) {
				myMap(firstHeader, function(endPointName) {
					var data;
					if(!display) {
						data = findNamedData([endPointName].concat(dataKeys), me.state.data);
						if(data) {
							myMap(data.stats, function(key, value) {
								display = display || (!me.isDataExcluded(endPointName, key) && value > 0);
							});
						}
					}
				});
				
				if(display) {
					entries.push(dataKeys);
					ignoreSystem = null;
				} else if(dataKeys.length === 1) {
					ignoreSystem = dataKeys[0];
				}
			}
		});
		
		var header = function() {
			var firstHeaderName;
			return (<thead>
						<tr>
							<th></th>
							<th></th>
							{myMap(firstHeader, function(value, index) {
								return <th colSpan={headerSpans[value]}>{value}</th>;
							})}
						</tr>
						<tr>
							<th></th>
							<th></th>
							{myMap(secondHeader, function(value) {
								if(value[0] !== firstHeaderName) {
									className = "first-data-cell";
									firstHeaderName = value[0];
								}
								return <th>{value[1]}</th>;
							})}
						</tr>
					</thead>);
		};
		
		var dataRow = function(dataKeys) {
			var firstHeaderName, result = [], className;
			
			switch(dataKeys.length) {
				case 1:
					if(rowNumber > 0) {
						className = 'first-data-row strong';
					} else {
						className = 'strong';
					}
					result.push(<td>{dataKeys[0]}</td>);
					result.push(<td></td>);
					break;
					
				case 2:
					result.push(<td></td>);
					if(dataKeys[0] === me.state.focus.systemName && dataKeys[1] === me.state.focus.moduleName) {
						className = 'highlight';
					} else {
						className = '';
					}
					result.push(<td>{dataKeys[1]}</td>);
					break;
			}
			
			result = result.concat(myMap(secondHeader, function(value) {
				var data, endPointName, statKey, className;
				endPointName = value[0];
				statKey = value[1];
				data = findNamedData([endPointName].concat(dataKeys), me.state.data);
				
				// data.stats may not exist because the data has not arrived yet!
				if(data && data.stats && data.stats[statKey] > 0) {
					data = data.stats[statKey];
				} else {
					data = '';
				}
				
				className = statKey;
				if(endPointName !== firstHeaderName) {
					className += " first-data-cell";
					firstHeaderName = endPointName;
				}
				return <td className={className}>{data}</td>;
			}));
			
			rowNumber++;
			return <tr className={className}>{result}</tr>;
		};
		
		return (<table>
					{header()}
					<tbody>
						{myMap(entries, dataRow)}
					</tbody>
				</table>);
	}
});

var MessageBox = React.createClass({
	render: function() {
		var me = this;
		var renderMessages = function() {
			var result = [];
			
			if(me.props.messages && me.props.messages.length > 0) {
				myMap(me.props.messages, function(value) {
					var details = [];
					
					myMap(value, function(key, value) {
						if(key !== 'id') {
							if(isArray(value) && value.length > 0) {
								details = details.concat(myMap(value, function(value) {
									return <li className={key}>{value}</li>;
								}));
							}
						}
					});
					
					if(details.length > 0) {
						result.push(<li><ul><li className='header'>{value.id}</li>{details}</ul></li>);
					}
				});
			}
			
			return result;
		};
		
		return (<ul id='messages'>
					{renderMessages()}
				</ul>);
	}
});

var TaskBar = React.createClass({
	getInitialState: function() {
		return {'endPointNames': ['compile-problems','usages'],
				'eligibleEndPointNames': [],
				'exclude': {'usages':['ok']},
				'systems':[],
				'data': [],
				'entries': {},
				'focus': false};
	},
	
	componentDidMount: function() {
		var me = this;
		EndPoints.on('systems-and-modules-list', function(data) {
			me.updateSystems(data);
		});
		myMap(this.state.endPointNames, function(endPointName) {
			EndPoints.on(endPointName+'/1', function(data) {
				return me.updateData(data, endPointName);
			});
		});
		this.props.options.activate.take(this.activateTopMaintenanceTask);
		this.props.options.deactivate.take(this.deactivateTopMaintenanceTask);
	},
	
	updateSystems: function(data) {
		this.setState({'systems': flattenEntries(data.value, true)});
		
		if(this.state.data.length > 0) {
			this.update();
		}
	},
	
	updateData: function(data, key) {
		if(data.keys.length > 0) {
			this.state.data = dataMerge(createNamedData([key].concat(data.keys.slice(0,data.keys.length-1)), data.value), this.state.data);
		} else {
			data.value.name = key;
			this.state.data = dataMerge(data.value, this.state.data);
		}
		
		if(this.state.systems.length > 0) {
			this.update();
		}
	},
	
	update: function() {
		var eligibleResult, focus = null, me = this;
		var endPointName, moduleName, systemName;
		var focusUnchanged;
		
		eligibleResult = this.computeEligibility(this.state.data);
		
		if(this.state.focus) {
			item = this.nextItem(eligibleResult.eligibleEndPointNames, eligibleResult.entries);
			
			if(item) {
				endPointName = item.endPointName;
				systemName = item.entry[0];
				moduleName = item.entry[1];
				
				focus = [endPointName, systemName, moduleName];
				focusUnchanged = endPointName === this.state.focus[0]
					&& systemName === this.state.focus[1]
					&& moduleName === this.state.focus[2];
			} else {
				focus = null;
				focusUnchanged = false;
			}
			
			if(isFunction(this.props.options.onTaskChange) && !focusUnchanged) {
				this.props.options.onTaskChange(item);
			}
		}
		
		this.setState({'data': this.state.data,
						'eligibleEndPointNames': eligibleResult.eligibleEndPointNames,
						'entries': eligibleResult.entries,
						'focus': focus});
	},
	
	computeEligibility: function(data) {
		var me = this, eligibleEndPointNames = [], entries = {}, ignoreSystem = {};
		
		myMap(this.state.endPointNames, function(endPointName) {
			var span = 0, headerKeys = [], display = false, value;
			
			value = findNamedData([endPointName], data);
			if(value) {
				myMap(value.stats, function(key, value) {
					if(!me.isDataExcluded(endPointName, key) && value > 0) {
						display = true;
						headerKeys.push([endPointName, key]);
						span++;
					}
				});
				
				if(display) {
					eligibleEndPointNames.push(endPointName);
					entries[endPointName] = [];
				}
			}
		});
		
		myMap(me.state.systems, function(dataKeys) {
			myMap(eligibleEndPointNames, function(endPointName) {
				var data, display = false;
				
				if(!(ignoreSystem[endPointName] && ignoreSystem[endPointName] === dataKeys[0])) {
					data = findNamedData([endPointName].concat(dataKeys), me.state.data);
					if(data) {
						myMap(data.stats, function(key, value) {
							display = display || (!me.isDataExcluded(endPointName, key) && value > 0);
						});
						
						if(display) {
							entries[endPointName].push(dataKeys);
							ignoreSystem[endPointName] = null;
						} else if(dataKeys.length === 1) {
							ignoreSystem[endPointName] = dataKeys[0];
						}
					}
				}
			});
		});
		
		return {'eligibleEndPointNames': eligibleEndPointNames, 'entries': entries};
	},
	
	isDataExcluded: function(endPointName, dataKey) {
		return isArray(this.state.exclude[endPointName]) && this.state.exclude[endPointName].indexOf(dataKey) !== -1;
	},
	
	baseEndPointName: function(endPointName) {
		var index = endPointName.indexOf('/');
		return index === -1 ? endPointName : endPointName.slice(0, index);
	},
	
	nextItem: function(eligibleEndPointNames, entries) {
		var item, endPointName;
		
		if(eligibleEndPointNames.length > 0) {
			endPointName = eligibleEndPointNames[0];
			if(entries[endPointName].length > 1) {
				item = {'endPointName': endPointName, 'entry': entries[endPointName][1]};
			}
		}
		
		return item;
	},
	
	activateTopMaintenanceTask: function() {
		var item = this.nextItem(this.state.eligibleEndPointNames, this.state.entries);
		var endPointName, moduleName, systemName;
		
		if(item) {
			endPointName = item.endPointName;
			systemName = item.entry[0];
			moduleName = item.entry[1];
		}
		
		this.focus(moduleName, systemName, endPointName);
		
		return item;
	},
	
	deactivateTopMaintenanceTask: function() {
		this.focus();
	},
	
	focus: function(moduleName, systemName, endPointName) {
		if(moduleName && systemName && endPointName) {
			this.setState({'focus': [endPointName, systemName, moduleName]});
		} else {
			this.setState({'focus': null});
		}
	},
	
	render: function() {
		var focus = this.state.focus, entries = this.state.entries;
		
		var listEntry = function(endPointName) {
			var result, i, list, item, lastHeaderName;
			var pushInResult = function(headerName) {
				if(focus && focus[0] === endPointName && focus[1] === lastHeaderName) {
					result.push(<li><ul className='selected'>{list}</ul></li>);
				} else {
					result.push(<li><ul>{list}</ul></li>);
				}
				if(headerName) {
					lastHeaderName = headerName;
					list = [<li className='header'>{lastHeaderName}</li>];
				}
			};
			
			if(isArray(entries[endPointName]) && isArray(entries[endPointName][0])) {
				result = [<li className='endPointName'>{endPointName}</li>];
				lastHeaderName = entries[endPointName][0][0];
				list = [<li className='header'>{lastHeaderName}</li>];
				
				for(i=1; i<entries[endPointName].length; i++) {
					item = entries[endPointName][i];
					switch(item.length) {
						case 1:
							pushInResult(item[0]);
							break;
						
						case 2:
							if(focus && focus[0] === endPointName && focus[1] === item[0] && focus[2] === item[1]) {
								list.push(<li className='focused'>{item[1]}</li>);
							} else {
								list.push(<li>{item[1]}</li>);
							}
							break;
					}
				}
				pushInResult();
			}
			
			return result;
		};
		
		return <ul>{myMap(this.state.eligibleEndPointNames, function(endPointName) { return listEntry(endPointName); })}</ul>;
	}
});

var UseTable = React.createClass({
	getInitialState: function() {
		return {'data': {}};
	},
	
	componentDidMount: function() {
		EndPoints.on(this.props.endPointName, this.update);
		$(window).resize(this.resizeTableHeader);
	},
	
	update: function(data) {
		this.setState({'data': data.value});
	},
	
	resizeTableHeader: function() {
		var table, columnWidths;
		
		table = $(this.getDOMNode());
		columnWidths = table.find('tbody tr:first').children().map(function() {
			return $(this).width();
		}).get();
		
		table.find('thead tr').children().each(function(index, value) {
			$(value).width(columnWidths[index]);
		});    
	},
	
	componentDidUpdate: function() {
		this.resizeTableHeader();
	},
	
	render: function() {
		var me = this, headerLabels = [], values = {};
		
		myMap(this.state.data, function(value) {
			var data = {};
			
			myMap(value['in'], function(value) {
				data[value.name] = value['in'];
				if(headerLabels.indexOf(value.name) === -1) {
					headerLabels.push(value.name);
				}
			});
			
			values[value.name] = data;
		});
		
		var header = function() {
			return (<thead>
						<tr>
							<th></th>
							{myMap(headerLabels, function(value, index) {
								return <th>{value}</th>;
							})}
						</tr>
					</thead>);
		};
		
		var dataRow = function(value) {
			var firstHeaderName;
			return myMap(headerLabels, function(label, index) {
				var className = index === 0 ? "first-data-cell" : "";
				return (<td className={className}>
							<ul className="verticalList">
								{myMap(value[label], function (value) { return <li>{value}</li>; })}
							</ul>
						</td>);
			});
		};
		
		return (<table className="scroll">
					{header()}
					<tbody style={{'height': this.props.height ? this.props.height : 300}}>
						{myMap(values, function(key, value) {
							return (<tr>
										<td className="first-cell">{key}</td>
										{dataRow(value)}
									</tr>);
						})}
					</tbody>
				</table>);
	}
});

var makeDependencyDashboard = function(endPointQualification) {
	var realEndPointQualification = endPointQualification ? '/'+endPointQualification : '';
	return {getInitialState: function() {
				return {'dependencies': {}, 'LoC': {}};
			},
			
			componentDidMount: function() {
				EndPoints.on('dependencies'+realEndPointQualification, this.updateDependencies);
			},
			
			updateDependencies: function(data) {
				this.setState({'dependencies': data.value});
			},
			
			render: function() {
				var result;
				var renderLoCSummary = function() {
					return (<ConnectedSmallMultipleBarChart
								endPointName={'line-and-form-counts'+realEndPointQualification}
								dataKeys={['line-count','form-count','line-form-ratio']}
								width="300" barHeight="20" tickCount="3" xLabelHeight="12" />);
				};
				
				var renderSummary = function() {
					var endPointNames = myMap(['compile-problems','usages'], function (endPointName) {
						return endPointName + realEndPointQualification;
					});
					return enlistAsHorizontalList(
								renderLoCSummary(),
								<TableSummary endPointNames={endPointNames} exclude={{'usages':['ok']}} />);
				};
				
				var renderSingles = function(data) {
					var singlesMap, classNames, header, list;
					
					classNames = ['wide exported','narrow exported','unused exported','exported',
									'wide to-be-used','narrow to-be-used','to-be-used',
									'unused wide','unused narrow','unused',
									'wide','narrow','end',
									''];
									
					header = function(className) {
						var description = className !== '' ? className : 'default';
						return enlistAsHorizontalList(
									(<span className={className}>
										{data.stats[className].total+' '+description}
									</span>),
									renderUsageStatistics(data.stats[className]));
					};
					
					list = function(className) {
						if(data && data.stats && data.stats[className]) {
							return enlistAsVerticalList(
								header(className),
								myMap(singlesMap[className], function(node) {
									return <span className={node.className}>{node.value}</span>;
								}));
						}
					};
					
					singlesMap = data ? sortNodesByDependencyClass(data.nodes) : {};
					return enlistAsHorizontalList(myMap(classNames, list));
				};
				
				var renderComponent = function(data) {
					var result = [];
					
					var header = function(data) {
						return enlistAsHorizontalList(
								<span>{data.total + ' action' + ((data.total>1) ? 's' : '')}</span>,
								renderUsageStatistics(data));
					};
					
					var list = function(data) {
						var maxValue = -1;
						var chartData = myMap(data.stats, function(key, value) {
							if(value.total > maxValue) {
								maxValue = Math.min(4, value.total);
							}
							
							return [key, value.total];
						}).sort(function(a,b) { return d3.descending(a[1], b[1]); });
						
						return enlistAsVerticalList(
									header(myReduce(data.stats, totalizeStatistics)),
									<BarChart data={chartData} width="300" barHeight="20" tickCount={maxValue} xLabelHeight="12" />);
					};
					
					if(isArray(data)) {
						result = myMap(data, function(value, index) {
							return enlistAsHorizontalList(
											list(value),
											<ReactiveGraph graphClassName="graph" key={index} data={value} />,
											null, null, null);
						});
					}
					return result;
				};
				
				result = enlistAsVerticalList(
							renderSummary(),
							renderSingles(this.state.dependencies.singles),
							enlistAsVerticalList(renderComponent(this.state.dependencies.graphs)),
							endPointQualification ? <UseTable endPointName={'use'+realEndPointQualification} /> : null);
				return result ? result : <ul></ul>;
			}};
};

var makeDependencyGraphLegend = function() {
	var html = '';
	
	var ellipse = function(className) {
		return '<svg width="80" height="30"><g class="node '+className+'"><ellipse cx="35" cy="15" rx="30" ry="12"/><text font-size="14" text-anchor="middle" x="35" y="18">do</text></g></svg>';
	};
	
	var row = function(className, description) {
		return '<tr><td>'+ellipse(className)+'</td><td style="text-align: left">'+description+'</td></tr>';
	};
	
	html += '<tbody>';
	html += row('', 'the default: unexported, used with relevance limited to its module');
	html += row('narrow', 'relevance up to the system');
	html += row('wide', 'global relevance');
	html += row('exported', 'exported: meant to have global relevance');
	html += row('to-be-used', 'to be used: unexported and part of an unused chain with an exported action ahead of it');
	html += row('unused', 'unused, either truly unused or part of an unused chain with no exported action ahead of it');
	html += '</tbody>';
	
	return '<table>'+html+'</table>';
};

var makeHtmlFragment = function(domElement, endPointName) {
	var htmlFragment;
	
	var refresh = function(data) {
		if(data && data.value && data.value.html) {
			$(domElement).html(data.value.html);
		} else {
			$(domElement).empty();
		}
	};
	
	var reset = function() {
		htmlFragment = {};
		EndPoints.on(endPointName, refresh);
	};
	reset();
	return htmlFragment;
};