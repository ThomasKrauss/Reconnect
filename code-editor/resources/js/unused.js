var makeTask = function(id, label, selectFunction) {
	var task, taskId, taskLabel, active, selected, onSelect;
	
	// with reSelect, we can force the event onTaskSelected to be fired again even if the task was already selected
	// it is useful to do that because the result returned by the selectFunction may have changed and we want to notify that
	var select = function(reSelect) {
		var result, _reSelect = reSelect ? true : false, selectionDone;
		if(active) {
			result = onSelect.apply(null, []);
			
			if(result) {
				if(selected && !reSelect) {
					selected = false;
					selectionDone = false;
					GlobalEvents.fireEvent('onTaskUnselected', [taskId, result]);
				} else {
					selected = true;
					selectionDone = true;
					GlobalEvents.fireEvent('onTaskSelected', [taskId, result]);
				}
			}
		}
		// Direct actions can be processed in the select function
		// OR once onTaskSelected is fired.
		// In that last case, processes listening to the event will probably call task.fireOnTaskDeactivated
		// and therefore selected will be set to false although we will have done all the selection process!
		// Hence the dissociation of selected and selectionDone
		return selectionDone;
	};
	
	var isActive = function() {
		return active;
	};
	
	var isSelected = function() {
		return selected;
	};
	
	var fireOnTaskActivated = function() {
		if(!active) {
			active = true;
			GlobalEvents.fireEvent('onTaskActivated', [taskId]);
		}
	};
	
	var fireOnTaskDeactivated = function() {
		if(active) {
			active = false;
			if(selected) {
				selected = false;
				GlobalEvents.fireEvent('onTaskUnselected', [taskId]);
			}
			GlobalEvents.fireEvent('onTaskDeactivated', [taskId]);
		}
	};
	
	var updateLabel = function(label) {
		taskLabel = label;
		GlobalEvents.fireEvent('onTaskLabelChanged', [taskId, taskLabel]);
	};
	
	var getId = function() {
		return taskId;
	};
	
	var getLabel = function() {
		return taskLabel;
	};
	
	var reset = function() {
		task = {};
		taskId = id;
		taskLabel = label;
		onSelect = selectFunction;
		
		active = false;
		selected = false;
		
		task.select = select;
		task.isActive = isActive;
		task.isSelected = isSelected;
		task.fireOnTaskActivated = fireOnTaskActivated;
		task.fireOnTaskDeactivated = fireOnTaskDeactivated;
		task.getId = getId;
		task.getLabel = getLabel;
		task.updateLabel = updateLabel;
	};
	reset();
	return task;
};

/*
	Module resource -> per systems (key), per modules (key), get ids
*/
var makeModuleResourceListener = function(resourceName) {
	var moduleResourceListener, resourceData, currentModule, currentSystem, onUpdate, onModuleChanged;
	
	var getResourceDataKey = function(moduleName, systemName) {
		var result = [], _moduleName = moduleName ? moduleName : currentModule, _systemName = systemName ? systemName : currentSystem;
		
		if(resourceData[_systemName] && resourceData[_systemName][_moduleName]) {
			$.each(resourceData[_systemName][_moduleName], function(key, value) {
				if(value) {
					result.push(key);
				}
			});
		}
		
		return result;
	};
	
	var getResourceDataValue = function(moduleName, systemName) {
		var result = [], _moduleName = moduleName ? moduleName : currentModule, _systemName = systemName ? systemName : currentSystem;
		
		if(resourceData[_systemName] && resourceData[_systemName][_moduleName]) {
			$.each(resourceData[_systemName][_moduleName], function(key, value) {
				result.push(value);
			});
		}
		
		return result;
	};
	
	var fireEventsOnModuleChanged = function() {
		$.each(onModuleChanged, function(index, fn) {
			fn.apply(null, []);
		});
	};
	
	var fireEventsOnUpdate = function() {
		$.each(onUpdate, function(index, fn) {
			fn.apply(null, []);
		});
	};
	
	var makeActivator = function(task, activationProcess) {
		return function() {
			var activate = activationProcess.apply(moduleResourceListener, [resourceData, currentModule, currentSystem]);
			
			if(activate) {
				task.updateLabel(activate);
				task.fireOnTaskActivated();
			} else {
				task.fireOnTaskDeactivated();
			}
		};
	};
	
	/*
		A module task activates according to the given activation process which is called on two events: when a new module has been selected for edition and when the resource data has changed.
		
		It's obvious a task at the level of a module has to change on these two events.
		
		When selected, a module task uses the given select function whose result determines if it should be highlighted (and the resulting data be notified) or if it has performed the task directly (result is false).
	*/
	var makeModuleTask = function(id, label, activationProcess, selectFunction) {
		var task, activator;

		task = makeTask(id, label, function() {
			return selectFunction.apply(moduleResourceListener, [resourceData, currentModule, currentSystem]);
		});
		
		activator = makeActivator(task, activationProcess);
		
		onModuleChanged.push(activator);
		onUpdate.push(function() {
			activator();
			// on data update, we want to notify again the result of the select function of the module task if it was already selected
			if(task.isSelected()) {
				task.select(true);
			}
		});
		
		return task;
	};
	
	/*
		A system task activates according to the given activation process only when the resource data has changed. But it is also called once when the system task has been created.
		
		When selected, a system task is never highlighted: it must return a moduleSelection object: {'moduleName', 'systemName'} and the onModuleSelected event will be fired according to these information.
	*/
	var makeSystemTask = function(id, label, activationProcess, selectFunction) {
		var task, activator;

		task = makeTask(id, label, function() {
			var moduleSelection = selectFunction.apply(moduleResourceListener, [resourceData, currentModule, currentSystem]);
			GlobalEvents.fireEvent('onModuleSelected', [moduleSelection.moduleName, moduleSelection.systemName]);
			return false;
		});
		
		activator = makeActivator(task, activationProcess);
		onUpdate.push(activator);
		activator.apply(null, []);
		
		return task;
	};
	
	var reset = function() {
		moduleResourceListener = {};
		resourceData = {};
		onUpdate = [];
		onModuleChanged = [];
		
		moduleResourceListener.getResourceDataKey = getResourceDataKey;
		moduleResourceListener.getResourceDataValue = getResourceDataValue;
		moduleResourceListener.makeModuleTask = makeModuleTask;
		moduleResourceListener.makeSystemTask = makeSystemTask;
		
		GlobalEvents.listenToEvent('onModuleSelected', function(moduleName, systemName) {
			currentModule = moduleName;
			currentSystem = systemName;
			
			fireEventsOnModuleChanged();
		});
		
		EndPoints.on(resourceName, function(json) {
			var data, oldCurrentValue;
			try {
				data = JSON.parse(json);
				if(data.value) {
					if(currentSystem && currentModule && currentSystem === data.key) {
						oldCurrentValue = resourceData[data.key][currentModule];
					}
					resourceData[data.key] = data.value;
					
					if(oldCurrentValue && oldCurrentValue !== data.value[currentModule]) {
						GlobalEvents.fireEvent('onModuleUpdated', [currentModule, currentSystem]);
					}
				}
			} catch(e) {
				resourceData = {};
				console.error('Cannot parse resource '+resourceName+'! Error is: '+e);
			}
			fireEventsOnUpdate();
		});
	};
	reset();
	return moduleResourceListener;
};

var TaskList = React.createClass({
	getInitialState: function() {
		return {'activeTasks': []};
	},
	
	componentDidMount: function() {
		GlobalEvents.listenToEvent('onTaskActivated', this.updateActiveTasks);
		GlobalEvents.listenToEvent('onTaskDeactivated', this.updateActiveTasks);
		GlobalEvents.listenToEvent('onTaskLabelChanged', this.updateLabels);
		
		this.updateActiveTasks();// in case some tasks are active at the very beginning
	},
	
	updateActiveTasks: function() {
		var activeTasks = [];
		$.each(this.props.tasks, function(index, task) {
			if(task.isActive()) {
				activeTasks.push(task);
			}
		});
		this.setState({'activeTasks': activeTasks});
	},
	
	updateLabels: function(taskId) {
		var i = 0, found = false;
		while(i <this.state.activeTasks.length && !found) {
			if(this.state.activeTasks[i].getId() === taskId) {
				found = true;
			}
			i++;
		}
		
		if(found) {
			this.forceUpdate();
		}
	},
	
	doTopTask: function() {
		var done = false, taskDone;
		if(this.state.activeTasks.length > 0) {
			console.log('Do top task '+this.state.activeTasks[0].getId());
			taskDone = this.state.activeTasks[0].select();
			console.log('It returned '+taskDone);
			if(taskDone) {
				this.forceUpdate();
			}
			done = true;
		}
		return done;
	},
	
	render: function() {
		var renderUsualTask = function(task, index) {
			if(index === 0 && task.isSelected()) {
				return <li className='selected'>{task.getLabel()}</li>;
			} else {
				return <li>{task.getLabel()}</li>;
			}
		};
		
		return (<ul>
					{$.map(this.state.activeTasks, renderUsualTask)}
				</ul>);
	}
});

var TaskPack = React.createClass({
	componentDidMount: function() {
		GlobalEvents.listenToEvent('doTopTask', this.doTopTask);
	},
	
	doTopTask: function() {
		var done = false, i = 0;
		console.log('Do the top task of the top task list');
		while(i < this.props.tasks.length && !done) {
			done = this.refs['tasklist'+i].doTopTask();
			console.log('Task list n°'+i+' returned: '+done);
			i++;
		}
		return done;
	},
	
	render: function() {
		return (<ul>
					{$.map(this.props.tasks, function(item, i) {
						return <TaskList tasks={item} ref={'tasklist'+i} />;
					})}
				</ul>);
	}
});

var makeSaveTaskWrapper = function(taskId, saveFn) {
	var wrapper, editors, saveTask;
	
	var save = function() {
		var chunks = [], tmpChunks, savedChunks, identifiedChunks = [];
		
		// collect, save, collect identified chunks, fire code saved
		$.each(editors, function(index, editor) {
			tmpChunks = editor.getChunks();
			if($.isArray(tmpChunks)) {
				chunks = chunks.concat(tmpChunks);
			}
		});
		
		savedChunks = saveFn.apply(null,[chunks]);
		
		if(savedChunks) {
			$.each(editors, function(index, editor) {
				identifiedChunks.push(editor.contentIsSaved(savedChunks[index]));
			});
			
			editors = [];
			saveTask.fireOnTaskDeactivated();
		}
		return savedChunks;
	};
	
	var getTask = function() {
		return saveTask;
	};
	
	var activateTask = function(editor) {
		editors.push(editor);
		saveTask.fireOnTaskActivated();
	};
	
	var deactivateTask = function() {
		saveTask.fireOnTaskDeactivated();
	};
	
	var reset = function() {
		wrapper = {};
		editors = [];
		
		saveTask = makeTask(taskId, 'Save changes', function() {
			return true;
		});
		
		wrapper.getTask = getTask;
		wrapper.save = save;
		wrapper.deactivateTask = deactivateTask;
		wrapper.activateTask = activateTask;
	};
	reset();
	return wrapper;
};