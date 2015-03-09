/** @jsx React.DOM */

var Timer;

/**
	SAVE & LOAD UTILITIES
**/
function ensureDirectoryExists(path) {
	try {
		require('fs').mkdirSync(path);
	} catch(e) {
		// EEXIST means the directory already exists, which is fine given our purpose is to ensure it does.
		if(e.code !== 'EEXIST') {
			console.error(e);
		}
	}
	return path;
}

function save(path, data) {
	require('fs').writeFileSync(path, JSON.stringify(data), {'encoding': 'utf8'});
	return data;
}

function load(path) {
	var content, result, attrs = [];
	try {
		content = require('fs').readFileSync(path, {'encoding': 'utf8'})
		result = JSON.parse(content);
	} catch(e) {
		result = [];
		
		// ENOENT means the file does not exist but it is not a problem for us here.
		if(e.code !== 'ENOENT') {
			console.error('Problem thrown while loading: '+attrs);
		}
	}
	return result;
}

var storeDirectory = 'c:/home/thomas/work/my-utilities/pomodoro';

function today() {
	var now = new Date(), month, day;
	
	month = (now.getMonth()+1) + '';
	if(month.length === 1) {
		month = '0'+month;
	}
	
	day = now.getDate() + '';
	if(day.length === 1) {
		day = '0'+day;
	}
	
	return now.getFullYear() + '-' + month + '-' + day;
}

function yesterday() {
	var now = new Date(), year, month, day;
	var dayPerMonth = [31,28,31,30,31,30,31,31,30,31,30,31];
	
	year = now.getFullYear();
	month = (now.getMonth()+1);
	day = now.getDate() - 1;
	
	if((year % 4 === 0 && year % 100 !== 0) || year % 400 === 0) {
		dayPerMonth[1]++;
	}
	
	if(day === 0) {
		month--;
		if(month === 0) {
			year--;
			month = 12;
			day = 31;
		} else {
			day = dayPerMonth[month-1];
		}
	}
	
	month += '';
	if(month.length === 1) {
		month = '0'+month;
	}
	
	day += '';
	if(day.length === 1) {
		day = '0'+day;
	}
	
	return year + '-' + month + '-' + day;
}

function loadDayTasks(date) { return load(storeDirectory+'/' + date + '-todays-tasks.json'); }
function loadTodaysTasks() { return loadDayTasks(today()); }
function saveDayTasks(date, data) { return save(storeDirectory+'/' + date + '-todays-tasks.json', data); }
function saveTodaysTasks(data) { return saveDayTasks(today(), data); }
function loadActivityInventory() { return load(storeDirectory+'/activity-inventory.json'); }
function saveActivityInventory(data) { return save(storeDirectory+'/activity-inventory.json', data); }

/**
	UTILITIES (searching annotations, archiving, computing statistics)
**/
function makeTask(label) {
	return {'label': label, 'pomodoro': 0, 'interrupt': 0, 'done': false};
}

function isTaskAnnotated(task) {
	return task.pomodoro !== 0 || task.interrupt !== 0;
}

function indexOfTaskOfLabel(label, tasks) {
	var i = 0, position = -1;
	
	while(position === -1 && i < tasks.length) {
		if(tasks[i].label === label) {
			position = i;
		}
		i++;
	}
	
	return position;
}

function taskOfLabel(label, tasks) {
	var i = 0, value;
	
	while(!value && i < tasks.length) {
		if(tasks[i].label === label) {
			value = tasks[i];
		}
		i++;
	}
	
	return value;
}

function modifyTaskLabel(oldLabel, newLabel, tasks) {
	var position = indexOfTaskOfLabel(oldLabel, tasks);
	
	if(position !== -1) {
		tasks[position].label = newLabel;
	}
	
	return tasks;
}

/**
	The inventory is trimmed of all tasks that are done.
	Today's tasks is set to all the tasks yet to be finished of the most recent day.
	Return the today's tasks.
**/
function initializeTodaysTasks() {
	var task, lastDayTasks, i;
	var todaysTasks = [], newInventory = [];
	
	lastDayTasks = loadDayTasks(getTaskDates().reverse()[0]);
	activityInventory = loadActivityInventory();
	
	for(i=0; i<activityInventory.length; i++) {
		task = taskOfLabel(activityInventory[i].label, lastDayTasks);
		
		if(!task || !task.done) {
			newInventory.push(activityInventory[i]);
			if(task) {
				todaysTasks.push(makeTask(activityInventory[i].label));
			}
		}
	}
	
	saveActivityInventory(newInventory);
	
	return saveTodaysTasks(todaysTasks);;
}

function getTaskDates() {
	var files, match, dates = [], i;
	var dateRegexp = /(\d{4}-\d{2}-\d{2})-todays-tasks\.json/;
	
	files = require('fs').readdirSync(storeDirectory);
	for(i=0; i<files.length; i++) {
		match = dateRegexp.exec(files[i]);
		if(match) {
			dates.push(match[1]);
		}
	}
	
	return dates.sort();
}

function getStatistics() {
	var annotations, currentAnnotations, dates, tasks, i, j;
	var stats = [], pomodoro, interrupt, done, planned;
	
	dates = getTaskDates();
	
	for(i=0; i<dates.length; i++) {
		tasks = loadDayTasks(dates[i]);
		pomodoro = 0;
		interrupt = 0;
		done = 0;
		planned = 0;
		
		for(j=0; j<tasks.length; j++) {
			if(tasks[j].pomodoro) { pomodoro += tasks[j].pomodoro; }
			if(tasks[j].interrupt) { interrupt += tasks[j].interrupt; }
			if(tasks[j].done) { done++; }
			planned++;
		}
		
		stats.push({'date': dates[i], 'pomodoro': pomodoro, 'interrupt': interrupt, 'done': done, 'planned': planned});
	}
	
	return stats;
}

/**
	REACT COMPONENTS
**/
// the wish list display the tasks I wish to be done
var WishList = React.createClass({
	getInitialState: function() {
		return {'label': null, 'newLabelValue': null};
	},

	handleInputChange: function(e) {
		this.setState({'newLabelValue': e.target.value});
	},
	
	resetNewValue: function(label) {
		if(this.state.label === label) {
			this.setState({'label': null, 'newLabelValue': null});
		}
	},
	
	submitNewValue: function(e) {
		e.preventDefault();
		e.stopPropagation();
		this.props.handleChange(this.state.label, this.state.newLabelValue);
		this.setState({'label': null, 'newLabelValue': null})
	},
	
	editInPlace: function(label) {
		this.setState({'label': label, 'newLabelValue': label});
	},
	
	createTextItem: function(task) {
		var result;
		
		if(isTaskAnnotated(task)) {
			if(task.done) {
				result = <li className="done">{task.label}</li>;
			} else {
				result = <li className="editable" onClick={this.editInPlace.bind(this, task.label)}><span>{task.label}</span></li>;
			}
		} else {
			result = <li className="editable" onClick={this.editInPlace.bind(this, task.label)}><span>{task.label}</span> [<a href="#" onClick={this.props.handleDelete.bind(this, task.label)}>x</a>]</li>;
		}
		
		return result;
	},
	
	createInputItem: function(label) {
		return <form onSubmit={this.submitNewValue}><input id="changeLabel" value={this.state.newLabelValue} onChange={this.handleInputChange} onBlur={this.resetNewValue.bind(this, label)} /></form>;
	},

	createItem: function(task) {
		var result;
		
		if(task.label === this.state.label) {
			result = this.createInputItem(task.label);
		} else {
			result = this.createTextItem(task);
		}
		
		return result;
	},
	
	render: function() {
		return <ul>{this.props.tasks.map(this.createItem)}</ul>;
	},
	
	componentDidUpdate: function() {
		$('#changeLabel').focus();
	}
});

// the wish task manager helps managing (creating, modifying and deleting) the tasks I wish to be done
var WishTaskManager = React.createClass({
	getInitialState: function() {
		return {'tasks': this.props.loadFunction(), 'text': ''};
	},
	
	handleDelete: function(label, e) {
		this.state.tasks.splice(indexOfTaskOfLabel(label, this.state.tasks), 1);
		this.setState({'tasks': this.state.tasks});
	},
	
	handleChange: function(oldLabel, newLabel) {
		this.setState({'tasks': modifyTaskLabel(oldLabel, newLabel, this.state.tasks)});
	},
	
	handleInputChange: function(e) {
		this.setState({'text': e.target.value});
	},

	handleSubmit: function(e) {
		e.preventDefault();
		if(this.state.text !== '') {
			var nextItems = this.state.tasks.concat([makeTask(this.state.text)]);
			this.setState({'tasks': nextItems, 'text': ''});
		}
	},
	
	internalDoneHandler: function() {
		this.props.saveFunction(this.state.tasks);
		this.props.doneHandler();
	},
	
	makeDoneButton: function() {
		if(this.state.tasks.length === 0) {
			return <button className="doneButton" disabled>Done</button>;
		} else {
			return <button className="doneButton" onClick={this.internalDoneHandler}>Done</button>;
		}
	},

	render: function() {
		return (
		  <div id="wishTaskManager">
			<h3>{this.props.title}</h3>
			<div className="content" style={{'height': '70%'}}>
				<WishList tasks={this.state.tasks} handleChange={this.handleChange} handleDelete={this.handleDelete} />
			</div>
			<form onSubmit={this.handleSubmit}>
			  <input onChange={this.handleInputChange} value={this.state.text} />
			  <button>'Enlist task'</button>
			</form>
			{this.makeDoneButton()}
		  </div>
		);
	}
});

// the global wish task manager allows to edit either the activity inventory or the today's tasks list
var GlobalWishTaskManager = React.createClass({
	getInitialState: function() {
		var todaysTasks = loadTodaysTasks();
		return {'tasks': todaysTasks, 'text': '', 'target': 'today', 'title': "Today's tasks",
				'todaysTasks': todaysTasks, 'activityInventory': loadActivityInventory()};
	},
	
	handleDelete: function(label, e) {
		this.state.tasks.splice(indexOfTaskOfLabel(label, this.state.tasks), 1);
		this.setState({'tasks': this.state.tasks});
	},
	
	handleChange: function(oldLabel, newLabel) {
		var tasks, activityInventory, todaysTasks;
		
		tasks = modifyTaskLabel(oldLabel, newLabel, this.state.tasks);
		todaysTasks = modifyTaskLabel(oldLabel, newLabel, this.state.todaysTasks);
		activityInventory = modifyTaskLabel(oldLabel, newLabel, this.state.activityInventory);
		
		this.setState({'tasks': tasks, 'todaysTasks': todaysTasks, 'activityInventory': activityInventory});
	},
	
	handleInputChange: function(e) {
		this.setState({'text': e.target.value});
	},

	handleSubmit: function(e) {
		var newTask, tasks, todaysTasks, activityInventory;
		
		e.preventDefault();
		
		if(this.state.text !== '') {
			newTask = makeTask(this.state.text);
			tasks = this.state.tasks.concat([newTask]);
			
			if(this.state.target === 'today') {
				todaysTasks = tasks;
				activityInventory = this.state.activityInventory.concat([newTask]);
				
			} else if(this.state.target === 'inventory') {
				todaysTasks = this.state.todaysTasks;
				activityInventory = tasks;
			}
			
			this.setState({'tasks': tasks, 'todaysTasks': todaysTasks, 'activityInventory': activityInventory, 'text' :''});
		}
	},
	
	internalSave: function() {
		saveTodaysTasks(this.state.todaysTasks);
		saveActivityInventory(this.state.activityInventory);
	},
	
	switchEditing: function() {
		var tasks, target, title, todaysTasks, activityInventory;
		
		if(this.state.target === 'today') {
			tasks = this.state.activityInventory;
			todaysTasks = this.state.tasks;
			activityInventory = this.state.activityInventory;
			target = 'inventory';
			title = 'Activity Inventory';
			
		} else if(this.state.target === 'inventory') {
			tasks = this.state.todaysTasks;
			todaysTasks = this.state.todaysTasks;
			activityInventory = this.state.tasks;
			target = 'today';
			title = "Today's tasks";
		}
		
		this.setState({'tasks': tasks, 'target': target, 'title': title, 'todaysTasks': todaysTasks, 'activityInventory': activityInventory});
	},
	
	killHandler: function() {
		this.internalSave();
		Timer.kill();
		this.props.doneHandler();
	},
	
	internalDoneHandler: function() {
		this.internalSave();
		this.props.doneHandler();
	},
	
	makeKillButton: function() {
		if(this.props.addKillButton) {
			return getKillButtonUI(this.killHandler);
		}
	},
	
	makeSwitchButton: function() {
		if(this.state.target === 'today') {
			return <button className="centered" onClick={this.switchEditing}>Switch to Activity Inventory</button>;
			
		} else if(this.state.target === 'inventory') {
			return <button className="centered" onClick={this.switchEditing}>Switch to Today's tasks</button>;
		}
	},
	
	makeDoneButton: function() {
		return <button className="doneButton" onClick={this.internalDoneHandler}>Done</button>;
	},

	render: function() {
		var contentStyle = {'height': '60%'};
		if(this.props.addKillButton) {
			contentStyle.height = '50%';
		}
		
		return (<div id="wishTaskManager">
					<h3>{this.state.title}</h3>
					<div className="content" style={contentStyle}>
						<WishList tasks={this.state.tasks} handleChange={this.handleChange} handleDelete={this.handleDelete} />
					</div>
					<form onSubmit={this.handleSubmit}>
					  <input onChange={this.handleInputChange} value={this.state.text} />
					  <button>'Enlist task'</button>
					</form>
					{this.makeSwitchButton()}
					{this.makeKillButton()}
					{this.makeDoneButton()}
				  </div>);
	}
});

// the selection lists allows to enlist tasks in the inventory as today's tasks
var SelectionLists = React.createClass({
	getInitialState: function() {
		return {'selected': []};
	},
	
	componentDidMount: function() {
		if(this.props.preselection) {
			this.setState({'selected': this.props.preselection});
		}
	},
	
	add: function(label) {
		if(indexOfTaskOfLabel(label, this.state.selected) === -1) {
			this.state.selected.push(taskOfLabel(label, this.props.tasks));
			this.setState({'selected': this.state.selected});
		}
	},
	
	remove: function(label) {
		this.state.selected.splice(indexOfTaskOfLabel(label, this.state.selected),1);
		this.setState({'selected': this.state.selected});
	},
	
	makeReferenceItem: function(task) {
		return <li onClick={this.add.bind(this, task.label)}>{task.label}</li>;
	},
	
	makeSelectedItem: function(task) {
		return <li onClick={this.remove.bind(this, task.label)}>{task.label}</li>;
	},
	
	internalDoneHandler: function() {
		this.props.saveFunction(this.state.selected);
		this.props.doneHandler();
	},
	
	makeDoneButton: function() {
		if(this.state.selected.length === 0) {
			return <button className="doneButton" disabled>Done</button>;
		} else {
			return <button className="doneButton" onClick={this.internalDoneHandler}>Done</button>;
		}
	},
	
	render: function() {
		return (<div id="selectionLists">
					<div className="pure-g content" style={{'height': '80%'}}>
						<div className="pure-u-1-2">
							<h3>Activity Inventory</h3>
							<ul>{this.props.tasks.map(this.makeReferenceItem)}</ul>
						</div>
						<div className="pure-u-1-2">
							<h3>Today's Tasks</h3>
							<ul>{this.state.selected.map(this.makeSelectedItem)}</ul>
						</div>
					</div>
					{this.makeDoneButton()}
				</div>);
	}
});

// the task list allows to annotate tasks
var AnnotationList = React.createClass({
	getInitialState: function() {
		return {'label': null, 'tasks': loadTodaysTasks()};
	},
	
	annotate: function(label) {
		this.setState({'label': label});
	},
	
	makeAnnotationItem: function(task) {
		var marks = '', i, result;
		
		for(i=0; i<task[this.props.annotationName]; i++) {
			if(i > 0) {
				marks += ' ';
			}
			marks += this.props.marker;
		}
		
		if(this.state.label && this.state.label === task.label) {
			result = <li>{task.label} <span className="annotation">{marks}</span><span className={this.props.annotationName}>{this.props.marker}</span></li>;
		} else {
			if(task.done) {
				result = <li className='done'>{task.label} <span className="annotation">{marks}</span></li>;
			} else {
				result = <li onClick={this.annotate.bind(this, task.label)}>{task.label} <span className="annotation">{marks}</span></li>;
			}
		}
		
		return result;
	},
	
	modifyTodaysTasks: function() {
		var position = indexOfTaskOfLabel(this.state.label, this.state.tasks);
		this.state.tasks[position][this.props.annotationName]++;
		return position;
	},
	
	killHandler: function() {
		this.modifyTodaysTasks();
		saveTodaysTasks(this.state.tasks);
		Timer.kill();
		this.props.doneHandler();
	},
	
	alternateDoneHandler: function() {
		var nextState = this.props.postAnnotate(this.state.label, this.state.tasks, this.modifyTodaysTasks());
		saveTodaysTasks(this.state.tasks);
		this.props.doneHandler(nextState);
	},
	
	internalDoneHandler: function() {
		this.modifyTodaysTasks();
		saveTodaysTasks(this.state.tasks);
		this.props.doneHandler();
	},
	
	makeKillButton: function() {
		if(this.props.addKillButton) {
			return getKillButtonUI(this.killHandler, !this.state.label);
		}
	},
	
	makeAlternativeDoneButton: function() {
		if(this.props.postAnnotateLabel) {
			if(!this.state.label) {
				return <button className="doneButton" disabled>{this.props.postAnnotateLabel}</button>;
			} else {
				return <button className="doneButton" onClick={this.alternateDoneHandler}>{this.props.postAnnotateLabel}</button>;
			}
		}
	},
	
	makeDoneButton: function() {
		if(!this.state.label) {
			return <button className="doneButton" disabled>Done</button>;
		} else {
			return <button className="doneButton" onClick={this.internalDoneHandler}>Done</button>;
		}
	},
	
	render: function() {
		var contentStyle = {'height': '70%'};
		if(this.props.addKillButton) {
			contentStyle.height = '60%';
		}
		
		return (<ul id="annotationList">
					<h3>{this.props.title}</h3>
					<div className="content" style={contentStyle}>
						{this.state.tasks.map(this.makeAnnotationItem)}
					</div>
					{this.makeKillButton()}
					{this.makeAlternativeDoneButton()}
					{this.makeDoneButton()}
				</ul>);
	}
});

// very straighforward display of statistics as a table (date, number of pomodoro done, number of interruption, number of tasks done, number of tasks planned)
var Statistics = React.createClass({
	renderLine: function(arr) {
		return arr.map(function(value) { return <span className="entry">{value}</span>; });
	},
	
	renderItem: function(item) {
		return <li>{this.renderLine([item.date, item.pomodoro, item.interrupt, item.done, item.planned])}</li>;
	},
	
	makeDoneButton: function() {
		return <button className="doneButton" onClick={this.props.doneHandler}>Done</button>;
	},
	
	render: function() {
		return (<div id="statistics">
					<div className="content" style={{height: '80%'}}>
						<ul>
							<li>{this.renderLine(['Date','Pomodoro done','Interruptions','Tasks done','Tasks planned'])}</li>
							{getStatistics().map(this.renderItem)}
						</ul>
					</div>
					{this.makeDoneButton()}
				</div>);
	}
});

/**
	TIMER UI and related utilities
**/
function getKillButtonId() { return 'killBtn'; }

function getKillButtonUI(clickHandler, disabled) {
	if(clickHandler) {
		if(disabled) {
			return <button id="killBtn" disabled>Kill the Pomodoro</button>;
		} else {
			return <button id="killBtn" onClick={clickHandler}>Kill the Pomodoro</button>;
		}
	} else {
		return '<button id="killBtn">Kill the Pomodoro</button>';
	}
}

// the debug flag only affects the durations. Instead of a 25 minutes cycle, a 5 minutes short pause and a 15 minutes long pause, we have respectively 10 seconds, 5 seconds and 6 seconds.
function makeTimer(stateSwitcher, debug) {
	var POMODORO_DURATION = 25*60, POMODORO_SHORT_PAUSE = 5*60, POMODORO_LONG_PAUSE = 15*60;
	var timer, pomodoroCount;
	var inPomodoro;// are we timing a pomodoro or a pause?
	var paused;// is the timer is paused? Not to be confused with being timing a pause!
	var pastSeconds;// how many seconds have been timed? Needed to resume the timer.
	var killed;// was the timer killed while timing?
	var timeoutId;// hold the id of the timing function, if any
	
	if(debug) {
		POMODORO_DURATION = 10;
		POMODORO_SHORT_PAUSE = 5;
		POMODORO_LONG_PAUSE = 6;
	} else {
		POMODORO_DURATION = 25*60;
		POMODORO_SHORT_PAUSE = 5*60;
		POMODORO_LONG_PAUSE = 15*60;
	}
	
	var displayOverlay = function(callback) {
		require('child_process').execFile('TransparentOverlay.exe',[],{}, callback);
	};
	
	var formatTime = function(totalSeconds) {
		var minutes, seconds;
		
		minutes = Math.floor(totalSeconds / 60);
		seconds = totalSeconds - minutes * 60;
		
		return minutes + ':' + (seconds < 10 ? '0' + seconds : seconds);
	};
	
	var makeTimeCounter = function(seconds, onInstall, endProcess) {
		return function() {
			var targetTime = (new Date()).getTime() + seconds * 1000;
			
			if(timeoutId) {
				global.clearInterval(timeoutId);
			}
			
			timeoutId = global.setInterval(function() {
				pastSeconds = Math.round((targetTime - (new Date()).getTime()) / 1000);
				if(pastSeconds < 0) {
					global.clearInterval(timeoutId);
					timeoutId = null;
					endProcess();
				} else {
					$('#timer').text(formatTime(pastSeconds));
				}
				
			}, 1000);
			
			onInstall();
		};
	};
	
	// we stop calling the timing function and just forget about it and we note the timer is paused.
	var pauseTimer = function() {
		global.clearInterval(timeoutId);
		timeoutId = null;
		paused = true;
	};
	
	// we stop calling the timing function and just forget about it. The timer may have been ended during a pause. We don't care if it were the case or not: paused is forced to false. We also nullify pastSeconds because there's no need for resuming.
	// we only thing we care about is whether we should time a pomodoro or a pause. If no argument is given, we toogle the inPomodoro flag
	var endTimer = function(_inPomodoro) {
		global.clearInterval(timeoutId);
		timeoutId = null;
		pastSeconds = null;
		paused = false;
		if(_inPomodoro === undefined) {
			inPomodoro = !inPomodoro;
		} else {
			inPomodoro = _inPomodoro;
		}
	};
	
	var installTimerUI = function(startDuration) {
		var duration, timeCounter;
		var installKillButtons = function() {
			$('#timerBtn').text('Interrupt')
				.unbind('click')
				.click(function() {
					pauseTimer();
					stateSwitcher('noteInterrupt');
				});
			
			$('#layout').append(getKillButtonUI());
			
			$('#'+getKillButtonId()).click(function() {
				endTimer(false);
				stateSwitcher('noteInterrupt');
			});
		};
		
		duration = startDuration ? startDuration : POMODORO_DURATION;
		timeCounter = makeTimeCounter(duration,
			function() {
				setWindowInTimingMode();
				installKillButtons();
			},
			function() {
				pomodoroCount++;
				endTimer();
				displayOverlay(function() { stateSwitcher('notePomodoroEnd'); });
			});
		
		if(startDuration) {
			$('#layout').html('<div class="drag"><p id="timer">'+formatTime(duration)+'</p></div><button id="timerBtn"></button>');
			timeCounter();
		} else {
			$('#layout').html('<div class="drag"><p id="timer">'+formatTime(duration)+'</p></div><button id="timerBtn">Start a Pomodoro</button>');
			$('#timerBtn').click(timeCounter);
		}
	};

	var installPauseUI = function(startDuration) {
		var pauseText, duration, timeCounter;
		var onPauseEnded = function() {
			endTimer(true);
			installTimerUI();
		};
		var installEndButton = function() {
			$('#timerBtn').text('End the pause')
				.unbind('click')
				.click(onPauseEnded);
		};
		
		if(pomodoroCount == 4) {
			pomodoroCount = 0;
			duration = POMODORO_LONG_PAUSE;
			pauseText = 'Long pause';
		} else {
			duration = POMODORO_SHORT_PAUSE;
			pauseText = 'Short pause';
		}
		
		timeCounter = makeTimeCounter(duration, installEndButton, onPauseEnded);
		
		if(startDuration) {
			$('#layout').html('<div class="drag"><p id="timer">'+formatTime(duration)+'</p></div><button id="timerBtn"></button>');
			timeCounter();
		} else {
			$('#layout').html('<div class="drag"><p id="timer">'+formatTime(duration)+'</p></div><button id="timerBtn">'+pauseText+'</button>');
			$('#timerBtn').click(timeCounter);
		}
	};
	
	var installUI = function(startDuration) {
		if(inPomodoro) {
			installTimerUI(startDuration);
		} else {
			installPauseUI(startDuration);
		}
	};
	
	var resume = function() {
		if(paused) {
			if(pastSeconds > 0) {
				paused = false;
				installUI(pastSeconds);
					
			// if pastSeconds is zero or below, the timing has naturally ended so we just proceed with endTimer() and then install the appropriate UI.
			} else {
				installUI();
			}
		
		} else {
			killed = false;
			installUI();
		}
	};
	
	var isPaused = function() {
		return paused;
	};
	
	var isKilled = function() {
		return killed;
	};
	
	var kill = function() {
		endTimer();
		killed = true;
	};
	
	var reset = function() {
		timer = {};
		// initially, no pomodoro have been done, the timer has not been killed and we are in the exact same state as if the timer has ended timing a pause
		pomodoroCount = 0;
		killed = false;
		endTimer(true);
		
		timer.pause = pauseTimer;
		timer.resume = resume;
		timer.isPaused = isPaused;
		timer.isKilled = isKilled;
		timer.kill = kill;
	};
	reset();
	return timer;
}

/**
	LAYOUT
**/
function setWindowInTaskMode() {
	var windowUI = require("nw.gui").Window.get();
	windowUI.moveTo(200, 150);
	windowUI.resizeTo(1520, 780);
}

function setWindowInTimerMode() {
	var windowUI = require("nw.gui").Window.get();
	windowUI.moveTo(1650, 850);
	windowUI.resizeTo(220, 120);
}

function setWindowInTimingMode() {
	var windowUI = require("nw.gui").Window.get();
	windowUI.moveTo(1650, 850);
	windowUI.resizeTo(220, 170);
}

function setWindowToState(state, stateSwitcher) {
	var windowUI = require("nw.gui").Window.get(), layout = document.getElementById('layout');
	
	if(state === 'initializeActivityInventory') {
		React.unmountComponentAtNode(layout);
		setWindowInTaskMode();
		React.renderComponent(<WishTaskManager title="Activity Inventory" saveFunction={saveActivityInventory} loadFunction={loadActivityInventory} doneHandler={stateSwitcher} />, layout);
		
	} else if(state === "selectToday'sTasks") {
		React.unmountComponentAtNode(layout);
		setWindowInTaskMode();
		React.renderComponent(<SelectionLists tasks={loadActivityInventory()} preselection={loadTodaysTasks()} saveFunction={saveTodaysTasks} doneHandler={stateSwitcher} />, layout);
		
	} else if(state === 'timer') {
		React.unmountComponentAtNode(layout);
		setWindowInTimerMode();
		Timer.resume();
	
	} else if(state === 'notePomodoroEnd') {
		React.unmountComponentAtNode(layout);
		React.renderComponent(<AnnotationList title="Completion of the Pomodoro" marker="X" annotationName="pomodoro"
												doneHandler={stateSwitcher}
												postAnnotateLabel="Done and task done"
												postAnnotate={function(label, tasks, position) {
													tasks[position].done = true;
												}} />,
							layout);
		setWindowInTaskMode();
		
	} else if(state === 'noteInterrupt') {
		React.unmountComponentAtNode(layout);
		React.renderComponent(<AnnotationList title="Interruption of the Pomodoro"
									marker="'" annotationName="interrupt"
									doneHandler={stateSwitcher}
									addKillButton={Timer.isPaused() && !Timer.isKilled()} />,
							layout);
		setWindowInTaskMode();
		
	} else if(state === 'addTasks') {
		React.unmountComponentAtNode(layout);
		React.renderComponent(<GlobalWishTaskManager doneHandler={stateSwitcher} />, layout);
		setWindowInTaskMode();
		
	} else if(state === 'statistics') {
		React.unmountComponentAtNode(layout);
		React.renderComponent(<Statistics doneHandler={stateSwitcher} />, layout);
		setWindowInTaskMode();
	}
}

/**
	OPTIONS
**/
function makeOptionsPanel(stateSwitcher) {
	$('#options ul ul li').each(function(index) {
		switch(index) {
			case 0:
				$(this).click(function() {
					Timer.pause();
					setWindowToState('addTasks', function() { stateSwitcher.reinstall(); });
				});
				break;
			
			case 1:
				$(this).click(function() {
					Timer.pause();
					setWindowToState("selectToday'sTasks", function() { stateSwitcher.reinstall(); });
				});
				break;
			
			case 2:
				$(this).click(function() {
					Timer.pause();
					setWindowToState('statistics', function() { stateSwitcher.reinstall(); });
				});
				break;
			
			default:
				console.error('Index '+index+' is not recognized in the making of options');
				break;
		}
	});
}

/**
	STATE FLOW
**/
function makeStateSwitchFunction(activityInventory, todaysTasks) {
	var state, stateSwitcher;
	
	/*
		Are we more advanced than the undefined state?
		The answer is to assign the value of the state that represent what is already done
		(and not what the state should be: that's the job of the stateSwitcher function itself to decide that)
	*/
	if(activityInventory.length > 0) {
		state = 'initializeActivityInventory';
		if(todaysTasks.length > 0) {
			state = "selectToday'sTasks";
		} else {
			initializeTodaysTasks();
		}
	}
	
	/*
		Switch from a given state to another one, maybe taking into account the requested state if it actually means something.
		The "timer" state is the core state and have 2 internal states: timing a Pomodoro work session and timing a pause.
		"Internal states" can sound like a complexification but it is simple actually: the role of the stateSwitcher is to deal	with all the various endeavours *around* the Pomodoro cycle, not with the Pomodoro cycle itself.
		
		The timer can manage this cycle all by itself by proposing 2 methods: resume and kill. Being the recipient of such methods,	it can count how many Pomodoros have really been done and therefore adapt the length of the pause.
		
		So, about those various endeavours *around* the Pomodoro cycle, 3 flows can be identified: 
		- the "init flow", leading to the "timer" core state
		- the "regular flow", the Pomodoro cycle itself in fact with the quick step of recording which task the current Pomodoro was about
		- the "interrupted flow", interruptions happen and there are 3 possible ways to deal with them, all leading back to the "timer" state
		
		This is actually in this last flow that the requested state argument will be used because of the number of options.
		Here's the detailed description of each flow:
		
		- the "init flow": undefined -> initializeActivityInventory -> selectToday'sTasks -> timer
		  Obviously, before being able to reach the "timer" core state, some activities have to be listed and then selected for the current day.
		  While "initializeActivityInventory" will happen from times to times, at least one time at the very start of setting up the Pomodoro cycle, "selectToday'sTasks" will happen daily, upon starting the Pomodoro cycle. 
		  
		- the "regular flow": timer (pomodoro) -> notePomodoroEnd -> timer (pause) -> timer (pomodoro) -> ...
		  The timer has naturally reached its end, meaning the current pomodoro has ended. We note it on the task it was about then start timing the pause.
		  And then the Pomodoro cycle resumes.
		  We can see here why the timer manage the Pomodoro cycle by itself: on the transition 'timer (pause) -> timer (pomodoro)', no special UI installations are needed. We only have to reset the timer at the appropriate time count value.
		  
		- the "interrupted flow": timer -> noteInterrupt (-> addTasks) -> timer (variable)
		  The timer was interrupted and the interruption has to be noted but then 3 cases are possible:
		  i) Simple interruption: the timer can be directly resumed in the next few seconds.
		  ii) Complex interruption: the Pomodoro was directly killed or will be killed during "noteInterrupt".
		  iii) Classic interruption: some new tasks are to be enlisted, either in the Activity Inventory or the Today's Wish List.
		  
		  Again, we see here some more reasons as to why the timer manages the Pomodoro cycle by itself. Indeed, we may resume or restart the timer anytime: directly, after having entered "noteInterrupt" or even after having recorded some new tasks!
	*/
	stateSwitcher = function(requestedState) {
		if(!state) {
			state = 'initializeActivityInventory';
			
		} else if(state === 'initializeActivityInventory') {
			state = "selectToday'sTasks";
			
		} else if(state === "selectToday'sTasks") {
			state = 'timer';
			
		} else if(state === 'timer') {
			state = requestedState;
		
		} else if(state === 'notePomodoroEnd' || state === 'addTasks') {
			state = 'timer';
			
		} else if(state === 'noteInterrupt') {
			if(requestedState) {
				state = requestedState;
			} else {
				state = 'timer';
			}
		}
		
		setWindowToState(state, stateSwitcher);
		
		return stateSwitcher;
	};
	
	stateSwitcher.reinstall = function() {
		setWindowToState(state, stateSwitcher);
		return stateSwitcher;
	};
	
	return stateSwitcher;
}

/*
	Installation
	Setting debug to true make the developer tools available and shorten to seconds the various durations of the timer (10 seconds cycle, 5 seconds short pause and 6 seconds long pause).
*/
window.onload = function() {
	var activityInventory, todaysTasks, stateSwitcher, debug = false;
	
	ensureDirectoryExists(storeDirectory);
	
	activityInventory = loadActivityInventory();
	todaysTasks = loadTodaysTasks();
	
	stateSwitcher = makeStateSwitchFunction(activityInventory, todaysTasks);
	Timer = makeTimer(stateSwitcher, debug);
	makeOptionsPanel(stateSwitcher);
	
	stateSwitcher();
	
	var windowUI = require("nw.gui").Window.get();
	windowUI.show();
	if(debug) {
		windowUI.showDevTools();
	}
}