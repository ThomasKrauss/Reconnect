/** @jsx React.DOM */

GlobalEvents = makeEventManager();

$(document).ready(function() {
	var DependencyDashboard, SystemMenu;
	
	/** INSTALL UI **/
	SystemMenu = React.createClass(makeReactiveMenu('systems-and-modules-list', 'dashboard', 'index.html', dashboardNavigate));
	React.renderComponent(
	  <SystemMenu />,
	  document.getElementById('layout')
	);
	
	$('#layout').append('<div id="graph" style="width: 100%; overflow: auto;"></div>');
	DependencyDashboard = React.createClass(makeDependencyDashboard());
	React.renderComponent(<DependencyDashboard />, document.getElementById('graph'));
	
	
	$('#layout').append('<div id="stats1"></div>');
	/*React.renderComponent(
	  <NestedBarChart dataKeys={['line-count','form-count','line-form-ratio']} endPointName='line-and-form-counts' width="400" barHeight="20" tickCount="4" xLabelHeight="12" />,
	  document.getElementById('stats1')
	);*/
	
	$('#stats1').html(makeDependencyGraphLegend());

	// KEYBINDINGS
	var keyMap = {};
	keyMap.basic = {
		'Ctrl-1': function() {
			GlobalEvents.fireEvent('focusCommandLine');
		}
	};
	globalKeyBinder = makeGlobalKeyBinder({}, keyMap, 'basic');
});