/** @jsx React.DOM */

GlobalEvents = makeEventManager();

$(document).ready(function() {
	var systemName;
	var SystemMenu, DependencyDashboard;
	
	querySearch = /[\?&]system-name=([^&]*)/.exec(window.location.search);
	if(querySearch) {
		systemName = querySearch[1];
	}
	
	/** INSTALL UI **/
	SystemMenu = React.createClass(makeReactiveMenu('systems-and-modules-list', 'dashboard', 'index.html', dashboardNavigate));
	React.renderComponent(
	  <SystemMenu selection={[systemName]} />,
	  document.getElementById('layout')
	);
	
	$('#layout').append('<div id="graph" style="width: 100%; overflow: auto;"></div>');
	DependencyDashboard = React.createClass(makeDependencyDashboard(systemName));
	React.renderComponent(<DependencyDashboard />, document.getElementById('graph'));
});