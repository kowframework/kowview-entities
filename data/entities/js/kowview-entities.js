/*
 * Entity form handling
 * TODO :: http://dojotoolkit.org/reference-guide/dojox/editor/plugins/NormalizeStyle.html
 */


dojo.require( "kowview.entities.DetectPaste" );
dojo.require( "kowview.entities.ImageSelect" );

kowview.entities = new Object();


kowview.entities.connectEditor = function (editorId, inputId) {
	input = dojo.byId( inputId );
	editor= dijit.byId( editorId );


	editor.inputField = input;

	dojo.connect(
			editor,
			"onKeyDown",
			function(e) {
					input.value = editor.getValue();
				}
		);
	dojo.connect(
			editor,
			"onChange",
			function(newContent) {
					input.value = editor.getValue();
				}
		);
	dojo.connect(
			editor,
			"onClick",
			function(e) {
					input.value = editor.getValue();
				}
		);
	dojo.connect(
			editor,
			"execCommand",
			function(e) {
					input.value = editor.getValue();
				}
		);
	
	input.value = editor.getValue();

	/* About the events being used:
	 * 	even tough it's possible to use only the onChange event, I am using several others as 
	 * 	auto-save to be enabled needs this...
	 */
}

kowview.entities.submitForm = function ( moduleId, reloadPage ) {
	var parameters = new Object();


	kowview.entities.onSetParameters( moduleId, parameters );

	if( kowview.isSet( parameters.load ) )
		parameters.userLoad = parameters.load;

	parameters.load = function (data) {
		if( kowview.isSet( reloadPage ) && reloadPage == true )
			document.location.reload();

		if( kowview.isSet( parameters.userLoad ) )
			parameters.userLoad( data );
	}

	kowview.modules.iframeSend(
			moduleId,
			parameters
		);
}


kowview.entities.onSetParameters = function( moduleId, parameters ) {
	parameters.form = dojo.byId( "entity_form_" + moduleId );
	parameters.method = "POST";
	parameters.load = kowview.entities.onFormLoad;
	parameters.error = kowview.entities.onFormError;
}


/**
 * the response is in this function SO event dojo subsystem can be used by
 * modules implementors.
 */
kowview.entities.onFormLoad = function( data ) {
	if( data.response && data.response.is_new == true ) {
		href = window.location.href;
		if( href.indexOf( "?" ) > -1 )
			href += "&";
		else
			href += "?"
		href += "entity_id=" + data.response.entity_id;
		window.location.href = href;
	}
}


kowview.entities.onFormError = function( data ) {
	console.dir( data );
	// since validation error is handled by kowview core javascript framework
	// this method is here only for event listening
}



/**
 * TODO :: kowview.entities.SafePaste
 * TODO :: the SafePaste should be used by teachers and is simply a textarea where text can be pasted and then imported with no style or format.
 *
 *
 * This is to allow teachers to work with copy&paste and still leave the content consistent
 */
