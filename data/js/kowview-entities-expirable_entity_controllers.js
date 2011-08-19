/**
 * Script for the expirable entity controller module types
 */





kowview.entities.expirable_entity_controllers = new Object();



/*************/
/* Variables */
/*************/


kowview.entities.expirable_entity_controllers.extensions = new Array();
kowview.entities.expirable_entity_controllers.items = new Array();
kowview.entities.expirable_entity_controllers.moduleId = null;


/*************************************/
/* Driver Functions called from Ada */
/***********************************/


/**
 * Update the module listing
 */
kowview.entities.expirable_entity_controllers.updateList = function( select ){
	select.domNode.parentNode.submit();
}


/**
 * Mark a item for initialization
 */
kowview.entities.expirable_entity_controllers.initializeItem = function( itemId, entityId, isValid ) {
	kowview.entities.expirable_entity_controllers.items.push( {
					itemId		: itemId,
					entityId	: entityId,
					isValid		: isValid
				} );
}

/**
 * Register the available extensions
 */
kowview.entities.expirable_entity_controllers.initExtension = function( entityTag, label ) {
	kowview.entities.expirable_entity_controllers.extensions.push({
					entityTag	: entityTag,
					label		: label
				});
}


/**
 * Call the initialization routines
 */
kowview.entities.expirable_entity_controllers.init = function( moduleId ) {
	kowview.entities.expirable_entity_controllers.moduleId = moduleId;
	dojo.forEach(kowview.entities.expirable_entity_controllers.items, kowview.entities.expirable_entity_controllers.doInitializeItem);
}


/*****************************/
/* Interface Initialization */
/***************************/

kowview.entities.expirable_entity_controllers.doInitializeItem = function( item, key ) {

	console.log("initializing the following item");
	console.dir(item);

	var menu = new dijit.Menu(
			{
					targetNodeIds:[item.itemId]
				}
		);
	

	var validateSubMenu = new dijit.Menu();
	validateSubMenu.addChild(
			new dijit.MenuItem({
					label	: "Rápida",
					disabled: item.isValid,
					onClick	: function() { kowview.entities.expirable_entity_controllers.fastValidate( item.entityId ); }
				})
		);
	validateSubMenu.addChild(new dijit.MenuSeparator());

	dojo.forEach(kowview.entities.expirable_entity_controllers.extensions, function(ext) {
		validateSubMenu.addChild(
				new dijit.MenuItem( {
						label	: ext.label,
						onClick	: function() { kowview.entities.expirable_entity_controllers.formValidate( item.entityId, ext.entityTag ) }
					} )
			);
	});


	menu.addChild(
			new dijit.PopupMenuItem({
					label	: "Ativar",
					disabled: item.isValid,
					popup	: validateSubMenu
				})
		);

	menu.addChild(
			new dijit.MenuItem({
					label	: "Desativar",
					disabled: !item.isValid,
					onClick	: function() { kowview.entities.expirable_entity_controllers.expire( item.entityId ) }
				})
		);
	menu.startup();

}


/**************************/
/* Activation Interfaces */
/************************/


function reloadMe(data){
	document.location.reload();
}

kowview.entities.expirable_entity_controllers.fastValidate = function( entityId ) {
	kowview.entities.expirable_entity_controllers.callJson( "validate_entity", entityId, null, reloadMe, null );
}


kowview.entities.expirable_entity_controllers.formValidate = function( entityId, extensionTag ){

	var showDialog = function( data ) {
		console.dir(data.response);
		theForm = new dijit.Dialog({
				title		: "Validar entidade",
				content	: data.response.formHTML
			});
		theForm.show();

	}
	
	kowview.entities.expirable_entity_controllers.callJson( "render_form", entityId, null, showDialog, null, { validation_entity_tag : extensionTag } );

}

kowview.entities.expirable_entity_controllers.expire = function( entityId ){
	kowview.entities.expirable_entity_controllers.callJson( "expire_entity", entityId, null, reloadMe, null );
}



/*******************/
/* Json Interface */
/*****************/

/**
 * Valid actions:
 * 	* expire_entity
 * 	* validate_entity
 * 	* render_form
 * 	* store_validation_period
 *
 */


kowview.entities.expirable_entity_controllers.callJson = function( action, entityId, form, load, error, content ) {
	if ( action != "expire_entity" && action != "validate_entity" && action != "render_form" && action != "store_validation_period" ) {
		kowview.showErrorMessage( "Ação Inválida", "Aparentemente o desenvolvedor comeu bola. Entre em contato com o fornecedor da aplicação" );
		throw "Invalid Action";
	}

	var params = new Object();;


	if( form != null )
		params.form = form;


	if( kowview.isSet( content ) ) {
		params.content = content;
	} else {
		params.content = new Object();
	}

	params.content.action	= action;
	params.content.entity_id= entityId;

	params.load = load;

	if( error != null )
		params.error = error;
	else
		params.error =function( data ) { console.dir(data);};
	

	console.log( kowview.entities.expirable_entity_controllers.moduleId );
	console.dir( params );
	kowview.modules.postJson(
			kowview.entities.expirable_entity_controllers.moduleId,
			params
		);

}


