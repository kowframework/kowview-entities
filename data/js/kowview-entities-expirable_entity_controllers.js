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
kowview.entities.expirable_entity_controllers.initializeItem = function( itemId, isValid ) {
	kowview.entities.expirable_entity_controllers.items.push( {
					itemId		: itemId,
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
					onClick	: function() { kowview.entities.expirable_entity_controllers.fastValidate( item.itemId ); }
				})
		);
	validateSubMenu.addChild(new dijit.MenuSeparator());

	dojo.forEach(kowview.entities.expirable_entity_controllers.extensions, function(ext) {
		validateSubMenu.addChild(
				new dijit.MenuItem( {
						label	: ext.label,
						onClick	: function() { kowview.entities.expirable_entity_controllers.formValidate( item.itemId, ext.entityTag ) }
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
					onClick	: function() { kowview.entities.expirable_entity_controllers.expire( item.itemId ) }
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

kowview.entities.expirable_entity_controllers.fastValidate = function( itemId ) {
	kowview.entities.expirable_entity_controllers.callJson( "validate_entity", itemId, null, reloadMe, null );
}


kowview.entities.expirable_entity_controllers.formValidate = function( itemId, extensionTag ){

	var showDialog = function( response ) {
		theForm = new dijit.Dialog({
				title		: "Validar entidade",
				innerHTML	: response.formHTML
			});

	}
	
	kowview.entities.expirable_entity_controllers.callJson( "render_form", itemId, null, showDialog, null );

}

kowview.entities.expirable_entity_controllers.expire = function( itemId ){
	kowview.entities.expirable_entity_controllers.callJson( "expire_entity", itemId, null, reloadMe, null );
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


kowview.entities.expirable_entity_controllers.callJson = function( action, itemId, form, load, error ) {
	if ( action != "expire_entity" && action != "validate_entity" && action != "render_form" && action != "store_validation_period" ) {
		kowview.showErrorMessage( "Ação Inválida", "Aparentemente o desenvolvedor comeu bola. Entre em contato com o fornecedor da aplicação" );
		throw "Invalid Action";
	}

	var params = new Object();

	if( form != null )
		params.form = form;
	
	params.content = {
			action		: action,
			entity_id	: kowview.entities.expirable_entity_controllers.getEntityId( itemId )
		};

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



/**
 * Get the entity to be validadet id from the list item id
 */
kowview.entities.expirable_entity_controllers.getEntityId = function( itemId ) {
	// TODO :: implement-me
	return 1;
}
