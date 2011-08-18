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
					onClick	: function() { alert('lalala');}
				})
		);
	menu.startup();

}


/**************************/
/* Activation Interfaces */
/************************/


kowview.entities.expirable_entity_controllers.fastValidate = function( itemId ) {
	console.log("fast validating item " + itemId );
}


kowview.entities.expirable_entity_controllers.formValidate = function( itemId, extensionTag ){
	console.log( "form validating item " + itemId + " using extension " + extensionTag );
}
