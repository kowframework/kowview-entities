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
console.dir(select);
	select.domNode.parentNode.submit();
}


/**
 * Mark a item for initialization
 */
kowview.entities.expirable_entity_controllers.initializeItem = function( itemId, isValid ) {
	console.log( "initializing item" + itemId );
	console.log(isValid);
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
	console.log("hey");
	console.dir( kowview.entities.expirable_entity_controllers.extensions );
}


/*****************************/
/* Interface Initialization */
/***************************/

kowview.entities.expirable_entity_controllers.doInitializeItem = function( item, key ) {
	console.dir( item );
}
