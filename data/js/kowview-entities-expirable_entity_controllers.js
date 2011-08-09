/**
 * Script for the expirable entity controller module types
 */





kowview.entities.expirable_entity_controllers = new Object();


dojo.addOnLoad(function(){console.log("oie!")});




/*************************************/
/* Driver Functions called from Ada */
/***********************************/


/**
 * Update the module listing
 */
kowview.entities.expirable_entity_controllers.updateList = function( select ){
	console.dir(select.value);
}


/**
 * Initialize a item, building the menu
 */
kowview.entities.expirable_entity_controllers.initializeItem = function( moduleId, itemId, isValid ) {
	console.log( "initializing item" + itemId );
}









/*************/
/* Variables */
/*************/
