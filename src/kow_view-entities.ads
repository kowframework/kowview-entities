

--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

---------------
-- Ada Works --
---------------
with KOW_Config;
with KOW_Ent;
with KOW_Lib.File_System;
with KOW_View.Components;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Status;
with Templates_Parser;

package KOW_View.Entities is


	--------------------------
	-- The Entity Component --
	--------------------------

	type Component_Type is new KOW_View.Components.Component_Interface with null record;




	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		);
	-- initialize this component
	



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return KOW_View.Components.Module_Instance_Interface'Class;
	-- create a module instance by given name
	-- Available module:
	-- 	view_entity
	-- 	TODO :: edit_entity
	-- 	TODO :: create_entity
	


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return KOW_View.Components.Service_Instance_Interface'Class;


	--------------------------------------
	-- Modules for the Entity component --
	--------------------------------------

	--
	-- View Entity
	--

	type View_Entity_Module is new KOW_View.Components.Module_Instance_Interface with private;
	-- a type for rendering the data from entities in a determined form area



	procedure Initialize_Request(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		) is null;
	-- TODO :: implement Initialize_Request so it'll check user's permission to view the entity

	procedure Process_Header(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is null;
	-- TODO :: implement as optional in the template

	procedure Process_Request(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- draws the main template for this entity

	procedure Process_Footer(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is null;
	-- TODO :: implement as optional in the template

	procedure Finalize_Request(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
		) is null;


	--
	-- Edit Entity
	--





	type Edit_Entity_Module is new KOW_View.Components.Module_Instance_Interface with private;
	-- a type for rendering the data from entities into a form that can save the entity back


	overriding
	procedure Process_Request(
			Module		: in out Edit_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);


	--
	-- Edit User Entity
	--
	
	type Edit_User_Entity_Module is new Edit_Entity_Module with private;
	-- use Edit_Entity_Module to create a form to edit user properties to user
	-- types derived from kow_sec.entities.user_type

	overriding
	procedure Process_Request(
			Module		: in out Edit_User_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- draw up the form for the loged user
	-- if no user is loged ir it's loged as other user type, return an empty string
	--
	-- also, make sure to reload the entity from the database at loading time
	



	--
	-- Create Entity
	--

	type Create_Entity_Module is new KOW_View.Components.Module_Instance_Interface with private;
	-- a type for rendering the form for creating new entities


	overriding
	procedure Process_Request(
			Module		: in out Create_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);



	-- 
	-- Entity Browser Module
	--
	
	type Entity_Browser_Module is new KOW_View.Components.Module_Instance_Interface with private;
	-- The entity browser module not only browse throughout entities of various kinds but
	-- also allows the user to edit and create new ones.
	--
	-- It can be controlled by HTTP parameters.
	--
	--
	-- Available parameters:
	-- 	entity_tag		=> the tag for the entity to browse.
	-- 	allow_creation		=> can we create new entities ( true/false )
	-- 	allow_edit		=> can the entity be edited?
	-- 	view_entity_template	=> the tempate when viewing an entity
	-- 	edit_entity_template	=> the template when editting an entity
	--	listing_template	=> the template for listing the entity


	overriding
	procedure Process_Request(
			Module		: in out Entity_Browser_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- the entire cycle for this module is inside this single procedure.

	---------------------------------------
	-- Services for the Entity component --
	---------------------------------------

	type Store_Entity_Service is new KOW_View.Components.Service_Instance_Interface with private;


	overriding
	procedure Process_Request(
			Service		: in out Store_Entity_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- read the entity from the form and process it.
	-- save it back to the database backend afterwards




private



	Default_View_Entity_Template_Name	: constant Unbounded_String := To_Unbounded_String( "default_templates" & KOW_Lib.File_System.Separator & "view_entity" );
	Default_Edit_Entity_Template_Name	: constant Unbounded_String := To_Unbounded_String( "default_templates" & KOW_Lib.File_System.Separator & "edit_entity" );
	Default_Create_Entity_Template_Name	: constant Unbounded_String := To_Unbounded_String( "default_templates" & KOW_Lib.File_System.Separator & "create_entity" );
	Default_List_Entities_Template_Name	: constant Unbounded_String := To_Unbounded_String( "default_templates" & KOW_Lib.File_System.Separator & "list_entities" );

	--------------------------------------
	-- Modules for the Entity component --
	--------------------------------------

	type View_Entity_Module is new KOW_View.Components.Module_Instance_Interface with record
		Id			: KOW_Ent.ID_Type;
		Entity_Tag		: Unbounded_String;
		Template_Name		: Unbounded_String;
	end record;


	type Edit_Entity_Module is new KOW_View.Components.Module_Instance_Interface with record
		Id			: KOW_Ent.ID_Type;
		Entity_Tag		: Unbounded_String;
		Template_Name		: Unbounded_String;
	end record;

	type Edit_User_Entity_Module is new Edit_Entity_Module with null record;
	-- use Edit_Entity_Module to create a form to edit user properties to user
	-- types derived from kow_sec.entities.user_type





	type Create_Entity_Module is new KOW_View.Components.Module_Instance_Interface with record
		Entity_Tag		: Unbounded_String;
		Template_Name		: Unbounded_String;
	end record;



	type Entity_Browser_Module is new KOW_View.Components.Module_Instance_Interface with record
		Entity_Tag	: Unbounded_String;
		-- the tag for the entity to browse

		Allow_Creation	: Boolean;
		-- can the user create new entities?
		Allow_Editting	: Boolean;
		-- can the user edit the available entities?


		View_Entity_Template_Name	: Unbounded_String;
		-- the template used when viewing entities

		Edit_Entity_Template_Name	: Unbounded_String;
		-- the template used when editting an entity

		Create_Entity_Template_Name	: Unbounded_String;
		-- the template used when creating a new entity

		List_Entities_Template_Name	: Unbounded_String;
		-- the template used when listing the entities
	end record;


	---------------------------------------
	-- Services for the Entity component --
	---------------------------------------

	type Store_Entity_Service is new KOW_View.Components.Service_Instance_Interface with record
		Variable_Prefix : String( 1 .. 6 ) := "entity";
		-- the prefix for every variable to be processed by this service
	end record;




end KOW_View.Entities;
