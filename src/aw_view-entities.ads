

--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

---------------
-- Ada Works --
---------------
with Aw_Config;
with Aw_Ent;
with Aw_View.Components;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Status;
with Templates_Parser;

package Aw_View.Entities is

	--------------------------
	-- The Entity Component --
	--------------------------

	type Component_Type is new Aw_View.Components.Component_Interface with null record;




	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		);
	-- initialize this component
	



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Aw_View.Components.Module_Instance_Interface'Class;
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
		) return Aw_View.Components.Service_Instance_Interface'Class;


	--------------------------------------
	-- Modules for the Entity component --
	--------------------------------------

	--
	-- View Entity
	--

	type View_Entity_Module is new Aw_View.Components.Module_Instance_Interface with private;
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





	type Edit_Entity_Module is new View_Entity_Module with private;
	-- a type for rendering the data from entities into a form that can save the entity back
	-- for now it's the same as the View_Entity_Module but with a different default template.
	-- it should change anytime soon though


	overriding
	procedure Process_Request(
			Module		: in out Edit_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- the same as the View_Entity's Process_Request except for the FORM elements.
	-- this is so there can be a different form element type for each data type in the entity
	-- TODO :: make it possible for the user to extend the supported form element types



	---------------------------------------
	-- Services for the Entity component --
	---------------------------------------

	type Store_Entity_Service is new Aw_View.Components.Service_Instance_Interface with private;


	overriding
	procedure Process_Request(
			Service		: in out Store_Entity_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- read the entity from the form and process it.
	-- save it back to the database backend afterwards




private


	--------------------------------------
	-- Modules for the Entity component --
	--------------------------------------

	type View_Entity_Module is new Aw_View.Components.Module_Instance_Interface with record
		Id			: Aw_Ent.ID_Type;
		Entity_Tag		: Unbounded_String;
		Template_Name		: Unbounded_String;
	end record;


	type Edit_Entity_Module is new View_Entity_Module with null record;


	---------------------------------------
	-- Services for the Entity component --
	---------------------------------------

	type Store_Entity_Service is new Aw_View.Components.Service_Instance_Interface with record
		Variable_Prefix : String( 1 .. 6 ) := "entity";
		-- the prefix for every variable to be processed by this service
	end record;


end Aw_View.Entities;
