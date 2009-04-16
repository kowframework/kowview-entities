


---------
-- AWS --
---------
with AWS.Status;
with Templates_Parser;

---------------
-- Ada Works --
---------------
with Aw_View;

generic
	type Entity_Type is new Aw_Ent.Entity_Type with private;
	Component_Name	: constant String;
package Aw_View.Forms is



	function Read_Form_Data( Request : in AWS.Status.Data ) return Entity_Type;
	-- Read the form data, returning one entity
	--


	------------------------
	-- The FORM Component --
	------------------------

	type Component_Type is new Aw_View.Component_Interface with private;




	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		);
	-- initialize this component
	


	------------------------------------
	-- Modules for the FORM component --
	------------------------------------

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Aw_View.Module_Instance_Interface'Class;
	-- create a module instance by given name
	-- Available module:
	-- 	view
	-- 	edit_form
	-- 	insert_form
	


	type Entity_Form_Module is new Aw_View.Module_Instance_Interface with private;
	-- a type for rendering FORMS for entities.

	overriding
	procedure Process_Request(
			Module		: in out Entity_Form_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- This module draws agg
	--


private
	type Entity_Form_Module is new Aw_View.Module_Instance_Interface with record
		--Entity	: Entity_Type;
		Entity_Tag	: Ada.Tags.Tag;

	end record;




end Aw_View.Forms;
