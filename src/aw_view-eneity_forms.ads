



with Aw_View;

generic
	type Entity_Type is new Aw_Ent.Entity_Type with private;
	Component_Name	: constant String;
package Aw_View.Forms is

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
	




end Aw_View.Forms;
