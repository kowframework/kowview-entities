--------------
-- Ada 2005 --
--------------
with Ada.Tags;

---------------
-- Ada Works --
---------------
with Aw_View;


---------
-- AWS --
---------
with AWS.Status;
with Templates_Parser;


package body Aw_View.Entities is



	function Read_Form_Data( Request : in AWS.Status.Data ) return Entity_Type;
	-- Read the form data, returning one entity
	--


	--------------------------
	-- The Entity Component --
	--------------------------

	type Component_Type is new Aw_View.Components.Component_Interface with private;




	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		) is
	begin
		null;
	end Initialize;


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Aw_View.Components.Module_Instance_Interface'Class is
	-- create a module instance by given name
	-- Available module:
	-- 	view_entity
	-- 	TODO :: edit_entity
	-- 	TODO :: create_entity
		View_Entity : View_Entity_Module;
	begin
		if Component_Name = "view_entity" then
			return View_Module;
		else
			raise MODULE_ERROR with "Unknown module :: " & Module_Name;
		end if;
	end Create_Instance;



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Aw_View.Components.Service_Instance_Interface'Class is
	begin
		raise Aw_View.Components.SERVICE_ERROR with "there is no service implemented in the entities component yet";
	end Create_Instance;


	--------------------------------------
	-- Modules for the Entity component --
	--------------------------------------

	function Get_Label(
			Module		: in View_Entity_Module;
			Property	: in Aw_Ent.Entity_Property_Type'Class
		) return String is
	begin
		-- TODO: implement a proper way for handling labels:
		return To_String( Property.Column_Name );
	end Get_Label;

	function Get_Input(
			Module		: in View_Entity_Module;
			Property	: in Aw_Ent.Entity_Property_Type'Class;
			Entity		: in Aw_Ent.Entity_Type'Class
		) return String is
	begin
		return Aw_Ent.Get_Property( Property, Entity );
	end Get_Input;



	overriding
	procedure Process_Request(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is


		Properties	: Aw_Ent.Property_Lists.List;
		Entity		: Aw_Ent.Entity_Type'Class := Aw_Ent.New_Entity( Module.Tag );


		procedure Iterator( C : Aw_Ent.Property_Lists.Cursor ) is
			Label_Tag	: Templates_Parser.Tag;
			Input_Tag	: Templates_Parser.Tag;
			P		: Aw_Ent.Entity_Property_Ptr;
		begin
			P := Aw_Ent.Property_Lists.Element( C );
			Label_Tag := Label_Tag & Get_Label( Module, P.all );
			Input_Tag := Input_Tag & Get_Input( Module, P.all, Entity );
		end Iterator;


		My_Parameters : Templates_Parser.Translate_Set;

	begin

		if not Module.Tag in Aw_Ent.Entity_Type'Class then
			raise MODULE_ERROR with "Not an entity :: " & Ada.Tags.Expanded_Name( Module.Tag );
		end if;

		Properties := Aw_Ent.Entity_Registry.Get_Properties( Module.Tag );

		Aw_Ent.Property_Lists.Iterate( Properties, Iterator'Access );

		Templates_Parser.Assoc( My_Parameters, "entity_labels", Label_Tag );
		Templates_Parser.Assoc( My_Parameters, "entity_values", Input_Tag );

		Response := Response &
			To_Unbounded_String(
				Templates_Parser.Parse(
						To_String( Module.Template_File_name ),
						My_Parameters
					)
				);
	end Process_Request;



	---------------------------------------
	-- Services for the Entity component --
	---------------------------------------


end Aw_View.Entities;
