--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Text_IO;	use Ada.Text_IO;

---------------
-- Ada Works --
---------------
with Aw_Lib.File_System;
with Aw_View;
with Aw_View.Components_Registry;
with Aw_View.Entities_Helper;


---------
-- AWS --
---------
with AWS.Status;
with Templates_Parser;


package body Aw_View.Entities is



	--------------------------
	-- The Entity Component --
	--------------------------

	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		) is
	begin
		null;
	end Initialize;



	function Get_Default_View_Template_Name( Component : in Component_Type ) return Unbounded_String is
		use Aw_Lib.File_System;
	begin
		return To_Unbounded_String(
				Aw_View.Components_Registry.Locate_Resource(
						Component_Name  => "entities",
						Resource        => "default_templates" & Separator & "view_entity",
						Extension       => "html",
						Kind            => Ada.Directories.Ordinary_File
					)
				);
	end Get_Default_View_Template_Name;

	function Get_Default_Edit_Template_Name( Component : in Component_Type ) return Unbounded_String is
		use Aw_Lib.File_System;
	begin
		return To_Unbounded_String(
				Aw_View.Components_Registry.Locate_Resource(
						Component_Name  => "entities",
						Resource        => "default_templates" & Separator & "edit_entity",
						Extension       => "html",
						Kind            => Ada.Directories.Ordinary_File
					)
				);
	end Get_Default_Edit_Template_Name;




	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Aw_View.Components.Module_Instance_Interface'Class is
	-- create a module instance by given name
	-- Available module:
	-- 	view_entity
		View_Entity : View_Entity_Module;
		Edit_Entity : Edit_Entity_Module;
	begin
		if Module_Name = "view_entity" then
			View_Entity.Entity_Tag	:= Aw_Config.Element( Config, "entity_tag" );
			View_Entity.Id		:= Aw_Ent.To_ID( Aw_Config.Element( Config, "id" ) );
			View_Entity.Template_Name	:= Aw_Config.Value( Config, "template_name", Null_Unbounded_String );

			if View_Entity.Template_Name = Null_Unbounded_String then
				View_Entity.Template_Name := Get_Default_View_Template_Name( Component );
			end if;

			return View_Entity;
		elsif Module_Name = "edit_entity" then
			Edit_Entity.Entity_Tag	:= Aw_Config.Element( Config, "entity_tag" );
			Edit_Entity.Id		:= Aw_Ent.To_ID( Aw_Config.Element( Config, "id" ) );
			Edit_Entity.Template_Name	:= Aw_Config.Value( Config, "template_name", Null_Unbounded_String );

			if Edit_Entity.Template_Name = Null_Unbounded_String then
				Edit_Entity.Template_Name := Get_Default_Edit_Template_Name( Component );
			end if;

			return Edit_Entity;

		else
			raise Aw_View.Components.MODULE_ERROR with "Unknown module :: " & Module_Name;
		end if;
	end Create_Instance;



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Aw_View.Components.Service_Instance_Interface'Class is
		Store : Store_Entity_Service;
	begin
		return Store;
	end Create_Instance;


	--------------------------------------
	-- Modules for the Entity component --
	--------------------------------------




	overriding
	procedure Process_Request(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		use Templates_Parser;


		Properties	: Aw_Ent.Property_Lists.List;
		Entity		: Aw_Ent.Entity_Type'Class := Aw_Ent.New_Entity( Module.Entity_Tag );

		My_Parameters : Templates_Parser.Translate_Set;

	begin
		Aw_Ent.Load( Entity, Module.Id );
		Properties := Aw_Ent.Entity_Registry.Get_Properties( Module.Entity_Tag );

		Aw_View.Entities_Helper.Insert(
					My_Parameters,
					"entity",
					Entity
				);

		Response := Response &
			To_Unbounded_String(
				Templates_Parser.Parse(
						To_String( Module.Template_Name ),
						My_Parameters
					)
				);
	end Process_Request;



	---------------------------------------
	-- Services for the Entity component --
	---------------------------------------
	overriding
	procedure Process_Request(
			Service		: in out Store_Entity_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is
		-- read the entity from the form and process it.
		-- save it back to the database backend afterwards

		Entity: Aw_Ent.Entity_Type'Class := Aw_View.Entities_Helper.Load(
							Request,
							Service.Variable_Prefix
						);
	begin
		Aw_Ent.Store( Entity );
		Response := AWS.Response.URL( AWS.Status.Referer( Request ) );
	end Process_Request;


end Aw_View.Entities;
