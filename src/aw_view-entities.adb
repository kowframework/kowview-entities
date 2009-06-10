--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Text_IO;	use Ada.Text_IO;

---------------
-- Ada Works --
---------------
with Aw_View;
with Aw_View.Components_Registry;
with Aw_View.Entities_Helper;
with Aw_View.Security;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;
with Templates_Parser;


package body Aw_View.Entities is

	----------------------
	-- Helper Functions --
	----------------------
	function Get_Template( Template_Name : Unbounded_String ) return String is
	begin
		return Aw_View.Components_Registry.Locate_Resource(
						Component_Name  => "entities",
						Resource        => To_String( Template_Name ),
						Extension       => "html",
						Kind            => Ada.Directories.Ordinary_File
					);
	end Get_Template;



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





	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Aw_View.Components.Module_Instance_Interface'Class is
	-- create a module instance by given name
	-- Available module:
	-- 	view_entity
		View_Entity 	: View_Entity_Module;
		Edit_Entity 	: Edit_Entity_Module;
		Create_Entity 	: Create_Entity_Module;
		Entity_Browser	: Entity_Browser_Module;
	begin
		if Module_Name = "view_entity" then
			View_Entity.Entity_Tag	:= Aw_Config.Element( Config, "entity_tag" );
			View_Entity.Id		:= Aw_Ent.To_ID( Aw_Config.Element( Config, "id" ) );
			View_Entity.Template_Name	:= Aw_Config.Value( Config, "template_name", Default_View_ENtity_Template_Name );

			return View_Entity;
		elsif Module_Name = "edit_entity" then
			Edit_Entity.Entity_Tag	:= Aw_Config.Element( Config, "entity_tag" );
			Edit_Entity.Id		:= Aw_Ent.To_ID( Aw_Config.Element( Config, "id" ) );
			Edit_Entity.Template_Name	:= Aw_Config.Value( Config, "template_name", Default_Edit_Entity_Template_Name );

			return Edit_Entity;

		elsif Module_Name = "create_entity" then
			Create_Entity.Entity_Tag 	:= Aw_Config.Element( Config, "entity_tag" );
			Create_Entity.Template_Name	:= Aw_Config.Value( Config, "template_name", Default_Create_Entity_Template_Name );

			return Create_Entity;
		elsif Module_Name = "entity_browser" then
			Entity_Browser.Entity_Tag := Aw_Config.Element( Config, "entity_tag" );
			Entity_Browser.Allow_Creation := Aw_Config.Value( Config, "allow_creation", False );
			Entity_Browser.Allow_Editting := Aw_Config.Value( Config, "allow_editting", False );
			Entity_Browser.View_Entity_Template_Name := Aw_Config.Value( Config, "view_entity_template_name", Default_View_Entity_Template_Name );
			Entity_Browser.Edit_Entity_Template_Name := Aw_Config.Value( Config, "edit_entity_template_name", Default_Edit_Entity_Template_Name );
			Entity_Browser.Create_Entity_Template_Name := Aw_Config.Value( Config, "edit_entity_template_name", Default_Create_Entity_Template_Name );
			Entity_Browser.List_Entities_Template_Name := Aw_Config.Value( Config, "list_entities_template_name", Default_List_Entities_Template_Name );


			return Entity_Browser;

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



	--
	-- View_Entity
	--

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
						Get_Template( Module.Template_Name ),
						My_Parameters
					)
				);
	end Process_Request;

	--
	-- Edit_Entity
	--

	overriding
	procedure Process_Request(
			Module		: in out Edit_Entity_Module;
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
					Entity,
					Include_Form => True
				);


		Aw_View.Security.Grant_Authorization(
				Request,
				Ada.Tags.Expanded_Name( Entity'Tag ) & "::" & Aw_Ent.To_String( Entity.Id ),
				Aw_View.Security.Edit
			);


		Response := Response &
			To_Unbounded_String(
				Templates_Parser.Parse(
						Get_Template( Module.Template_Name ),
						My_Parameters
					)
				);
	end Process_Request;



	--
	-- Create Entity
	--

	overriding
	procedure Process_Request(
			Module		: in out Create_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		use Templates_Parser;


		Properties	: Aw_Ent.Property_Lists.List;
		Entity		: Aw_Ent.Entity_Type'Class := Aw_Ent.New_Entity( Module.Entity_Tag );

		My_Parameters : Templates_Parser.Translate_Set;

	begin
		Properties := Aw_Ent.Entity_Registry.Get_Properties( Module.Entity_Tag );

		Aw_View.Entities_Helper.Insert(
					My_Parameters,
					"entity",
					Entity,
					Include_Form => True
				);


		Aw_View.Security.Grant_Authorization(
				Request,
				Ada.Tags.Expanded_Name( Entity'Tag ),
				Aw_View.Security.Edit
			);


		Response := Response &
			To_Unbounded_String(
				Templates_Parser.Parse(
						Get_Template( Module.Template_Name ),
						My_Parameters
					)
				);
	end Process_Request;





	--
	-- Entity Browser
	--
	overriding
	procedure Process_Request(
			Module		: in out Entity_Browser_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- the entire cycle for this module is inside this single procedure.
		
		Params	: AWS.Parameters.List := AWS.Status.Parameters( Request );
		Action	: String := AWS.Parameters.Get( Params, "entity_browser_action" );
		ID	: String := AWS.Parameters.Get( Params, "entity_browser_id" );



		procedure Process_Listing_Request is
			Entity  : Aw_Ent.Entity_Type'Class := Aw_Ent.New_Entity( Module.Entity_Tag );
			All_Ids : Aw_Ent.Id_Array_Type := Aw_Ent.Get_All_Ids( Module.Entity_Tag );

			Ids_Tag		: Templates_Parser.Tag;
			Labels_Tag	: Templates_Parser.Tag;


			My_Parameters : Templates_Parser.Translate_Set := Parameters;


			use Templates_Parser;
		begin
			for i in All_Ids'First .. All_Ids'Last loop
				Aw_Ent.Load( Entity, All_Ids( i ) );

				Ids_Tag := Ids_Tag & Aw_Ent.To_String( Entity.Id );
				Labels_Tag := Labels_Tag & Aw_Ent.To_String( Entity );
			end loop;


			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc( "ids", Ids_Tag )
				);

			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc( "labels", Labels_Tag )
				);

			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc( "allow_editting", Module.Allow_Editting )
				);

			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc( "allow_creation", Module.Allow_Creation )
				);

			Response := Response &
				To_Unbounded_String(
					Templates_Parser.Parse(
							Get_Template( Module.List_Entities_Template_Name ),
							My_Parameters
						)
					);

		end Process_Listing_Request;


		procedure Process_Creation_Request is
			Create_Entity : Create_Entity_Module;
		begin
			Create_Entity.Entity_Tag := Module.Entity_Tag;
			Create_Entity.Template_Name := Module.Create_Entity_Template_Name;

			Process_Request(
					Create_Entity,
					Request,
					Parameters,
					Response
				);

		end Process_Creation_Request;


		procedure Process_Editting_Request is
			Edit_Entity : Edit_Entity_Module;
		begin
			Edit_Entity.Entity_Tag := Module.Entity_Tag;
			Edit_Entity.Id := Aw_Ent.To_Id( Natural'Value( ID ) );
			Edit_Entity.Template_Name := Module.Edit_Entity_Template_Name;

			Process_Request(
					Edit_Entity,
					Request,
					Parameters,
					Response
				);
		end Process_Editting_Request;
		

		procedure Process_Viewing_Request is
			View_Entity : View_Entity_Module;
		begin
			View_Entity.Entity_Tag := Module.Entity_Tag;
			View_Entity.Id := Aw_Ent.To_Id( Natural'Value( ID ) );
			View_Entity.Template_Name := Module.View_Entity_Template_Name;

			Process_Request(
					View_Entity,
					Request,
					Parameters,
					Response
				);
		end Process_Viewing_Request;


	begin
		if Action = "" or else Action = "list" then
			Process_Listing_Request;
		elsif Module.Allow_Creation and Action = "create" then
			Process_Creation_Request;
		elsif Module.Allow_Editting and Action = "edit" and ID /= "" then
			Process_Editting_Request;
		elsif Action = "view" and ID /= "" then
			Process_Viewing_Request;
		else
			Response := Response & "[ERROR :: BAD REQUEST]";
		end if;
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

		use Aw_Ent;
		use Ada.Tags;
	begin

		if Entity.Id.My_Tag = Ada.Tags.No_Tag then
			Aw_View.Security.Request_Authorization(
					Request,
					Ada.Tags.Expanded_Name( Entity'Tag ),
					Aw_View.Security.Create
				);
		else
			Aw_View.Security.Request_Authorization(
					Request,
					Ada.Tags.Expanded_Name( Entity'Tag ) & "::" & Aw_Ent.To_String( Entity.Id ),
					Aw_View.Security.Edit
				);
		end if;
		
		Aw_Ent.Store( Entity );
		Response := AWS.Response.URL( AWS.Status.Referer( Request ) );
	end Process_Request;


end Aw_View.Entities;
