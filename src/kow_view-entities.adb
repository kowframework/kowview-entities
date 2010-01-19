--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Tags;
with Ada.Text_IO;	use Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Log;
with KOW_Sec;
with KOW_Sec.Authentication.Entities;
with KOW_View;
with KOW_View.Components_Registry;
with KOW_View.Entities_Helper;
with KOW_View.Security;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;
with Templates_Parser;


package body KOW_View.Entities is

	Logger : KOW_Lib.Log.Logger_Type := KOW_Lib.Log.Get_Logger( "KOW_View.Entities" );

	procedure Log( Message : in String; Level : in KOW_Lib.Log.Log_Level := KOW_Lib.Log.Level_Debug ) is
	begin
		KOW_Lib.Log.Log(
				Logger	=> Logger,
				Level	=> Level,
				Message	=> Message
			);
	end Log;

	----------------------
	-- Helper Functions --
	----------------------
	function Get_Template( Template_Name : Unbounded_String ) return String is
	begin
		return KOW_View.Components_Registry.Locate_Resource(
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
			Config		: in out KOW_Config.Config_File
		) is
	begin
		null;
	end Initialize;





	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return KOW_View.Components.Module_Instance_Interface'Class is
	-- create a module instance by given name
	-- Available module:
	-- 	view_entity
		View_Entity 	: View_Entity_Module;
		Edit_Entity 	: Edit_Entity_Module;
		Edit_User	: Edit_User_Entity_Module;
		Create_Entity 	: Create_Entity_Module;
		Entity_Browser	: Entity_Browser_Module;
	begin
		if Module_Name = "view_entity" then
			View_Entity.Entity_Tag	:= KOW_Config.Element( Config, "entity_tag" );
			View_Entity.Id		:= KOW_Ent.To_ID( KOW_Config.Element( Config, "id" ) );
			View_Entity.Template_Name	:= KOW_Config.Value( Config, "template_name", Default_View_ENtity_Template_Name );

			declare
				Inlined_Forms	: KOW_Config.Config_File_Array := KOW_Config.Elements_Array( Config, "inlined" );
			begin
				for i in Inlined_Forms'Range loop
					KOW_Lib.UString_Vectors.Append(
							View_Entity.Inlined_Entity_Tags,
							KOW_Config.Element( Inlined_Forms(i), "entity_tag" )
						);
				end loop;
			end;



			return View_Entity;
		elsif Module_Name = "edit_entity" then
			Edit_Entity.Entity_Tag	:= KOW_Config.Element( Config, "entity_tag" );
			Edit_Entity.Id		:= KOW_Ent.To_ID( KOW_Config.Element( Config, "id" ) );
			Edit_Entity.Template_Name	:= KOW_Config.Value( Config, "template_name", Default_Edit_Entity_Template_Name );

			declare
				Inlined_Forms	: KOW_Config.Config_File_Array := KOW_Config.Elements_Array( Config, "inlined" );
			begin
				for i in Inlined_Forms'Range loop
					KOW_Lib.UString_Vectors.Append(
							Edit_Entity.Inlined_Entity_Tags,
							KOW_Config.Element( Inlined_Forms(i), "entity_tag" )
						);
				end loop;
			end;



			return Edit_Entity;

		elsif Module_Name = "edit_user_entity" then
			Edit_User.Template_Name := KOW_Config.Value( Config, "template_name", Default_Edit_Entity_Template_Name );

			return Edit_User;

		elsif Module_Name = "create_entity" then
			Create_Entity.Entity_Tag 	:= KOW_Config.Element( Config, "entity_tag" );
			Create_Entity.Template_Name	:= KOW_Config.Value( Config, "template_name", Default_Create_Entity_Template_Name );

			declare
				Inlined_Forms	: KOW_Config.Config_File_Array := KOW_Config.Elements_Array( Config, "inlined" );
			begin
				for i in Inlined_Forms'Range loop
					KOW_Lib.UString_Vectors.Append(
							Create_Entity.Inlined_Entity_Tags,
							KOW_Config.Element( Inlined_Forms(i), "entity_tag" )
						);
				end loop;
			end;

			return Create_Entity;
		elsif Module_Name = "entity_browser" then
			Entity_Browser.Entity_Tag := KOW_Config.Element( Config, "entity_tag" );
			Entity_Browser.Allow_Creation := KOW_Config.Value( Config, "allow_creation", False );
			Entity_Browser.Allow_Editting := KOW_Config.Value( Config, "allow_editting", False );
			Entity_Browser.View_Entity_Template_Name := KOW_Config.Value( Config, "view_entity_template_name", Default_View_Entity_Template_Name );
			Entity_Browser.Edit_Entity_Template_Name := KOW_Config.Value( Config, "edit_entity_template_name", Default_Edit_Entity_Template_Name );
			Entity_Browser.Create_Entity_Template_Name := KOW_Config.Value( Config, "edit_entity_template_name", Default_Create_Entity_Template_Name );
			Entity_Browser.List_Entities_Template_Name := KOW_Config.Value( Config, "list_entities_template_name", Default_List_Entities_Template_Name );



			declare
				Inlined_Forms	: KOW_Config.Config_File_Array := KOW_Config.Elements_Array( Config, "inlined" );
			begin
				for i in Inlined_Forms'Range loop
					KOW_Lib.UString_Vectors.Append(
							Entity_Browser.Inlined_Entity_Tags,
							KOW_Config.Element( Inlined_Forms(i), "entity_tag" )
						);
				end loop;
			end;


			return Entity_Browser;

		else
			raise KOW_View.Components.MODULE_ERROR with "Unknown module :: " & Module_Name;
		end if;
	end Create_Instance;



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return KOW_View.Components.Service_Instance_Interface'Class is
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


		Properties	: KOW_Ent.Property_Lists.List;
		Entity		: KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity( Module.Entity_Tag );

		My_Parameters : Templates_Parser.Translate_Set;

	begin
		KOW_Ent.Load( Entity, Module.Id );
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Module.Entity_Tag );

		KOW_View.Entities_Helper.Insert(
					My_Parameters,
					"entity",
					Entity,
					Form_Mode => KOW_View.Entities_Helper.Edit
				);



		KOW_View.Entities_Helper.Insert_All(
				My_Parameters,
				"inlined_entity",
				Module.Inlined_Entity_Tags,
				Include_Form	=> True,
				Form_Mode	=> KOW_View.Entities_Helper.Edit,
				Related_Entity	=> Entity
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


		Properties	: KOW_Ent.Property_Lists.List;
		Entity		: KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity( Module.Entity_Tag );

		My_Parameters : Templates_Parser.Translate_Set;

	begin
		Log( "Editing entity of tag " & To_String( Module.Entity_Tag ) );
		KOW_Ent.Load( Entity, Module.Id );
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Module.Entity_Tag );

		KOW_View.Entities_Helper.Insert(
					My_Parameters,
					"entity",
					Entity,
					Include_Form	=> True,
					Form_Mode	=> KOW_View.Entities_Helper.Edit
				);


		KOW_View.Entities_Helper.Insert_All(
				My_Parameters,
				"inlined_entity",
				Module.Inlined_Entity_Tags,
				Include_Form	=> True,
				Form_Mode	=> KOW_View.Entities_Helper.Edit,
				Related_Entity	=> Entity
			);



		KOW_View.Security.Grant_Authorization(
				Request,
				Ada.Tags.Expanded_Name( Entity'Tag ) & "::" & KOW_Ent.To_String( Entity.Id ),
				KOW_View.Security.Edit
			);


		Response := Response &
			To_Unbounded_String(
				Templates_Parser.Parse(
						Get_Template( Module.Template_Name ),
						My_Parameters
					)
				);
	end Process_Request;



	overriding
	procedure Process_Request(
			Module		: in out Edit_User_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- draw up the form for the loged user
		-- if no user is loged ir it's loged as other user type, return an empty string
		--
		-- also, make sure to reload the entity from the database at loading time
		The_User : KOW_Sec.User_Access := KOW_View.Security.Get_User( Request );
		use KOW_Sec;
	begin
		if The_User = null then
			Response := Response & "you should be loged in to do that";
			return;
		elsif The_User.all not in KOW_Sec.Authentication.Entities.User_Type'Class then
			Response := Response & "invalid user type :: " & Ada.Tags.Expanded_Name( The_User.all'Tag );
			return;
		end if;


		-- if got here, time to work things out

		Module.ID := KOW_Sec.Authentication.Entities.User_Type'Class( The_User.all ).ID;
		Module.Entity_Tag := To_Unbounded_String(
					Ada.Tags.Expanded_Name( Module.ID.My_Tag )
				);

		-- time to reload the entity...
		declare
			Entity : KOW_Sec.Authentication.Entities.User_Entity_Type'Class :=
					KOW_Sec.Authentication.Entities.To_User_Entity(
							KOW_Sec.Authentication.Entities.User_Type'Class( The_User.all )
						);
		begin
			KOW_Ent.Load( Entity, Module.ID );

			KOW_Sec.Authentication.Entities.User_Type'Class( The_User.all ) := KOW_Sec.Authentication.Entities.To_User( Entity );
			-- in order for these operations to be safe, we need the user types to be quite consistent
		end;

		Process_Request(
				Edit_Entity_Module( Module ),
				Request,
				Parameters,
				Response
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


		Properties	: KOW_Ent.Property_Lists.List;
		Entity		: KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity( Module.Entity_Tag );

		My_Parameters : Templates_Parser.Translate_Set;

	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Module.Entity_Tag );

		Log( "Creating entity of tag " & To_String( Module.Entity_Tag ) );
		KOW_View.Entities_Helper.Insert(
					My_Parameters,
					"entity",
					Entity,
					Include_Form	=> True,
					Form_Mode	=> KOW_View.Entities_Helper.Create
				);

		KOW_View.Entities_Helper.Insert_All(
				My_Parameters,
				"inlined_entity",
				Module.Inlined_Entity_Tags,
				Include_Form	=> True,
				Form_Mode	=> KOW_View.Entities_Helper.Create,
				Related_Entity	=> Entity
			);


		KOW_View.Security.Grant_Authorization(
				Request,
				Ada.Tags.Expanded_Name( Entity'Tag ),
				KOW_View.Security.Edit
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
			Entity  : KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity( Module.Entity_Tag );
			All_Ids : KOW_Ent.Id_Array_Type := KOW_Ent.Get_All_Ids( Module.Entity_Tag );

			Ids_Tag		: Templates_Parser.Tag;
			Labels_Tag	: Templates_Parser.Tag;
			Descriptions_Tag: Templates_Parser.Tag;
			Image_URLs_Tag	: Templates_Parser.Tag;


			My_Parameters : Templates_Parser.Translate_Set := Parameters;


			use Templates_Parser;
		begin
			for i in All_Ids'First .. All_Ids'Last loop
				KOW_Ent.Load( Entity, All_Ids( i ) );

				Ids_Tag := Ids_Tag & KOW_Ent.To_String( Entity.Id );
				Labels_Tag := Labels_Tag & KOW_Ent.To_String( Entity );
				Descriptions_Tag := Descriptions_Tag & KOW_Ent.Describe( Entity );
				Image_URLs_Tag := Image_URLs_Tag & KOW_Ent.Image_URL( Entity );
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
					Templates_Parser.Assoc( "descriptions", Descriptions_Tag )
				);
			Templates_Parser.Insert(
					My_parameters,
					Templates_Parser.Assoc( "image_urls", Image_URLs_Tag )
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
			Create_Entity.Inlined_Entity_Tags := Module.Inlined_Entity_Tags;

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
			Edit_Entity.Id := KOW_Ent.To_Id( Natural'Value( ID ) );
			Edit_Entity.Template_Name := Module.Edit_Entity_Template_Name;
			Edit_Entity.Inlined_Entity_Tags := Module.Inlined_Entity_Tags;

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
			View_Entity.Id := KOW_Ent.To_Id( Natural'Value( ID ) );
			View_Entity.Template_Name := Module.View_Entity_Template_Name;
			View_Entity.Inlined_Entity_Tags := Module.Inlined_Entity_Tags;

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

		Entity	: KOW_Ent.Entity_Type'Class := KOW_View.Entities_Helper.Load(
							Request,
							Service.Variable_Prefix,
							1
						);
		Params	: AWS.Parameters.List := AWS.Status.Parameters( Request );


		Inlined_Tag	: Unbounded_String;
		Inlined_Count	: Positive := 1;

		use KOW_Ent;
		use Ada.Tags;
	begin

		if Entity.Id.My_Tag = Ada.Tags.No_Tag then
			KOW_View.Security.Request_Authorization(
					Request,
					Ada.Tags.Expanded_Name( Entity'Tag ),
					KOW_View.Security.Create
				);
		else
			KOW_View.Security.Request_Authorization(
					Request,
					Ada.Tags.Expanded_Name( Entity'Tag ) & "::" & KOW_Ent.To_String( Entity.Id ),
					KOW_View.Security.Edit
				);
		end if;
		
		KOW_Ent.Store( Entity );


		for N in 1 .. AWS.Parameters.Count( Params, Service.Inlined_Variable_Prefix & "_tag" ) loop
			Log( "loading entity" );
			declare
				Current_Inlined_Tag : Unbounded_String := 
							To_Unbounded_String(
								AWS.Parameters.Get(
										Params,
										Service.inlined_Variable_Prefix & "_tag"
									)
							);

				Inlined_Entity : KOW_Ent.Entity_Type'Class :=
								KOW_Ent.Entity_Registry.New_Entity(
										Current_Inlined_Tag
									);
			begin
				if Inlined_Tag /= Current_Inlined_Tag then
					Inlined_Tag := Current_Inlined_Tag;
					Inlined_Count := 1;
				else
					Inlined_Count := Inlined_Count + 1;
				end if;
				KOW_View.Entities_Helper.Load(
							Data		=> Request,
							Variable_Prefix	=> Service.Inlined_Variable_Prefix,
							Entity		=> Inlined_Entity,
							N		=> Inlined_Count
						);

				Log( "entity loaded :: " & To_String( Inlined_Tag ) & " :: " & Positive'Image( Inlined_Count ) );



				-- TODO :: deal with security for inlined entities...
				-- NOTE :: there is a problem where I create more than one entity... how do I control that?
				-- NOTE :: it's simple, as long as I have a map of granted authorizations for this request
				KOW_Ent.Set_Foreign_Key( Entity => Inlined_Entity, Related_Entity => Entity );
				Store( Inlined_Entity );
			end;
		end loop;

		Response := AWS.Response.URL( AWS.Status.Referer( Request ) );
	end Process_Request;



end KOW_View.Entities;
