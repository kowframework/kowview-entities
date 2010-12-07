--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Tags;
with Ada.Text_IO;	use Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent.ID_Query_Builders;
with KOW_Lib.Log;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;
with KOW_Sec;
with KOW_Sec.Entities;
with KOW_View;
with KOW_View.Components_Registry;
with KOW_View.Entities_Helper;
with KOW_View.Entity_Property_Renderers;
with KOW_View.Entity_Properties;
with KOW_View.Security;

with KOW_View.Entity_Default_Property_Renderers;
pragma Elaborate(KOW_View.Entity_Default_Property_Renderers);
with KOW_View.Entity_Extra_Property_Renderers;
pragma Elaborate( KOW_View.Entity_Extra_Property_Renderers );
with KOW_View.Entity_KVE_Property_Renderers;
pragma Elaborate(KOW_View.Entity_KVE_Property_Renderers);
-- this pkg need elaboration so the renderers are properly registered

with KOW_View.Security;


---------
-- AWS --
---------
with AWS.Messages;
with AWS.MIME;
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




	function Load( The_Tag : in Unbounded_String; ID : in KOW_Ent.ID_Type; Narrow : in Boolean ) return KOW_Ent.Entity_Type'Class is
		Entity : KOW_Ent.Entity_Type'Class := KOW_Ent.Entity_Registry.New_Entity( The_Tag );
	begin
		KOW_Ent.Load( Entity, ID );
		if Narrow then
			return KOW_Ent.Narrow( Entity );
		else
			return Entity;
		end if;
	end Load;



	function Default_Get_IDs(
				Module	: in Entity_Browser_Module;
				Filter	: in String;
				Request	: in AWS.Status.Data
			) return KOW_Ent.Id_Array_type is
		use KOW_Ent.ID_Query_Builders;
		use KOW_Ent.Property_Lists;
		use KOW_Ent;

		package V renames KOW_Lib.UString_Vectors;


		Filter_Values	: V.Vector := KOW_Lib.String_Util.Explode( ' ', Filter );
		Treated_Filter	: Unbounded_String;
		Query		: Query_Type;
		Main_Query	: Query_Type;
		Properties	: KOW_Ent.Property_Lists.List := KOW_Ent.Entity_Registry.Get_Properties( Entity_Tag => Module.Entity_Tag, Force_all => False );
		-- todo :: change force_all to true once we have reimplemented the query builder to use joins




		procedure Filter_Iterator( C : in V.Cursor ) is
		begin
			Treated_Filter := To_Unbounded_String( "%" );
			Append( Treated_Filter, V.Element( C ) );
			Append( Treated_Filter, '%' );
			Append(
					Q		=> Query,
					Column		=> "filter_tags",
					Value		=> Treated_Filter,
					Appender	=> Appender_And,
					Operator	=> Operator_Like
				);
		end Filter_Iterator;




	begin
		Prepare( Query, Module.Entity_Tag );

		V.Iterate( Filter_Values, Filter_Iterator'Access );


		if Module.User_Data_Only then
			Prepare( Main_Query, Module.Entity_Tag );
			declare
				use KOW_sec;
				User : Logged_User_Type := KOW_View.Security.Get_user( Request );
			begin
				Append(
						Q		=> Main_Query,
						Column		=> To_String( Module.User_Identity_Column ),
						Value		=> String( User.User.Identity ),
						Appender	=> Appender_And,
						Operator	=> Operator_Equals
					);
				Append(
						Q		=> Main_Query,
						Child_Q		=> Query,
						Appender	=> Appender_And
					);
			end;
		else
			Main_Query := Query;
		end if;
		
		return Get_All( Main_Query );
	end Default_Get_IDs;

	function Get_IDs(
				Module	: in Entity_Browser_Module;
				Filter	: in String;
				Request	: in AWS.Status.Data
			) return KOW_Ent.Id_Array_type is
	begin

		if Module.Get_IDs_Function /= "" then
			return Get_IDs_Functions_Registry.Get( Module.Get_IDs_Function ).all( Module, Filter, Request );
		else
			return Default_Get_IDs( Module, Filter, Request );
		end if;

	end Get_IDs;




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
			View_Entity.Narrow	:= KOW_Config.Value( Config, "narrow", True );
			View_Entity.Ignore	:= KOW_Lib.String_Util.Explode( ',', To_Unbounded_String( KOW_Config.Value( Config, "ignore", "" ) ) );

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
			begin
				Edit_Entity.Id		:= KOW_Ent.To_ID( KOW_Config.Element( Config, "id" ) );
				Edit_Entity.Has_id	:= True;
			exception
				when others => Edit_Entity.Has_Id := false;
			end;
			Edit_Entity.Template_Name	:= KOW_Config.Value( Config, "template_name", Default_Edit_Entity_Template_Name );
			Edit_Entity.Narrow	:= KOW_Config.Value( Config, "narrow", True );
			Edit_Entity.Ignore	:= KOW_Lib.String_Util.Explode( ',', To_Unbounded_String( KOW_Config.Value( Config, "ignore", "" ) ) );

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

			Edit_Entity.Form_Life_Time := Duration( KOW_Config.Value( Config, "form_life_time", 300.0 ) );


			return Edit_Entity;

		elsif Module_Name = "edit_user_entity" then
			Edit_User.Template_Name := KOW_Config.Value( Config, "template_name", Default_Edit_Entity_Template_Name );

			return Edit_User;

		elsif Module_Name = "create_entity" then
			Create_Entity.Entity_Tag 	:= KOW_Config.Element( Config, "entity_tag" );
			Create_Entity.Template_Name	:= KOW_Config.Value( Config, "template_name", Default_Create_Entity_Template_Name );
			Create_Entity.Ignore		:= KOW_Lib.String_Util.Explode( ',', To_Unbounded_String( KOW_Config.Value( Config, "ignore", "" ) ) );

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

			Create_Entity.Form_Life_Time := Duration( KOW_Config.Value( Config, "form_life_time", 300.0 ) );

			return Create_Entity;
		elsif Module_Name = "entity_browser" then
			Entity_Browser.Entity_Tag := KOW_Config.Element( Config, "entity_tag" );
			Entity_Browser.Allow_Creation := KOW_Config.Value( Config, "allow_creation", False );
			Entity_Browser.Allow_Editting := KOW_Config.Value( Config, "allow_editting", False );
			Entity_Browser.View_Entity_Template_Name := KOW_Config.Value( Config, "view_entity_template_name", Default_View_Entity_Template_Name );
			Entity_Browser.Edit_Entity_Template_Name := KOW_Config.Value( Config, "edit_entity_template_name", Default_Edit_Entity_Template_Name );
			Entity_Browser.Create_Entity_Template_Name := KOW_Config.Value( Config, "edit_entity_template_name", Default_Create_Entity_Template_Name );
			Entity_Browser.List_Entities_Template_Name := KOW_Config.Value( Config, "list_entities_template_name", Default_List_Entities_Template_Name );
			Entity_Browser.Get_IDs_Function := KOW_Config.Value( Config, "get_ids_function", "" );
			Entity_Browser.Narrow := KOW_Config.Value( Config, "narrow", True );
			Entity_Browser.Ignore := KOW_Lib.String_Util.Explode( ',', TO_Unbounded_String( KOW_Config.Value( Config, "ignore", "" ) ) );
			Entity_Browser.User_Data_Only := KOW_Config.Value( Config, "user_data_only", False );
			if Entity_Browser.User_Data_Only then
				Entity_Browser.User_Identity_Column := KOW_Config.Element( Config, "user_identity_column" );
			end if;



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


			Entity_Browser.Form_Life_Time := Duration( KOW_Config.Value( Config, "form_life_time", 300.0 ) );


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
		Store		: Store_Entity_Service;
		File_Download	: File_Download_Service;
		Image_Download	: Image_Download_Service;
	begin
		if Service_Name = "store" then
			return Store;
		elsif Service_Name = "file_download" then
			return File_Download;
		elsif Service_Name = "image_download" then
			return Image_Download;
		else
			raise KOW_View.Components.Service_Error with "unknown service " & service_name;
		end if;
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
		Entity		: KOW_Ent.Entity_Type'Class := Load( Module.Entity_Tag, Module.Id, Module.Narrow );

		My_Parameters : Templates_Parser.Translate_Set;

	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Module.Entity_Tag );

		KOW_View.Entities_Helper.Insert(
					My_Parameters,
					"entity",
					Entity,
					Form_Mode => KOW_View.Entity_Property_Renderers.Edit,
					Ignore => Module.Ignore
				);



		KOW_View.Entities_Helper.Insert_All(
				My_Parameters,
				"inlined_entity",
				Module.Inlined_Entity_Tags,
				Include_Form	=> True,
				Form_Mode	=> KOW_View.Entity_Property_Renderers.Edit,
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

		P : AWS.Parameters.List := AWS.Status.Parameters( Request );
	begin

		-- first we check if we need to get the id from the request..

		if not Module.Has_Id then
			Module.Id := KOW_Ent.To_Id( Natural'Value( AWS.Parameters.Get( P, "entity_id" ) ) );
			Module.Has_id := AWS.Parameters.Get( P, "entity_id" ) /= "";
		end if;

		declare
			Properties	: KOW_Ent.Property_Lists.List;
			Entity		: KOW_Ent.Entity_Type'Class := Load( Module.Entity_Tag, Module.Id, Module.Narrow );
	
			My_Parameters : Templates_Parser.Translate_Set;
	
		begin
			Properties := KOW_Ent.Entity_Registry.Get_Properties( Module.Entity_Tag );
	
			KOW_View.Entities_Helper.Insert(
						My_Parameters,
						"entity",
						Entity,
						Include_Form	=> True,
						Form_Mode	=> KOW_View.Entity_Property_Renderers.Edit,
						Ignore		=> Module.Ignore
					);
	
	
			KOW_View.Entities_Helper.Insert_All(
					My_Parameters,
					"inlined_entity",
					Module.Inlined_Entity_Tags,
					Include_Form	=> True,
					Form_Mode	=> KOW_View.Entity_Property_Renderers.Edit,
					Related_Entity	=> Entity
				);
	
	
	
			KOW_View.Security.Grant_Authorization(
					Request,
					Ada.Tags.Expanded_Name( Entity'Tag ) & "::" & KOW_Ent.To_String( Entity.Id ),
					KOW_View.Security.Edit,
					Module.Form_Life_Time
				);
	
	
			Response := Response &
				To_Unbounded_String(
					Templates_Parser.Parse(
							Get_Template( Module.Template_Name ),
							My_Parameters
						)
					);
		end;
	end Process_Request;



	overriding
	procedure Process_Request(
			Module		: in out Edit_User_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
	begin
		-- TODO :: reimplement edit_user_entity_module as edit_user_module
		null;
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
					Form_Mode	=> KOW_View.Entity_Property_Renderers.Create,
					Ignore		=> Module.Ignore
				);

		KOW_View.Entities_Helper.Insert_All(
				My_Parameters,
				"inlined_entity",
				Module.Inlined_Entity_Tags,
				Include_Form	=> True,
				Form_Mode	=> KOW_View.Entity_Property_Renderers.Create,
				Related_Entity	=> Entity
			);


		KOW_View.Security.Grant_Authorization(
				Request,
				Ada.Tags.Expanded_Name( Entity'Tag ),
				KOW_View.Security.Edit,
				Module.Form_Life_Time
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
		Filter	: String := AWS.Parameters.Get( Params, "filter" );



		procedure Process_Listing_Request is
			All_Ids : KOW_Ent.Id_Array_Type := Get_IDs( Module, Filter, Request );

			Ids_Tag		: Templates_Parser.Tag;
			Labels_Tag	: Templates_Parser.Tag;
			Descriptions_Tag: Templates_Parser.Tag;
			Image_URLs_Tag	: Templates_Parser.Tag;


			My_Parameters : Templates_Parser.Translate_Set := Parameters;


			use Templates_Parser;
		begin
			for i in All_Ids'First .. All_Ids'Last loop
				declare
					Entity : KOW_Ent.Entity_Type'Class := Load( Module.Entity_Tag, All_Ids( i ), Module.Narrow );
				begin

					Ids_Tag := Ids_Tag & KOW_Ent.To_String( Entity.Id );
					Labels_Tag := Labels_Tag & KOW_Ent.To_String( Entity );
					Descriptions_Tag := Descriptions_Tag & KOW_Ent.Describe( Entity );
					Image_URLs_Tag := Image_URLs_Tag & KOW_Ent.Image_URL( Entity );
				end;
			end loop;

			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc( "filter", Filter )
				);

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
			Create_Entity.Ignore := Module.Ignore;
			Create_Entity.Form_Life_Time := Module.Form_Life_Time;

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
			Edit_Entity.Ignore := Module.Ignore;
			Edit_Entity.Form_Life_Time := Module.Form_Life_Time;
			Edit_Entity.Narrow := Module.Narrow;

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
			View_Entity.Ignore := Module.Ignore;
			View_Entity.Narrow := Module.Narrow;

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


	--------------------------
	-- Store Entity Service --
	--------------------------
	
	
	
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

		if Entity in Service_Triggering_Entity_Type'Class then
			Before_Service(
					Entity		=> Service_Triggering_Entity_Type'Class( Entity ),
					Service		=> Service,
					Request		=> Request
				);
		end if;

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

				if Entity in Service_Triggering_Entity_Type'Class then
					Before_Service(
							Entity	=> Service_Triggering_Entity_type'Class( Inlined_Entity ),
							Service	=> Service,
							Request	=> Request
						);
					Store( Inlined_Entity );

					After_Service(
							Entity		=> Service_Triggering_Entity_Type'Class( Inlined_Entity ),
							Service		=> Service,
							Request		=> Request,
							Response	=> Response
						);
				else
					Store( Inlined_Entity );
				end if;
			end;
		end loop;



		Response := AWS.Response.URL( AWS.Status.Referer( Request ) );

		if Entity in Service_Triggering_Entity_Type'Class then
			After_Service(
					Entity		=> Service_Triggering_Entity_Type'Class( Entity ),
					Service		=> Service,
					Request		=> Request,
					Response	=> Response
				);
		end if;

	end Process_Request;





	---------------------------
	-- File Download Service --
	---------------------------

	procedure Get_Filename(
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Column_Name	: in     Unbounded_String;
				Filename	:    out Unbounded_String
			) is
	begin
		Log( "Getting file for entity of tag " & Ada.Tags.Expanded_Name( Entity'Tag ) & " and ID " & KOW_Ent.To_String( Entity.ID ) );

		
		declare
			package P renames KOW_Ent.Property_Lists;
			Properties	: P.List := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );

			procedure Iterator( C : in P.Cursor ) is
				Property : KOW_Ent.Entity_Property_Ptr := P.Element( C );
			begin
				if Property.Column_Name = Column_Name then
					Filename := To_Unbounded_String( KOW_Ent.Get_Property( Property.all, Entity ) );
				end if;
			end Iterator;
		begin
			P.Iterate( Properties, Iterator'Access );
		end;

		
	end Get_Filename;
		




	overriding
	procedure Process_Request(
			Service		: in out File_Download_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is
		-- get an uploaded file, respecting the URL:
		-- 	/service_mapping/[entity_tag]/[entity_id]/[column_name]
		-- serve the file using the name used in the storage

		Request_Info	: constant KOW_Lib.UString_Vectors.Vector := KOW_Lib.String_Util.Explode( '/', AWS.Status.URI( Request ) );

		Entity_Tag	: constant Unbounded_String := KOW_Lib.UString_Vectors.Element( Request_Info, 2 );
		Entity_Id	: constant Natural := Natural'Value( To_String( KOW_Lib.UString_Vectors.Element( Request_Info, 3 ) ) );
		Column_Name	: constant Unbounded_String := KOW_Lib.UString_Vectors.Element( Request_Info, 4 );

		Entity		: KOW_Ent.Entity_Type'Class := Load( Entity_Tag, KOW_Ent.To_ID( Entity_ID ), True );

		Filename	: Unbounded_String;
	begin


		if Entity in Service_Triggering_Entity_Type'Class then
			Before_Service(
					Entity		=> Service_Triggering_Entity_Type'Class( Entity ),
					Service		=> Service,
					Request		=> Request
				);
		end if;

		-- the service

		Get_Filename(
				Entity		=> Entity,
				Column_Name	=> Column_Name,
				Filename	=> Filename
			);

		Response := AWS.Response.File(
				Content_Type	=> AWS.MIME.Content_Type( To_String( Filename ) ),
				Filename	=> To_String( Filename ),
				Cache_Control	=> AWS.Messages.No_Cache
			);


		-- end the service

		if Entity in Service_Triggering_Entity_Type'Class then
			After_Service(
					Entity		=> Service_Triggering_Entity_Type'Class( Entity ),
					Service		=> Service,
					Request		=> Request,
					Response	=> Response
				);
		end if;

	end Process_Request;


	----------------------------
	-- Image Download Service --
	----------------------------


	overriding
	procedure Process_Request(
			Service		: in out Image_Download_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is
		-- same as the file download service, but with the option of showing the thumbnail..


		Request_Info	: constant KOW_Lib.UString_Vectors.Vector := KOW_Lib.String_Util.Explode( '/', AWS.Status.URI( Request ) );

		Entity_Tag	: constant Unbounded_String := KOW_Lib.UString_Vectors.Element( Request_Info, 2 );
		Entity_Id	: constant Natural := Natural'Value( To_String( KOW_Lib.UString_Vectors.Element( Request_Info, 3 ) ) );
		Column_Name	: constant Unbounded_String := KOW_Lib.UString_Vectors.Element( Request_Info, 4 );

		Entity		: KOW_Ent.Entity_Type'Class := Load( Entity_Tag, KOW_Ent.To_ID( Entity_ID ), True );

		Filename	: Unbounded_String;


		Params		: AWS.Parameters.List := AWS.Status.Parameters( Request );
		Mode		: String := AWS.Parameters.Get( Params, "mode" );
	begin

		if Entity in Service_Triggering_Entity_Type'Class then
			Before_Service(
					Entity		=> Service_Triggering_Entity_Type'Class( Entity ),
					Service		=> Service,
					Request		=> Request
				);
		end if;

		-- the service

		Get_Filename(
				Entity		=> Entity,
				Column_Name	=> Column_Name,
				Filename	=> Filename
			);

		if mode = "thumbnail" then
			declare
				thumb : String := KOW_View.Entity_Properties.Thumb_name( To_String( Filename ) );
			begin
				Response := AWS.Response.File(
						Content_Type	=> AWS.MIME.Content_Type( Thumb ),
						Filename	=> Thumb,
						Cache_Control	=> AWS.Messages.No_Cache
					);
			end;
		else
			Response := AWS.Response.File(
					Content_Type	=> AWS.MIME.Content_Type( To_String( Filename ) ),
					Filename	=> To_String( Filename ),
					Cache_Control	=> AWS.Messages.No_Cache
				);
		end if;

		-- end the service

		if Entity in Service_Triggering_Entity_Type'Class then
			After_Service(
					Entity		=> Service_Triggering_Entity_Type'Class( Entity ),
					Service		=> Service,
					Request		=> Request,
					Response	=> Response
				);
		end if;

	end Process_Request;




	protected body Get_IDs_Functions_Registry is
		function Get( Name : in Unbounded_String ) return Get_IDs_Function_Access is
		begin
			return Get_IDs_Function_Maps.Element( My_Map, Name );
		end Get;

		procedure Register( Name : in String; Function_Access : in Get_IDs_Function_Access ) is
		begin
			Get_IDs_Function_Maps.Include( My_Map, To_Unbounded_String( Name ), Function_Access );
		exception
			when CONSTRAINT_ERROR =>
				raise Constraint_Error with Name & " alredy in the Get_IDs Functions Registry";
		end Register;
	end Get_IDs_Functions_Registry;


end KOW_View.Entities;
