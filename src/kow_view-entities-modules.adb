------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWView is free software; you can redistribute it  and/or modify it under--
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWView is distributed in the hope that it will be useful,but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWView; see file COPYING.  If not, write--
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
------------------------------------------------------------------------------
pragma License( GPL );


--------------
-- Ada 2005 --
--------------
with Ada.Strings;
with Ada.Strings.Fixed;

-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Config;
with KOW_Ent;
with KOW_Ent.ID_Query_Builders;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_Sec;
with KOW_View.Entities.Components;
with KOW_View.Entities.Property_Renderers;
with KOW_View.Entities.Validation;
with KOW_View.Entities.Toolbar_Util;				use KOW_View.Entities.Toolbar_Util;
with KOW_View.Locales;
with KOW_View.Modules;
with KOW_View.Modules.Stateless_Module_Factories;
with KOW_View.Security;
with KOW_View.URI_Util;

---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;

package body KOW_View.Entities.Modules is

	------------------------
	-- Entity_Module_Type --
	------------------------
	
	-- this is the basic module type for all entity handling operations...
	-- 
	-- it's an abstract type to enforce it not being instanciated as it does nothing. :)
	

	overriding
	procedure Initialize_Request(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			) is
		-- setup configurable variables
		-- if you override this method remember to call the Entity_Module_Type's implementation of it.

		function Get_Style return Rendering_Style_Type is
		begin
			return Rendering_Style_Type'Value( AWS.Parameters.Get( AWS.Status.Parameters( Request ), "style" ) & "_Rendering" );
		exception
			when others => return Small_Rendering;
		end Get_Style;


		L : constant KOW_Lib.Locales.Locale := KOW_View.Locales.Get_Locale( Request );
		function E( Key, Default : in String ) return Unbounded_String is
		begin
			if KOW_Config.Has_Element( Config, Key ) then
				return KOW_Config.Element( Config, To_Unbounded_String( Key ), L.CODE );
			else
				return To_Unbounded_String( Default );
			end if;
		end E;

	begin
		Module.Entity_Tag := KOW_Config.Value( Config, "entity_tag", "" );

		Module.New_Label	:= E( "new_label", "Create" );
		Module.Edit_Label	:= E( "edit_label", "Edit" );
		Module.Submit_Label	:= E( "submit_label", "Submit" );
		Module.List_Label	:= E( "list_label", "List" );

		Module.Narrow := KOW_Config.Value( Config, "narrow", True );
		Module.Style := Get_Style;

		Include_Component_Script( Module, KOW_View.Entities.Components.Component, "kowview-entities.js" );
		Include_Component_CSS( Module, KOW_View.Entities.Components.Component, "kowview-entities.css" );
	end Initialize_Request;

	overriding
	procedure Process_Body(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			) is
		Entity_Id	: Integer := Get_Entity_id( Entity_Module_Type'Class( Module ), Request );
	begin

		Append( Output, "<span class=""" & KOW_View.Modules.Get_Name( Entity_Module_Type'Class( Module ) ) & ' '  );
		Append( Output, Rendering_Style_Type'Image( Module.Style ) & """>" );
		if Entity_Id = -1 and then ( Module.Style = Small_Rendering or Module.Style = Big_Rendering ) then
			Render_List(
					Module	=> Entity_Module_Type'Class( Module ),
					Request	=> Request,
					Output	=> Output
				);
		else
			declare
				Entity	: KOW_Ent.Entity_Type'Class := Load_Entity( Module, Entity_ID );
			begin
				Render_View(
						Module	=> Entity_Module_Type'Class( Module ),
						Request	=> Request,
						Entity	=> Entity,
						Output	=> Output
					);
			end;
		end if;

		Append( Output, "</span>" );
	end Process_Body;


	overriding
	procedure Process_Json_Request(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			) is
		Entity_Id	: Integer := Get_Entity_id( Entity_Module_Type'Class( Module ), Request );
		Entity		: KOW_Ent.Entity_Type'Class := Load_Entity( Module, Entity_ID );

		Resp		: KOW_Lib.Json.Object_Type;
	begin

		-- security measures...

		if KOW_Ent.Is_New( Entity ) then
			if not Can_Create( Entity_Module_Type'Class( Module ), Request ) then
				raise KOW_Sec.Access_Denied with "Can't create entity";
			end if;
		else
			if not Can_Edit( Entity_Module_type'Class( Module ), Request, Entity ) then
				raise KOW_Sec.Access_Denied with "Can't edit the entity " & KOW_Ent.To_String( Entity );
			end if;
		end if;

		Set_Values(
				Module	=> Module,
				Entity	=> Entity,
				Request	=> Request
			);

		if Entity in KOW_View.Entities.Validation.Validatable_Entity_Interface'Class then
			KOW_View.Entities.Validation.Validate(
						Entity	=> KOW_View.Entities.Validation.Validatable_Entity_Interface'Class( Entity ),
						Request	=> Request
					);
		end if;

		KOW_Lib.Json.Set( Resp, "is_new", KOW_Ent.Is_New( Entity ) );

		Before_Store(
				Entity	=> Entity,
				Request	=> Request
			);

		KOW_Ent.Store( Entity );

		After_Store(
				Entity	=> Entity,
				Request	=> Request
			);

		KOW_Lib.Json.Set( Resp, "entity_id", Integer( Entity.ID.Value ) );
		
		Response := Resp;
	end Process_Json_Request;


	----------------------
	-- Security Methods --
	----------------------

	function Can_Create(
				Module	: in     Entity_Module_Type;
				Request	: in     AWS.Status.Data
			) return Boolean is
		-- check if the user can created entities
		-- default implementation check if the user is online only
	begin
		return not KOW_Sec.Is_Anonymous( KOW_View.Security.Get_User( Request ) );
	end Can_Create;
	
	function Can_Edit(
				Module	: in     Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class
			) return Boolean is
		-- check if the user can edit the given entity
	begin
		return not KOW_Sec.Is_Anonymous( KOW_View.Security.Get_User( Request ) );
	end Can_Edit;


	-------------------
	-- Data Handling --
	-------------------


	function Get_Entity_id(
				Module	: in Entity_Module_Type;
				Request	: in AWS.Status.Data
			) return Integer is
		ID_Str : constant String := AWS.Parameters.Get( AWS.Status.Parameters( Request ), "entity_id" );
	begin
		if ID_Str = "" then
			return -1;
		else
			return Integer'Value( ID_Str );
		end if;
	end Get_Entity_Id;


	function Query_Entities(
				Module	: in Entity_Module_Type;
				Request	: in AWS.Status.Data;
				From	: in Positive;
				Limit	: in Natural
			) return KOW_Ent.Id_Array_Type is
		use KOW_Ent.Id_Query_Builders;

		Q : Query_Type;
	begin
		Prepare(
				Q		=> Q,
				Entity_Tag	=> Module.Entity_Tag
			);

		return Get_Some(
				Q		=> Q,
				From		=> From,
				Limit		=> Limit
			);
	end Query_Entities;


	function New_Entity(
				Module	: in Entity_Module_Type
			) return KOW_Ent.Entity_Type'Class is
	begin
		if Module.Entity_Tag = "" then
			raise CONSTRAINT_ERROR with "you need to supply the 'entity_tag' parameter";
		end if;
		return KOW_Ent.Entity_Registry.New_Entity( Module.Entity_Tag );
	end New_Entity;
	
	function Load_Entity(
				Module	: in Entity_Module_Type;
				Id	: in Integer
			) return KOW_Ent.Entity_Type'Class is
	begin
		if Id = -1 then
			return New_Entity( Entity_Module_Type'Class( Module ) );
		end if;
		return Load_Entity( Module, KOW_Ent.To_Id( ID ) );
	end Load_Entity;

	function Load_Entity(
				Module	: in Entity_Module_Type;
				Id	: in KOW_Ent.Id_Type
			) return KOW_Ent.Entity_Type'Class is
		use APQ;
		Entity : KOW_Ent.Entity_Type'Class := New_Entity( Entity_Module_Type'Class( Module ) );
	begin
		KOW_Ent.Load( Entity, Id );
		if Module.Narrow then
			return KOW_Ent.Narrow( Entity );
		else
			return Entity;
		end if;
	end Load_Entity;


	function Get_Properties(
				Module	: in Entity_Module_Type;
				Entity	: in KOW_Ent.Entity_Type'Class
			) return KOW_Ent.Property_Lists.List is
		-- get the properties that will be used by this module..
	begin
		return KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
	end Get_Properties;



	procedure Set_Values(
				Module	: in out Entity_Module_Type;
				Entity	: in out KOW_Ent.Entity_Type'Class;
				Request	: in     AWS.Status.Data
			) is
		Properties : KOW_Ent.Property_Lists.List := Get_Properties( Entity_Module_Type'Class( Module ), Entity );

		Params	: AWS.Parameters.List := AWS.Status.Parameters( Request );



		procedure Validation_Iterator( C : in KOW_Ent.Property_Lists.Cursor ) is
			Property : KOW_Ent.Entity_Property_Ptr := KOW_Ent.Property_Lists.Element( C );
		begin
			if KOW_Ent.Is_New( Entity ) or not Property.Immutable then
				-- we do not touch immutable values as probably we won't even receive
				-- a value from the browser, which would cause an exception
				KOW_View.Entities.Validation.Validate_Property(
						Entity		=> KOW_View.Entities.Validation.Validatable_Entity_Interface'Class( Entity ),
						Property	=> Property,
						Value		=> AWS.Parameters.Get( Params, To_String( Property.all.Column_Name ) )
					);
			end if;
		end Validation_Iterator;


		procedure Iterator( C : in KOW_Ent.Property_Lists.Cursor ) is
			Property : KOW_Ent.Entity_Property_Ptr := KOW_Ent.Property_Lists.Element( C );
		begin
			if KOW_Ent.Is_New( Entity ) or not Property.Immutable then
				-- we do not touch immutable values as probably we won't even receive
				-- a value from the browser, which would cause an exception
				KOW_Ent.Set_Property(
						Property	=> Property.all,
						Entity		=> Entity,
						Value		=> AWS.Parameters.Get( Params, To_String( Property.all.Column_Name ) )
					);
			end if;
		end Iterator;
	begin
		if Entity in KOW_View.Entities.Validation.Validatable_Entity_Interface'Class then
			KOW_Ent.Property_Lists.Iterate( Properties, Validation_Iterator'Access );
		end if;
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
	end Set_Values;



	-----------------------
	-- Rendering Methods --
	-----------------------


	procedure Render_List(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			) is

		From	: constant Positive := Get_List_From( Entity_Module_Type'Class( Module ), Request );
		Limit	: constant Natural  := Get_List_Limit( Entity_Module_Type'Class( Module ), Request );

		Ids : KOW_Ent.Id_Array_Type := Query_Entities(
								Module	=> Entity_Module_Type'Class( Module ),
								Request	=> Request,
								From	=> From,
								Limit	=> Limit
							);
	begin
		Render_List_Title(
					Module		=> Entity_MOdule_type'Class( Module ),
					Request		=> Request,
					Output		=> Output
				);

		if Module.Enable_List_Navigation_Bar( Top ) then
			Render_List_Navigation_Bar(
						Module		=> Entity_Module_Type'Class( Module ),
						Request		=> Request,
						From		=> From,
						Limit		=> Limit,
						Total_Shown	=> Ids'Length,
						Output		=> Output
					);
		end if;



		Render_List_Body(
				Module	=> Entity_Module_Type'Class( Module ),
				Request	=> Request,
				Ids	=> Ids,
				Output	=> Output
			);


		if Module.Enable_List_Navigation_Bar( Bottom ) then
			Render_List_Navigation_Bar(
						Module		=> Entity_Module_Type'Class( Module ),
						Request		=> Request,
						From		=> From,
						Limit		=> Limit,
						Total_Shown	=> Ids'Length,
						Output		=> Output
					);
		end if;

	end Render_List;

	
	function Get_List_From(
				Module	: in Entity_Module_Type;
				Request	: in AWS.Status.Data
			) return Positive is
		P	: AWS.Parameters.List := AWS.Status.Parameters( Request );
		F : constant String := AWS.Parameters.Get( P, "from" );
	begin
		if F = "" then
			return 1;
		else
			return Positive'Value( F );
		end if;
	end Get_List_From;

	function Get_List_Limit(
				Module	: in Entity_Module_Type;
				Request	: in AWS.Status.Data
			) return Natural is
		P	: AWS.Parameters.List := AWS.Status.Parameters( Request );
		L : constant String := AWS.Parameters.Get( P, "limit" );
	begin
		if L = "" then
			return 20;
		else
			return Natural'Value( L );
		end if;
	end Get_List_Limit;




	procedure Render_List_Title(
				Module		: in out Entity_Module_Type;
				Request		: in     AWS.Status.Data;
				Output		:    out Unbounded_String
			) is
	begin
		Append( Output, "<h1>" );
		Append( Output, Module.List_Label );
		Append( Output, "</h1>" );
	end Render_List_Title;




	procedure Render_List_Navigation_Bar(
				Module		: in out Entity_Module_Type;
				Request		: in     AWS.Status.Data;
				From		: in     Positive;
				Limit		: in     Natural;
				Total_Shown	: in     Natural;
				Output		:    out Unbounded_String
			) is


		procedure Append_Paging(
				Label	: in String;
				From	: in Integer;
				Enabled	: in Boolean
			) is
			function T( i : in integer ) return String is
			begin
				return Ada.Strings.Fixed.Trim( Integer'Image( I ), Ada.Strings.Both );
			end T;
		begin
			Append_Link_Button(
					Output	=> Output,
					Label	=> Label,
					Href	=> KOW_View.URI_Util.Build_URL( Request => Request, Key1 => "from", Value1 => T(From), Key2 => "limit", Value2 => T(Limit) ),
					Enabled	=> Enabled
				);
		end Append_Paging;


		function Previous_From return Integer is
		begin
			if From > Limit then
				return From - Limit;
			else
				return 1;
			end if;
		end Previous_From;

		function Has_Previous return Boolean is
		begin
			return From > 1;
		end Has_Previous;

		function Next_From return Integer is
		begin
			return From + Limit;
		end Next_From;

		function Has_Next return Boolean is
		begin
			return Total_Shown = Limit;
		end Has_Next;

	begin
		Toolbar_Open(
				Output	=> Output,
				Module	=> Module,
				Mode	=> Navigation_Toolbar
			);
		Append_Paging( "&lt;", Previous_From, Has_Previous );
		if Can_Create( Entity_Module_Type'Class( Module ), Request ) then
			Append_Link_Button( Output, Module.New_Label, KOW_View.URI_Util.Build_URL( Request, "style", "big_edit" ) );
		end if;
		Append_Paging( "&gt;", Next_From, Has_Next );
		Toolbar_Close(	Output	=> Output );
	end Render_List_Navigation_Bar;



	procedure Render_List_Body(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Ids	: in     KOW_Ent.Id_Array_Type;
				Output	:    out Unbounded_String
			) is
	begin
		Append( Output, "<ul>" );

		for i in Ids'range loop
			declare
				Entity	: KOW_Ent.Entity_Type'Class := Load_Entity( Module, Ids( i ) );
			begin
				Render_List_Body_Item(
						Module	=> Entity_Module_Type'Class( Module ),
						Request	=> Request,
						Entity	=> Entity,
						Output	=> Output
					);
			end;
		end loop;
		Append( Output, "</ul>" );
	end Render_List_Body;


	procedure Render_List_Body_Item(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			) is
		Buffer	: Unbounded_String;
		URL	: constant String := KOW_View.URI_Util.Build_URL( Request => Request, Key1 => "style", Value1 => "big", key2 => "entity_id", Value2 => KOW_ent.To_String( Entity.ID )  );
	begin
		Append( Output, "<li onClick=""window.location.href='" & URL & "'"">" );
		Append( Output, "<a href=""" );
			Append( Output, URL );
			Append( Output, """>" );
			Render_View(
					Module	=> Entity_Module_Type'Class( Module ),
					Request	=> Request,
					Entity	=> Entity,
					Output	=> Buffer
				);
			Append( Output, Buffer );
		Append( Output, "</a>" );
		Append( Output, "</li>" );
	end Render_List_Body_Item;





	procedure Render_View(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			) is

	begin
		Render_View_Title(
					Module	=> Entity_Module_Type'Class( Module ),
					Request	=> Request,
					Entity	=> Entity,
					Output	=> Output
				);

		Render_View_Form_Open(
				Module	=> Entity_Module_Type'Class( Module ),
				Request	=> Request,
				Entity	=> Entity,
				Output	=> Output
			);


		Render_View_Properties(
				Module	=> Entity_Module_Type'Class( Module ),
				Request	=> Request,
				Entity	=> Entity,
				Output	=> Output
			);

		Render_View_Buttons(
				Module	=> Entity_Module_Type'Class( Module ),
				Request	=> Request,
				Entity	=> Entity,
				Output	=> Output
			);

		Render_View_Form_Close(
				Module	=> Entity_Module_Type'Class( Module ),
				Request	=> Request,
				Entity	=> Entity,
				Output	=> Output
			);
	end Render_View;



	procedure Render_View_Title(
					Module	: in out Entity_Module_Type;
					Request	: in     AWS.Status.Data;
					Entity	: in     KOW_ENt.Entity_Type'Class;
					Output	:    out Unbounded_String
				) is
	begin
		if Module.Style = Big_Edit_Rendering or else Module.Style = Big_Rendering then
			Append( Output, "<h1>" );
			declare
				Label : constant Unbounded_String := KOW_Ent.Get_Label( Entity, KOW_View.Locales.Get_Locale( Request ) );
			begin
				Append( Output, Label );
			end;
			Append( Output, "</h1>" );
		end if;
	end Render_View_Title;


	procedure Render_View_Form_Open(
					Module	: in out Entity_Module_Type;
					Request	: in     AWS.Status.Data;
					Entity	: in     KOW_Ent.Entity_Type'Class;
					Output	:    out Unbounded_String
				) is
	begin
		if Is_Edit( Entity_Module_Type'Class( Module ) ) then
			Append( Output, "<form method=""POST"" enctype=""multipart/form-data"" id=""entity_form_" );
			Append( Output, Ada.Strings.Fixed.Trim( Integer'Image( Module.ID ),Ada.Strings.Both ) );
			Append( Output, """>" );
		end if;
	end Render_View_Form_Open;



	procedure Render_View_Properties(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			) is
		procedure Iterator( C : in KOW_Ent.Property_Lists.Cursor ) is
			use KOW_View.Entities.Property_Renderers;

			Property	: KOW_Ent.Entity_Property_Ptr := KOW_Ent.Property_Lists.Element( C );
			Renderer	: Property_Renderer_Ptr := Property_Renderer_Metadata.Get( Property.all );
			Renderer_Buffer : Unbounded_String;
		begin

			KOW_View.Entities.Property_Renderers.Render_Property(
							Renderer	=> Renderer.all,
							Module		=> Module,
							Request		=> Request,
							Entity		=> Entity,
			                                Property	=> Property.all,
							Style		=> Module.Style,
							Output		=> Renderer_Buffer
						);

			Append( Output, "<label class=""" );
			Append( Output, Property.Column_Name );
			Append( Output, """>" );
			Append( Output, Renderer_Buffer );
			Append( Output, "</label>" );
		end Iterator;

	begin
		KOW_Ent.Property_Lists.Iterate(
					Get_Properties( Entity_Module_Type'Class( Module ), Entity ),
					Iterator'Access
				);
	end Render_View_Properties;

	
	procedure Render_View_Buttons(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			) is
	begin

		if KOW_Ent.Is_New( Entity ) then
			if not Can_Create( Entity_Module_Type'Class( Module ), Request ) then
				return;
			end if;
		else
			if not Can_Edit( Entity_Module_Type'Class( Module ), Request, Entity ) then
				-- render no button if can't edit
				return;
			end if;
		end if;

		case Module.Style is
			when Small_Rendering | Small_Edit_Rendering =>
				null;
			when Big_Rendering =>
				Toolbar_Open(
						Output	=> Output,
						Module	=> Module,
						Mode	=> Form_Toolbar
					);
				Append_Link_Button(
						Output	=> Output,
						Label	=> Module.Edit_Label,
						Href	=> KOW_View.URI_Util.Build_URL(
											Request	=> Request,
											Key1	=> "entity_id",
											Value1	=> KOW_Ent.To_String( Entity.ID ),
											Key2	=> "style",
											Value2	=> "big_edit"
										)
								);
				Toolbar_Close( Output => Output );

			when Big_Edit_Rendering =>
				Toolbar_Open(
						Output	=> Output,
						Module	=> Module,
						Mode	=> Form_Toolbar
					);

				Append_Button(
						Output	=> Output,
						Label	=> Module.Submit_Label,
						onClick	=> "kowview.entities.submitForm(" & Integer'Image( Module.ID ) & ")"
					);
				Toolbar_Close( Output => Output );
		end case;
	end Render_View_Buttons;



	procedure Render_View_Form_Close(
					Module	: in out Entity_Module_Type;
					Request	: in     AWS.Status.Data;
					Entity	: in     KOW_Ent.Entity_Type'Class;
					Output	:    out Unbounded_String
				) is
	begin
		if Is_Edit( Entity_Module_Type'Class( Module ) ) then
			Append( Output, "</form>" );
		end if;
	end Render_View_Form_Close;



	--------------------
	-- Helper Methods --
	--------------------

	function Is_Edit( Module : in Entity_Module_Type ) return Boolean is
	begin
		return Module.Style = Small_Edit_Rendering or else Module.Style = Big_Edit_Rendering;
	end Is_Edit;

	procedure Check_Tag( Module : in KOW_View.Modules.Module_Type'Class ) is
		-- check if the module is in Entity_Module_Type'Class 
		-- if not, raise constraint error
	begin
		if Module not in Entity_Module_Type'Class then
			raise CONSTRAINT_ERROR with "subtype of Entity_Module_Type expected";
		end if;
	end Check_Tag;

end KOW_View.Entities.Modules;
