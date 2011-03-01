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
with KOW_View.Entities.Components;
with KOW_View.Entities.Property_Renderers;
with KOW_View.Entities.Validation;
with KOW_View.Locales;
with KOW_View.Modules;
with KOW_View.Modules.Stateless_Module_Factories;

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
		-- setup the entity_tag and narrow variables...
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

		Module.Submit_Label := E( "submit_label", "Submit" );
		Module.List_Label := E( "list_label", "List" );

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

		Append( Output, "<span class=""" & KOW_View.Modules.Get_Name( Entity_Module_Type'Class( Module ) ) & """>" );
		if Entity_Id = -1 and then ( Module.Style = Small_Rendering or Module.Style = Big_Rendering ) then
			declare
				P	: AWS.Parameters.List := AWS.Status.Parameters( Request );
				function From return Positive is
					F : constant String := AWS.Parameters.Get( P, "from" );
				begin
					if F = "" then
						return 1;
					else
						return Positive'Value( F );
					end if;
				end From;

				function Limit return Natural is
					L : constant String := AWS.Parameters.Get( P, "limit" );
				begin
					if L = "" then
						return 20;
					else
						return Natural'Value( L );
					end if;
				end Limit;
				Ids : KOW_Ent.Id_Array_Type := Query_Entities(
										Module	=> Entity_Module_Type'Class( Module ),
										Request	=> Request,
										From	=> From,
										Limit	=> Limit
									);

				procedure Append_Button(
						Label	: in String;
						Href	: in String;
						Enabled	: in Boolean := True
					) is
				begin
					Append( Output, "<button onClick=""window.location.href='"&href&"'"" dojoType=""dijit.form.Button""");
					if not Enabled then
						Append( Output, " disabled=true" );
					end if;
					Append( Output, ">"&label&"</button>" );
				end Append_Button;

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
					Append_Button(
							Label	=> Label,
							Href	=> "?from=" & T(From) & "&limit=" & T(Limit),
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
					return Ids'Length = Limit;
				end Has_Next;
			begin
				Append( Output, "<h1>" );
				Append( Output, Module.List_Label );
				Append( Output, "</h1>" );

				Include_Dojo_Package( Module, "dijit.form.Button" );
				Append( Output, "<div class=""entityToolbar"">" );
					Append_Paging( "&lt;", Previous_From, Has_Previous );
					Append_Button( "new", "?style=big_edit", True );
					Append_Paging( "&gt;", Next_From, Has_Next );
				Append( Output, "</div>" );
				Append( Output, "<ul>" );

				for i in Ids'range loop
					declare
						Buffer : Unbounded_String;
						Entity	: KOW_Ent.Entity_Type'Class := Load_Entity( Module, Ids( i ) );
					begin
						Append( Output, "<li>" );
						Append( Output, "<a href=""?style=big&entity_id=" & KOW_ent.To_String( Entity.ID ) & """>" );
						Render_View(
								Module	=> Module,
								Request	=> Request,
								Entity	=> Entity,
								Style	=> Module.Style,
								Output	=> Buffer
							);
						Append( Output, Buffer );
						Append( Output, "</a>" );
						Append( Output, "</li>" );
					end;
				end loop;
				Append( Output, "</ul>" );
				Append( Output, "<div class=""entityToolbar"">" );
					Append_Paging( "&lt;", Previous_From, Has_Previous );
					Append_Button( "new", "?style=big_edit", True );
					Append_Paging( "&gt;", Next_From, Has_Next );
				Append( Output, "</div>" );
			end;
		else
			declare
				Entity	: KOW_Ent.Entity_Type'Class := Load_Entity( Module, Entity_ID );
				H1	: constant String := "<h1>" & KOW_Ent.Get_Label( Entity, KOW_View.Locales.Get_Locale( Request ) ) & "</h1>";
				Buffer	: Unbounded_String;
			begin
				Render_View(
						Module	=> Module,
						Request	=> Request,
						Entity	=> Entity,
						Style	=> Module.Style,
						Output	=> Buffer
					);
				Append( Output, h1 );
				Append( Output, Buffer );
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

		KOW_Lib.Json.Set( Resp, "entity_id", KOW_Ent.To_String( Entity.ID ) );
		
		Response := Resp;
	end Process_Json_Request;
	




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


	procedure Render_View(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Style	: in     KOW_View.Entities.Rendering_Style_Type;
				Output	:    out Unbounded_String
			) is
		Buffer : Unbounded_String;

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
							Style		=> Style,
							Output		=> Renderer_Buffer
						);

			Append( Buffer, "<label>" );
			Append( Buffer, Renderer_Buffer );
			Append( Buffer, "</label>" );
		end Iterator;

	begin

		Buffer := To_Unbounded_String( "<fieldset><form enctype=""multipart/form-data"" id=""entity_form_" );
		Append( Buffer, Ada.Strings.Fixed.Trim( Integer'Image( Module.ID ),Ada.Strings.Both ) );
		Append( Buffer, """>" );

		KOW_Ent.Property_Lists.Iterate(
					Get_Properties( Entity_Module_Type'Class( Module ), Entity ),
					Iterator'Access
				);

		if Style = Small_Edit_Rendering or else Style = Big_Edit_Rendering then
			Include_Dojo_Package( Module, "dijit.form.Button" );
			Append( Buffer, "<button onclick=""kowview.entities.submitForm(" );
			Append( Buffer, Integer'Image( Module.ID ) );
			Append( Buffer, ")"" dojoType=""dijit.form.Button"">" );
			Append( Buffer, Module.Submit_Label );
			Append( Buffer, "</button>" );
		end if;
		Append( Buffer, "</form></fieldset>" );

		Output := Buffer;
	end Render_View;


	procedure Set_Values(
				Module	: in out Entity_Module_Type;
				Entity	: in out KOW_Ent.Entity_Type'Class;
				Request	: in     AWS.Status.Data
			) is
		Properties : KOW_Ent.Property_Lists.List := Get_Properties( Entity_Module_Type'Class( Module ), Entity );

		Params	: AWS.Parameters.List := AWS.Status.Parameters( Request );

		procedure Iterator( C : in KOW_Ent.Property_Lists.Cursor ) is
			Property : KOW_Ent.Entity_Property_Ptr := KOW_Ent.Property_Lists.Element( C );
		begin
			KOW_Ent.Set_Property(
					Property	=> Property.all,
					Entity		=> Entity,
					Value		=> AWS.Parameters.Get( Params, To_String( Property.all.Column_Name ) )
				);
		end Iterator;
	begin
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
	end Set_Values;



	--------------------
	-- Helper Methods --
	--------------------

	procedure Check_Tag( Module : in KOW_View.Modules.Module_Type'Class ) is
		-- check if the module is in Entity_Module_Type'Class 
		-- if not, raise constraint error
	begin
		if Module not in Entity_Module_Type'Class then
			raise CONSTRAINT_ERROR with "subtype of Entity_Module_Type expected";
		end if;
	end Check_Tag;

end KOW_View.Entities.Modules;
