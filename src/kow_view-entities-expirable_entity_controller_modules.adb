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



------------------------------------------------------------------------------
--                                                                          --
--  This package provides a module implementation in the spirit of the enti---
-- ty module type for handling expirable entity controllers                 --
--                                                                          --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.ID_Query_Builders;
with KOW_Sec;
with KOW_View.Entities.Components;
with KOW_View.Entities.Modules;
with KOW_View.URI_Util;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;



package body KOW_View.Entities.Expirable_Entity_Controller_Modules is


	---------------------
	-- The Module Type --
	---------------------


	Entity_Tag : constant Unbounded_String := To_Unbounded_String( Ada.Tags.Expanded_Name( Entity_Type'Tag ) );
	overriding
	procedure Initialize_Request(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			) is
		-- call entity_module_type's setup
		-- Entity_Tag is always set to Entity_Type'Tag;

		P : AWS.Parameters.List := AWS.Status.Parameters( Request );
	begin

		KOW_View.Entities.Modules.Initialize_Request(
					Module	=> KOW_View.Entities.Modules.Entity_Module_Type( Module ),
					Request	=> Request,
					Config	=> Config
				);



		Module.Valid_Label := KOW_Config.Value( Config, "valid_label", Default_Valid_Label );
		Module.All_Label := KOW_Config.Value( Config, "all_label", Default_All_Label );

		Module.Entity_Tag := Entity_Tag;
		Module.Style := Small_Rendering;

		Module.View_Entity := Get_View_Entity(
						Module	=> Lifetime_Handler_Module_Type'Class( Module ),
						P	=> P
					);


	end Initialize_Request;

	overriding
	procedure Process_Body(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			) is
		-- always render list
	begin
		Initialize_Dojo_Includes(
					Module	=> Lifetime_Handler_Module_Type'Class( Module ),
					Request	=> Request
				);


		Include_Component_Script(
					Module		=> Module,
					Component	=> KOW_View.Entities.Components.Component,
					Script		=> "kowview-entities.js"
				);

		Include_Component_Script(
					Module		=> Module,
					Component	=> KOW_View.Entities.Components.Component,
					Script		=> "kowview-entities-expirable_entity_controllers.js"
				);

		Module.Style := Small_Rendering;
		Render_List(
				Module	=> Lifetime_Handler_Module_Type'Class( Module ),
				Request	=> Request,
				Output	=> Output
			);
	end Process_Body;



	overriding
	procedure Process_Json_Request(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			) is
		-- act upon actions from Lifetime_Action_Type
		Obj : KOW_Lib.Json.Object_Type;

		function Get_Entity return Entity_Type is
			Entity : Entity_Type;
		begin
			KOW_Ent.Load( Entity, Natural( Get_Entity_Id( Lifetime_Handler_Module_Type'Class( Module ), Request ) ) );
			if not Can_Edit( Lifetime_Handler_Module_Type'Class( Module ), Request, Entity ) then
				raise KOW_Sec.Access_Denied with "Sorry, but you can't do that";
			end if;
			return Entity;
		end Get_Entity;
	begin
		Module.Lifetime_Action := Get_Lifetime_Action(
						Module	=> Lifetime_Handler_Module_Type'Class( Module ),
						P	=> AWS.Status.Parameters( Request )
					);

		case Module.Lifetime_Action is
			when Expire_Entity =>
				Controllers.Expire( Get_Entity );
			when Validate_Entity =>
				Controllers.Validate( Get_Entity );
			when Store_Validation_Period =>
				declare
					Entity			: Entity_Type := Get_Entity; -- just to check if there is really an entity in here..
					Validation_Entity	: Controllers.Validation_Entity'Class := 
									Get_Validation_Entity(
											Module	=> Lifetime_Handler_Module_Type'Class( Module ),
											Request	=> Request
										);
				begin
					Set_Values(
							Module		=> Lifetime_Handler_Module_Type'Class( Module ),
							Entity		=> Validation_Entity,
							Request		=> Request
						);
					KOW_Ent.Store( Validation_Entity );
				end;
			when Render_Form =>
				declare
					Buffer : Unbounded_String;
				begin
					Module.Style := Big_Edit_Rendering;

					Render_View(
							Module	=> Lifetime_Handler_Module_Type'Class( Module ),
							Request	=> Request,
							Entity	=> Get_Validation_Entity(
											Module	=> Lifetime_Handler_Module_Type'Class( Module ),
											Request	=> Request
										),
							Output	=> Buffer
						);
					KOW_Lib.Json.Set( Obj, "formHTML", Buffer );
				end;
		end case;


		

		KOW_Lib.Json.Set( Obj, "action", Lifetime_Action_Type'Image( Module.Lifetime_Action ) );
		Response := Obj;
	end Process_Json_Request;

	---------------------
	-- Entity Handling --
	---------------------
	overriding
	function New_Entity(
				Module	: in     Lifetime_Handler_Module_Type
			) return KOW_Ent.Entity_Type'Class is
		E : Entity_Type;
	begin
		return E;
	end New_Entity;


	-----------
	-- Query --
	-----------

	overriding
	function Query_Entities(
				Module	: in Lifetime_Handler_Module_Type;
				Request	: in AWS.Status.Data;
				From	: in Positive;
				Limit	: in Natural
			) return KOW_Ent.Id_Array_Type is
		-- run the query based on the "view_entities" property (View_Entity_Type type)
	begin
		case Module.View_Entity is
			when Valid_Entities =>
				return Controllers.Query_Valid( From => From, Limit => Limit );
			when All_Entities =>
				declare
					use KOW_Ent.Id_Query_Builders;
					Q : Query_Type;
				begin
					Prepare( Q, Entity_Type'Tag );

					return Get_Some(
							Q	=> Q,
							From	=> From,
							Limit	=> Limit
						);
				end;
		end case;
	end Query_Entities;




	---------------
	-- Rendering --
	---------------
	overriding
	procedure Render_View_Buttons(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			) is
		-- render the correct buttons for the form
	begin

		if Entity in Entity_Type'Class then
			-- it's the main view... ignore
			null;
		elsif Entity in Controllers.Validation_Entity'Class then
			-- yeah, we have some buttons to render ;)

			KOW_View.Entities.Modules.Render_View_Buttons(
						Module	=> KOW_View.Entities.Modules.Entity_Module_Type( Module ),
						Request	=> Request,
						Entity	=> Entity,
						Output	=> Output
					);
		end if;
	end Render_View_Buttons;


	overriding
	procedure Render_List_Title(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			) is


		procedure Begin_Options is
		begin
			Include_Dojo_Package( Module, "dijit.form.FilteringSelect" );
			Append( Output, "<form action=""#"" class=""validationOptions"">" );
			Append( Output, "<select name="""& View_Entity_Key & """ dojoType=""dijit.form.FilteringSelect"" onChange=""kowview.entities.expirable_entity_controllers.updateList(this)"">" );
		end Begin_Options;

		procedure Append_Option( Label : in Unbounded_String; Value : in View_Entity_Type ) is
			function selected return string is
			begin
				if Module.View_Entity = Value then
					return " selected=""selected""";
				else
					return "";
				end if;
			end selected;
		begin
			Append( Output, "<option value=""" & View_Entity_Type'Image( Value ) & '"' & selected & '>' );
			Append( Output, Label );
			Append( Output, "</option>" );
		end Append_Option;

		procedure End_Options is
		begin
			Append( Output, "</select>" );
			Append( Output, "</form>" );
		end End_Options;
	begin

		Append( Output, "<div class=""validation"">" );
			KOW_View.Entities.Modules.Render_List_Title(
						Module	=> KOW_View.Entities.Modules.Entity_Module_Type( Module ),
						Request	=> Request,
						Output	=> Output
					);
			Begin_Options;
				Append_Option( Module.Valid_Label, Valid_Entities );
				Append_Option( Module.All_Label, All_Entities );
			End_Options;
		Append( Output, "</div>" );
	end Render_List_Title;

	overriding
	procedure Render_List_Body_Item(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			) is
		-- render the list item and call Render_List_Body_Item_Initializer()
		Buffer	: Unbounded_String;
		URL	: constant String := KOW_View.URI_Util.Build_URL( Request => Request, Key1 => "style", Value1 => "big", key2 => "entity_id", Value2 => KOW_ent.To_String( Entity.ID )  );
		Id	: Unbounded_String;
	begin
		Generate_HTML_Id( Module, ID );
		Append( Output, "<li onClick=""window.location.href='" & URL & "'"" id=""" );
		Append( Output, ID );
		Append( Output, """>" );
		Append( Output, "<a href=""" );
			Append( Output, URL );
			Append( Output, """>" );
			Render_View(
					Module	=> Lifetime_Handler_Module_Type'Class( Module ),
					Request	=> Request,
					Entity	=> Entity,
					Output	=> Buffer
				);
			Append( Output, Buffer );
		Append( Output, "</a>" );

		 Render_List_Body_Item_Initializer(
				Module	=> Lifetime_Handler_Module_Type'Class( Module ),
				Request	=> Request,
				Entity	=> Entity,
				Li_ID	=> To_String( ID ),
				Output	=> Output
			);
		Append( Output, "</li>" );

	end Render_List_Body_Item;

	-----------------
	-- New Methods --
	-----------------

	procedure Initialize_Dojo_Includes(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data
			) is
		-- render every (empty) validation entity using big_edit_rendering
		-- so the renderers can include the needed dojo packages beforehand.
		Extensions	: Tag_Array := Get_Validation_Extensions(
							Module	=> Lifetime_Handler_Module_Type'Class( Module ),
							Request	=> Request
						);

		procedure Initialize( The_Tag : in Ada.Tags.Tag ) is
			Buffer : Unbounded_String;
			-- a buffer that we trash when the processing is finished
		begin
			Render_View(
					Module	=> Lifetime_Handler_Module_Type'Class( Module ),
					Request	=> Request,
					Entity	=> KOW_Ent.New_Entity( The_Tag ),
					Output	=> Buffer
				);

		exception
			when others => null;
			-- ignore all exceptions at this stage...
		end Initialize;
	begin

		Initialize( Entity_Type'Tag );
		for i in Extensions'Range loop
			Initialize( Extensions( i ) );
		end loop;
	end Initialize_Dojo_Includes;


	function Get_Validation_Extensions(
				Module	: in Lifetime_Handler_Module_Type;
				Request	: in AWS.Status.Data
			) return Tag_Array is
		-- override this method in case you want to enable your user to
		-- create
		--
		-- default: return an empty array
		Empty : Tag_Array( 2 .. 1 );
	begin
		return Empty;
	end Get_Validation_Extensions;


	function Get_Validation_Entity(
				Module	: in Lifetime_Handler_Module_Type;
				Request	: in AWS.Status.Data
			) return Controllers.Validation_Entity'Class is
		-- if entity_id is set, get the validation entity for this entity
		-- or else, allocate a new entity by the validation_entity_tag parameter
		Entity_ID	: constant Integer := Get_Entity_Id( Lifetime_Handler_Module_Type'Class( Module ), Request );
		The_Tag 	: constant String := AWS.Parameters.Get( AWS.Status.Parameters( Request ), "validation_entity_tag" );
	begin
		if Entity_Id /= -1 then
			declare
				Entity : Entity_Type;
			begin
				KOW_Ent.Load( Entity, Natural( Entity_Id ) );
				return Controllers.Get_Validation( Entity );
			end;
		elsif The_Tag /= "" then
			declare
				Available : Tag_Array := Get_Validation_Extensions( Lifetime_Handler_Module_Type'Class( Module ), Request );
			begin
				for i in Available'Range loop
					if The_Tag = Ada.Tags.Expanded_Name( Available( i ) ) then
						return Controllers.Validation_Entity'Class( KOW_Ent.New_Entity( To_Unbounded_String( The_Tag ) ) );
					end if;
				end loop;

				raise KOW_Sec.Access_Denied with "Sorry, but you don't have permission to create this entity";
			end;
		else
			declare
				The_Entity : Controllers.Validation_Entity;
			begin
				return The_Entity;
			end;
		end if;

	end Get_Validation_Entity;

	procedure Render_List_Body_Item_Initializer(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Li_ID	: in     String;
				Output	:    out Unbounded_String
			) is
		-- render the javascript initialization method

		function Is_Valid return Boolean is
		begin
			case Module.View_Entity is
				when Valid_Entities =>
					return True;
				when All_Entities =>
					return Controllers.Is_Valid( Entity_Type( Entity ) );
			end case;
		end Is_Valid;

		function Is_Valid return String is
		begin
			if Is_Valid then
				return "true";
			else
				return "false";
			end if;
		end Is_Valid;
	begin
		Append( Output, "<script type=""text/javascript"">" );
			Append( Output, "kowview.entities.expirable_entity_controllers.initializeItem(" );
				Append( Output, Integer'Image( Get_ID ( Module ) ) & ',' );
				Append( Output, '"' & Li_ID & """," );
				Append( Output, Is_Valid );
			Append( Output, ");" );
		Append( Output, "</script>" );
	end Render_list_Body_Item_Initializer;


	-----------------------
	-- Parameter Getting --
	-----------------------
	
	function Get_View_Entity(
				Module	: in Lifetime_Handler_Module_Type;
				P	: in AWS.Parameters.List
			) return View_Entity_Type is
		View_Entity : constant String := AWS.Parameters.Get( P, View_Entity_Key );
	begin
		if View_Entity = "" then
			return Valid_Entities;
		else
			return View_Entity_Type'Value( View_Entity );
		end if;
	exception
		when CONSTRAINT_ERROR =>
			raise PROGRAM_ERROR with "Invalid value for view mode: " & View_Entity;
	end Get_View_Entity;
	

	function Get_Lifetime_Action(
				Module	: in Lifetime_Handler_Module_Type;
				P	: in AWS.Parameters.List
			) return Lifetime_Action_Type is
		Act : constant String := AWS.Parameters.Get( P, Lifetime_Action_Key );
	begin
		if Act = "" then
			raise PROGRAM_ERROR with "Seems like the developer forgot to pass the action parameter...";
		else
			return Lifetime_Action_Type'Value( Act );
		end if;
	exception
		when constraint_error =>
			raise PROGRAM_ERROR with "Invalid value for action: " & Act;
	end Get_Lifetime_Action;




end KOW_View.Entities.Expirable_Entity_Controller_Modules;
