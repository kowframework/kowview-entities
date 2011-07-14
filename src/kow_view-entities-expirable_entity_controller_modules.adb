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
with KOW_Sec;
with KOW_View.Entities.Modules;


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
	begin
		KOW_View.Entities.Modules.Initialize_Request(
					Module	=> KOW_View.Entities.Modules.Entity_Module_Type( Module ),
					Request	=> Request,
					Config	=> Config
				);

		Module.Entity_Tag := Entity_Tag;
		Module.Style := Small_Rendering;
	end Initialize_Request;

	overriding
	procedure Process_Body(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			) is
		-- always render list
	begin
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
		P   : AWS.Parameters.List := AWS.Status.Parameters( Request );


		function Action return Lifetime_Action_Type is
			Act : constant String := AWS.Parameters.Get( P, "action" );
		begin
			if Act = "" then
				raise PROGRAM_ERROR with "Seems like the developer forgot to pass the action parameter...";
			else
				return Lifetime_Action_Type'Value( Act );
			end if;
		exception
			when constraint_error =>
				raise PROGRAM_ERROR with "Invalid value for action: " & Act;
		end Action;


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
		case Action is
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


		

		KOW_Lib.Json.Set( Obj, "action", Lifetime_Action_Type'Image( Action ) );
		Response := Obj;
	end Process_Json_Request;


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
		Output := To_Unbounded_String( "BOTOES AQUI PLZ" );
		-- NOTE :: don't forget to put the entity_id in the button function parameter!
	end Render_View_Buttons;

	-----------------
	-- New Methods --
	-----------------

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
		elsif The_Tag /= "" and The_Tag /= Ada.Tags.Expanded_Name( Entity_Type'Tag ) then
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
				The_Entity : COntrollers.Validation_Entity;
			begin
				return The_Entity;
			end;
		end if;

	end Get_Validation_Entity;

end KOW_View.Entities.Expirable_Entity_Controller_Modules;
