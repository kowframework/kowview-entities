------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
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
--                                                                          --
--  Throughout this pacakge you'll see two common names apearing all the    --
-- time:                                                                    --
--      * Entity_Type                                                       --
--      * Validation_Entity_Type                                            --
--                                                                          --
--  I believe it's quite clear the difference and I'm not goint through this--
-- now. But remember this when editing this file.                           --
--                                                                          --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Expirable_Entity_Controllers;
with KOW_View.Entities.Modules;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;


generic
	type Entity_Type is new KOW_Ent.Entity_Type with private;
	Table_Name : String;
	with package Controllers is new KOW_Ent.Expirable_Entity_Controllers( Entity_Type, Table_Name );
package KOW_View.Entities.Expirable_Entity_Controller_Modules is


	------------------------------------
	-- HTTP Request Control Variables --
	------------------------------------

	type Lifetime_Action_Type is (
	-- the lifetime action type is used by the json callback to identify
	-- what to do.
			Expire_Entity,			-- expirate the entity _NOW_
			Validate_Entity,		-- validate the entity _NOW_
			Store_Validation_Period,	-- store (insert a new or update an existing) a validation period
			Render_Form			-- render the entity form; if the validation_id is suplied, load the entity before
		);

	Lifetime_Action_Key : constant String := "action";
	
	
	type View_entity_Type is(
			Valid_Entities,
			All_Entities
		);

	View_Entity_Key : constant String := "view_entity";


	--------------------
	-- Default Labels --
	--------------------
	Default_Valid_LAbel	: Unbounded_String := To_Unbounded_String( "Valid Entities" );
	Default_All_Label	: Unbounded_String := To_Unbounded_String( "All Entities" );

	------------------------
	-- Other helper types --
	------------------------

	type Tag_Array is array( Positive range <> ) of Ada.Tags.Tag;

	---------------------
	-- The Module Type --
	---------------------


	type Lifetime_Handler_Module_Type is abstract new KOW_View.Entities.Modules.Entity_Module_Type with record
		-- this module will render a list of entities, giving the ability to expire/validate and revalidate entities

		-- The following attributes are updated in every request
		Lifetime_Action	: Lifetime_Action_Type;
		View_Entity	: View_Entity_Type;



		Valid_Label	: Unbounded_String;
		-- label for "valid entities"
		All_Label	: Unbounded_String;
		-- label for "all entities"
	end record;


	overriding
	procedure Initialize_Request(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			);
	-- call Entity_Module_Type's setup
	-- Entity_Tag is always set to Entity_Type'Tag;


	overriding
	procedure Process_Body(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			);
	-- always render list
	
	overriding
	procedure Process_Json_Request(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			);
	-- act upon actions from Lifetime_Action_Type


	-----------
	-- Query --
	-----------

	overriding
	function Query_Entities(
				Module	: in Lifetime_Handler_Module_Type;
				Request	: in AWS.Status.Data;
				From	: in Positive;
				Limit	: in Natural
			) return KOW_Ent.Id_Array_Type;
	-- run the query based on the "view_entities" property (View_Entity_Type type)



	---------------
	-- Rendering --
	---------------
	overriding
	procedure Render_View_Buttons(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			);
	-- render the correct buttons for the form

	overriding
	procedure Render_List_Title(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			);

	overriding
	procedure Render_List_Body_Item(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			);
	-- render the list item and call Render_List_Body_Item_Initializer()

	-----------------
	-- New Methods --
	-----------------


	procedure Initialize_Dojo_Includes(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data
			);
	-- render every (empty) validation entity using big_edit_rendering
	-- so the renderers can include the needed dojo packages beforehand.

	function Get_Validation_Extensions(
				Module	: in Lifetime_Handler_Module_Type;
				Request	: in AWS.Status.Data
			) return Tag_Array;
	-- override this method in case you want to enable your user to
	-- create
	--
	-- default: return an empty array

	function Get_Validation_Entity(
				Module	: in Lifetime_Handler_Module_Type;
				Request	: in AWS.Status.Data
			) return Controllers.Validation_Entity'Class;
	-- if entity_id is set, get the validation entity for this entity
	-- or else, allocate a new entity by the validation_entity_tag parameter


	procedure Render_List_Body_Item_Initializer(
				Module	: in out Lifetime_Handler_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Li_ID	: in     String;
				Output	:    out Unbounded_String
			);
	-- render the javascript initialization method


	-----------------------
	-- Parameter Getting --
	-----------------------
	
	function Get_View_Entity(
				Module	: in Lifetime_Handler_Module_Type;
				P	: in AWS.Parameters.List
			) return View_Entity_Type;
	

	function Get_Lifetime_Action(
				Module	: in Lifetime_Handler_Module_Type;
				P	: in AWS.Parameters.List
			) return Lifetime_Action_Type;


end KOW_View.Entities.Expirable_Entity_Controller_Modules;
