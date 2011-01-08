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
-- Modules for the Entities application for KOW View                        --
------------------------------------------------------------------------------



-------------------
-- KOW Framework --
-------------------
with KOW_View.Entities.Components;
with KOW_View.Modules;
with KOW_View.Modules.Stateless_Module_Factories;



package KOW_View.Entities.Modules is

	---------------
	-- Constants --
	---------------

	Default_View_Entity_Template_Name	: constant Unbounded_String := To_Unbounded_String( "default_templates" & KOW_Lib.File_System.Separator & "view_entity" );
	Default_Edit_Entity_Template_Name	: constant Unbounded_String := To_Unbounded_String( "default_templates" & KOW_Lib.File_System.Separator & "edit_entity" );
	Default_Create_Entity_Template_Name	: constant Unbounded_String := To_Unbounded_String( "default_templates" & KOW_Lib.File_System.Separator & "create_entity" );
	Default_List_Entities_Template_Name	: constant Unbounded_String := To_Unbounded_String( "default_templates" & KOW_Lib.File_System.Separator & "list_entities" );




	-----------------
	-- View Entity --
	-----------------


	type View_Entity_Module is new KOW_View.Modules.Module_Type with record
		-- a type for rendering the data from entities in a determined form area
		Id			: KOW_Ent.ID_Type;
		Entity_Tag		: Unbounded_String;
		Template_Name		: Unbounded_String;
		Inlined_Entity_Tags	: KOW_Lib.UString_Vectors.Vector;
		Narrow			: Boolean;
		Ignore			: KOW_Lib.UString_Vectors.Vector;
	end record;


	procedure Process_Body(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Response	:    out Unbounded_String
		);
	-- draws the main template for this entity


	package View_Entity_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
							Module_Type	=> View_Entity_Module,
							Component	=> KOW_View.Entities.Components.Component'Access
					);

	-----------------
	-- Edit Entity --
	-----------------




	type Edit_Entity_Module is new KOW_View.Modules.Module_Type with record
		-- a type for rendering the data from entities into a form that can save the entity back
		Id			: KOW_Ent.ID_Type;
		Has_Id			: Boolean := True;
		Entity_Tag		: Unbounded_String;
		Template_Name		: Unbounded_String;
		Inlined_Entity_Tags	: KOW_Lib.UString_Vectors.Vector;
		Narrow			: Boolean;
		Ignore			: KOW_Lib.UString_Vectors.Vector;

		Form_Life_Time		: Duration;
		-- how many secconds the form will be active
	end record;



	overriding
	procedure Process_Request(
			Module		: in out Edit_Entity_Module;
			Request		: in     AWS.Status.Data;
			Response	:    out Unbounded_String
		);


	package View_Entity_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
							Module_Type	=> Edit_Entity_Module,
							Component	=> KOW_View.Entities.Components.Component'Access
					);

	----------------------
	-- Edit User Entity --
	----------------------
	
	type Edit_User_Entity_Module is new Edit_Entity_Module with null record;
	-- use Edit_Entity_Module to create a form to edit user properties to user
	-- types derived from kow_sec.entities.user_type

	overriding
	procedure Process_Request(
			Module		: in out Edit_User_Entity_Module;
			Request		: in     AWS.Status.Data;
			Response	:    out Unbounded_String
		);
	-- draw up the form for the loged user
	-- if no user is loged ir it's loged as other user type, return an empty string
	--
	-- also, make sure to reload the entity from the database at loading time
	


	package View_Entity_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
							Module_Type	=> Edit_User_Entity_Module,
							Component	=> KOW_View.Entities.Components.Component'Access
					);


	-------------------
	-- Create Entity --
	-------------------

	type Create_Entity_Module is new KOW_View.Modules.Module_Type with record
		-- a type for rendering the form for creating new entities

		Entity_Tag		: Unbounded_String;
		Template_Name		: Unbounded_String;
		Inlined_Entity_Tags	: KOW_Lib.UString_Vectors.Vector;
		Ignore			: KOW_Lib.UString_Vectors.Vector;

		Form_Life_Time		: Duration;
		-- how many secconds the form will be active
	end record;



	overriding
	procedure Process_Request(
			Module		: in out Create_Entity_Module;
			Request		: in     AWS.Status.Data;
			Response	:    out Unbounded_String
		);


	package View_Entity_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
							Module_Type	=> Create_Entity_Module,
							Component	=> KOW_View.Entities.Components.Component'Access
					);


	---------------------------
	-- Entity Browser Module --
	---------------------------
	
	type Entity_Browser_Module is new KOW_View.Modules.Module_Type with record
		-- The entity browser module not only browse throughout entities of various kinds but
		-- also allows the user to edit and create new ones.
		--
		-- It can be controlled by HTTP parameters.
		--
		--
		-- Available parameters:
		-- 	entity_tag		=> the tag for the entity to browse.
		-- 	allow_creation		=> can we create new entities ( true/false )
		-- 	allow_edit		=> can the entity be edited?
		-- 	view_entity_template	=> the tempate when viewing an entity
		-- 	edit_entity_template	=> the template when editting an entity
		--	listing_template	=> the template for listing the entity


		Entity_Tag	: Unbounded_String;
		-- the tag for the entity to browse

		Allow_Creation	: Boolean;
		-- can the user create new entities?
		Allow_Editting	: Boolean;
		-- can the user edit the available entities?


		View_Entity_Template_Name	: Unbounded_String;
		-- the template used when viewing entities

		Edit_Entity_Template_Name	: Unbounded_String;
		-- the template used when editting an entity

		Create_Entity_Template_Name	: Unbounded_String;
		-- the template used when creating a new entity

		List_Entities_Template_Name	: Unbounded_String;
		-- the template used when listing the entities
		
		Get_IDs_Function		: Unbounded_String;
		-- when set, it's the name of the Get_IDs function to be used
		
		Inlined_Entity_Tags		: KOW_Lib.UString_Vectors.Vector;

		Narrow				: Boolean;

		Ignore			: KOW_Lib.UString_Vectors.Vector;


		User_Data_Only			: Boolean;
		User_Identity_Column		: Unbounded_String;




		Form_Life_Time			: Duration;
		-- how many secconds the form will be active
	end record;



	overriding
	procedure Process_Request(
			Module		: in out Entity_Browser_Module;
			Request		: in     AWS.Status.Data;
			Response	:    out Unbounded_String
		);
	-- the entire cycle for this module is inside this single procedure.


	package View_Entity_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
							Module_Type	=> Entity_Browser_Module,
							Component	=> KOW_View.Entities.Components.Component'Access
					);

end KOW_View.Entities.Modules;
