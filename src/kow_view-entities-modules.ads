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


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Ent;
with KOW_Lib.Json;
with KOW_View.Entities.Components;
with KOW_View.Modules;
with KOW_View.Modules.Stateless_Module_Factories;

---------
-- AWS --
---------
with AWS.Status;

package KOW_View.Entities.Modules is

	------------------------
	-- Entity_Module_Type --
	------------------------
	
	-- this is the basic module type for all entity handling operations...
	-- 
	-- it's an abstract type to enforce it not being instanciated as it does nothing. :)
	

	type Entity_Module_Type is abstract new KOW_View.Modules.Module_Type with record
		Entity_Tag	: Unbounded_String;
		-- the base/core entity tag..
		-- used only by the New_Entity method...
		-- so, in case you need to enforce creating only one type of entity
		-- you can simply override that method
		--
		--
		-- all the object method calls in this code has dynamic dispatching enabled.
		-- also, all the rendering methods only append to the output

		New_Label	: Unbounded_String;
		Edit_Label	: Unbounded_String;
		Submit_Label	: Unbounded_String;
		List_Label	: Unbounded_String;

		Narrow		: Boolean;
		-- used by the load_entity function

		Style		: Rendering_Style_Type;
		-- in setup don't use the _rendering sufix
	end record;

	overriding
	procedure Initialize_Request(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			);
	-- setup the entity_tag and narrow variables...
	-- if you override this method remember to call the Entity_Module_Type's implementation of it.


	overriding
	procedure Process_Body(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			);
	
	overriding
	procedure Process_Json_Request(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			);

	
	----------------------
	-- Security Methods --
	----------------------

	function Can_Create(
				Module	: in     Entity_Module_Type;
				Request	: in     AWS.Status.Data
			) return Boolean;
	-- check if the user can created entities
	-- default implementation check if the user is online only
	
	function Can_Edit(
				Module	: in     Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class
			) return Boolean;
	-- check if the user can edit the given entity
	
	-------------------
	-- Data Handling --
	-------------------
	
	function Get_Entity_Id(
				Module	: in Entity_Module_Type;
				Request	: in AWS.Status.Data
			) return Integer;
	-- try to get the entity ID.
	-- returns -1 if there is no entity id to be returned..

	function Query_Entities(
				Module	: in Entity_Module_Type;
				Request	: in AWS.Status.Data;
				From	: in Positive;
				Limit	: in Natural
			) return KOW_Ent.Id_Array_Type;

	function New_Entity(
				Module	: in Entity_Module_Type
			) return KOW_Ent.Entity_Type'Class;
	-- create a new object of the configured entity tag
	
	function Load_Entity(
				Module	: in Entity_Module_Type;
				Id	: in Integer
			) return KOW_Ent.Entity_Type'Class;
	-- load the entity of the configured entity tag with the given id
	-- if the configured narrow property is true then return the results for KOW_Ent.Narrow( entity )
	-- if id == -1 do not load, only call create

	function Load_Entity(
				Module	: in Entity_Module_Type;
				Id	: in KOW_Ent.Id_Type
			) return KOW_Ent.Entity_Type'Class;
	-- load the entity of the configured entity tag with the given id
	-- if the configured narrow property is true then return the results for KOW_Ent.Narrow( entity )
	-- if id == -1 do not load, only call create
	--
	-- extends this one in case you want to extend any load entity... 


	function Get_Properties(
				Module	: in Entity_Module_Type;
				Entity	: in KOW_Ent.Entity_Type'Class
			) return KOW_Ent.Property_Lists.List;
	-- ge the properties that will be used by this module..


	procedure Set_Values(
				Module	: in out Entity_Module_Type;
				Entity	: in out KOW_Ent.Entity_Type'Class;
				Request	: in     AWS.Status.Data
			);
	-- set the values




	-----------------------
	-- Rendering Methods --
	-----------------------

	procedure Render_List(
				Module		: in out Entity_Module_Type;
				Request		: in     AWS.Status.Data;
				Output		:    out Unbounded_String
			);
	-- render the entity list;
	-- the process is broken down into
	-- 	=> Query_Entities
	-- 	=> Render_List_Title
	-- 	=> Render_List_Navigation_Bar
	-- 	=> foreach Entity <li>Render_View</li>
	-- 	=> Render_List_Navigation_Bar

	procedure Render_List_Title(
				Module		: in out Entity_Module_Type;
				Request		: in     AWS.Status.Data;
				Output		:    out Unbounded_String
			);
	

	procedure Render_List_Navigation_Bar(
				Module		: in out Entity_Module_Type;
				Request		: in     AWS.Status.Data;
				From		: in     Positive;
				Limit		: in     Natural;
				Total_Shown	: in     Natural;
				Output		:    out Unbounded_String
			);
	-- render the navigation bar for the entity list


	procedure Render_View(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			);
	-- render the entity into a unbounded_string variable following the style
	--
	-- style is used only by the renderer and every element fits inside a 
	-- 		<label>
	-- 			<span class="label">LABEL</span>
	-- 			<span class="value"><!-- RENDER THE INPUT/VALUE DISPLAY --></span>
	--
	-- 		</label>
	--
	-- how this table is displayed is then controlled by the CSS style and browser
	-- NOTE :: this will ensure the form will be usable in any browser
	--
	-- The process is broken down into
	-- 	=> Render_View_Title
	-- 	=> Render_View_Form_Open
	-- 	=> Render_View_Properties
	-- 	=> Render_View_Buttons
	-- 	=> Render_View_Form_Close

	
	procedure Render_View_Title(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			);
	-- renders the title for the view
	-- in this case, render the entity title if it's in a big style


	procedure Render_View_Form_Open(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			);
	-- render the form start tag (including the fieldset tag).
	-- by default, only renders <form> when in edit mode


	procedure Render_View_Properties(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			);
	-- call the renderering for each <label>...</label>
	
	procedure Render_View_Buttons(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			);
	-- render the buttons for interacting with the given entity
	-- 	=> save when in big edit mode
	-- 	=> edit when in big view mode
	-- 	=> none otherwise
	
	procedure Render_View_Form_Close(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Entity	: in     KOW_Ent.Entity_Type'Class;
				Output	:    out Unbounded_String
			);
	-- closes the tags opened by Render_View_Form_Open_

	--------------------
	-- Helper Methods --
	--------------------


	function Is_Edit( Module : in Entity_Module_Type ) return Boolean;

	procedure Check_Tag( Module : in KOW_View.Modules.Module_Type'Class );
	-- check if the module is in Entity_Module_Type'Class 
	-- if not, raise constraint error




	--------------------------------------
	-- An instance of the entity module --
	--------------------------------------
	type Entity_Module is new Entity_Module_Type with null record;
	package Entity_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
							Module_Type	=> Entity_Module,
							Component	=> KOW_View.Entities.Components.Component'Access
						);

end KOW_View.Entities.Modules;
