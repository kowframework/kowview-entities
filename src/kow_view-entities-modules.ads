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

		Narrow		: Boolean;
		-- used by the load_entity function
	end record;

	overriding
	procedure Initialize_Request(
				Module	: in out Entity_Module_Type;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			);
	-- setup the entity_tag and narrow variables...
	-- if you override this method remember to call the Entity_Module_Type's implementation of it.


	function New_Entity(
				Module	: in Entity_Module_Type
			) return KOW_Ent.Entity_Type'Class;
	-- create a new object of the configured entity tag
	
	function Load_Entity(
				Module	: in Entity_Module_Type;
				Id	: in Natural
			) return KOW_Ent.Entity_Type'Class;
	-- load the entity of the configured entity tag with the given id
	-- if the configured narrow property is true then return the results for KOW_Ent.Narrow( entity )


	function Get_Properties(
				Module	: in Entity_Module_Type;
				Entity	: in KOW_Ent.Entity_Type'Class
			) return KOW_Ent.Property_Lists.List;
	-- ge the properties that will be used by this module..

	function Render_View(
				Module	: in Entity_Module_Type;
				Request	: in AWS.Status.Data;
				Entity	: in KOW_Ent.Entity_Type'Class;
				Style	: in KOW_View.Entities.Rendering_Style_Type
			) return Unbounded_String;
	-- render the entity into a unbounded_string variable following the style
	--
	-- style is used only by the renderer and every element fits inside a 
	-- 	<fieldset>
	-- 		<legend>LABEL</legend>
	--
	-- 		<label>
	-- 			<!-- Render results // -->
	-- 		</label>
	-- 		(...)
	-- 	</fieldset>
	--
	-- how this table is displayed is then controlled by the CSS style and browser
	-- NOTE :: this will assure the form will be usable in any browser

end KOW_View.Entities.Modules;
