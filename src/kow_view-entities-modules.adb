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


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Ent;
with KOW_View.Entities.Property_Renderers;
with KOW_View.Locales;
with KOW_View.Modules;
with KOW_View.Modules.Stateless_Module_Factories;

---------
-- AWS --
---------
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
	begin
		Module.Entity_Tag := KOW_Config.Element( Config, "entity_tag" );
		Module.Narrow := KOW_Config.Value( Config, "narrow", True );
	end Initialize_Request;


	function New_Entity(
				Module	: in Entity_Module_Type
			) return KOW_Ent.Entity_Type'Class is
	begin
		return KOW_Ent.Entity_Registry.New_Entity( Module.Entity_Tag );
	end New_Entity;
	
	function Load_Entity(
				Module	: in Entity_Module_Type;
				Id	: in Natural
			) return KOW_Ent.Entity_Type'Class is
		Entity : KOW_Ent.Entity_Type'Class := New_Entity( Module );
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
		-- ge the properties that will be used by this module..
	begin
		return KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
	end Get_Properties;


	function Render_View(
				Module	: in Entity_Module_Type;
				Request	: in AWS.Status.Data;
				Entity	: in KOW_Ent.Entity_Type'Class;
				Style	: in KOW_View.Entities.Rendering_Style_Type
			) return Unbounded_String is
		Buffer : Unbounded_String := To_Unbounded_String( "<fieldset>" );

		procedure Iterator( C : in KOW_Ent.Property_Lists.Cursor ) is
			use KOW_View.Entities.Property_Renderers;

			Property	: KOW_Ent.Entity_Property_Ptr := KOW_Ent.Property_Lists.Element( C );
			Renderer	: Property_Renderer_Ptr := Property_Renderer_Metadata.Get( Property.all );
			Renderer_Buffer : Unbounded_String;
		begin

			KOW_View.Entities.Property_Renderers.Render_Property(
							Renderer	=> Renderer.all,
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

		Legend : constant String := "<legend>" & KOW_Ent.Get_Label( Entity, KOW_View.Locales.Get_Locale( Request ) ) & "</legend>";
	begin
		Append( Buffer, Legend );
		KOW_Ent.Property_Lists.Iterate(
					Get_Properties( Module, Entity ),
					Iterator'Access
				);
		Append( Buffer, "</fieldset>" );

		return Buffer;
	end Render_View;



end KOW_View.Entities.Modules;
