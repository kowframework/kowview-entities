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
-- Renderer for properties                                                  --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Tags;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Generic_Property_Metadata;
with KOW_View.Locales;

---------
-- AWS --
---------
with AWS.Status;

package body KOW_View.Entities.Property_Renderers is


	Default_Renderer : Property_Renderer_Ptr := new Basic_Property_Renderer_Type;
	-- only one instance as it has no internal variables...

	function Get_Default_Renderer(
				Property	: in KOW_Ent.Entity_Property_Type'Class
			) return Property_Renderer_Ptr is
	begin
		return Default_Renderers_Registry.Get( Property'Tag );
	exception
		when others =>
			return Default_Renderer;
	end Get_Default_Renderer;



	--------------------------------
	-- Property Renderer Registry --
	--------------------------------
	
	protected body Default_Renderers_Registry is

		-- the property renderers here are used in case no renderer is registered to a given property
		-- in the entity property metadata.
		--
		-- These are called by the basic property renderer
		
		procedure Set(
					Property_Tag	: in Ada.Tags.Tag; 
					Renderer	: in Property_Renderer_Ptr
				) is
		begin
			Set( To_Unbounded_String( Ada.Tags.Expanded_Name( Property_Tag ) ), Renderer );
		end Set;

		procedure Set(
					Property_Tag	: in Unbounded_String;
					Renderer	: in Property_Renderer_Ptr
				) is
		begin
			Renderer_Maps.Include( Renderer_Map, Property_Tag, Renderer );
		end Set;

		function Get(
					Property_Tag	: in Ada.Tags.Tag
				) return Property_Renderer_Ptr is
		begin
			return Get( To_Unbounded_String( Ada.Tags.Expanded_Name( Property_Tag ) ) );
		end Get;

		function Get(
					Property_Tag	: in Unbounded_String
				) return Property_Renderer_Ptr is
		begin
			return Renderer_Maps.Element( Renderer_Map, Property_Tag );
		exception
			when others =>
				return null;
		end Get;
	end Default_Renderers_Registry;


	-----------------------------
	-- Basic Property Renderer --
	-----------------------------

	overriding
	procedure Render_Property(
				Renderer	: in out Basic_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
		-- render the property using the default property renderers registered by the property tag

		Label	: constant String := Get_Label(
							Renderer	=> Renderer,
							Request		=> Request,
							Entity		=> Entity,
							Property	=> Property,
							Style		=> Style
						);

		Input	: Unbounded_String;		
	begin
		Get_Input(
				Renderer	=> Renderer,
				Module		=> Module,
				Request		=> Request,
				Entity		=> Entity,
				Property	=> Property,
				Style		=> Style,
				Output		=> Input
			);
	
		Output := To_Unbounded_String( Label ) & Input;
	end Render_Property;

	function Get_Label(
				Renderer	: in Basic_Property_Renderer_Type;
				Request		: in AWS.Status.Data;
				Entity		: in KOW_Ent.Entity_Type'Class;
				Property	: in KOW_Ent.Entity_Property_Type'Class;
				Style		: in Rendering_Style_Type
			) return String is
	begin
		return KOW_Ent.Get_Label( Entity, Property.Column_Name, KOW_View.Locales.Get_Locale( Request ) );
	end Get_Label;

	procedure Get_Input(
				Renderer	: in out Basic_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
		Value	: constant String := KOW_Ent.Get_Property( Property, Entity );
	begin
		case Style is
			when Big_Rendering | Small_Rendering =>
				Output := To_Unbounded_String( Value );
			when Big_Edit_Rendering | Small_Edit_Rendering =>
				declare
					Buffer	: Unbounded_String;
				begin
					KOW_View.Modules.Include_Dojo_Package( Module, "dijit.Form.TextBox" );
					Append( Buffer, "<input type=""text"" dojoType=""dijit.Form.TextBox"" name=""" );
					Append( Buffer, Property.Column_Name );
					Append( Buffer, """ value=""" & Value & """/>" );
					Output := Buffer;
				end;
		end case;
	end Get_Input;
end KOW_View.Entities.Property_Renderers;
