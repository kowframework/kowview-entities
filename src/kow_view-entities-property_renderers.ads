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
with KOW_View.Modules;


---------
-- AWS --
---------
with AWS.Status;

package KOW_View.Entities.Property_Renderers is





	-----------------------
	-- Property Renderer --
	-----------------------

	type Property_Renderer_Interface is abstract new KOW_Ent.Entity_Property_Metadata_Interface with null record;
	type Property_Renderer_Ptr is access all Property_Renderer_Interface'Class;

	procedure Render_Property(
				Renderer	: in out Property_Renderer_Interface;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is abstract;
	-- render the entity property into the output


	function Get_Default_Renderer(
				Property	: in KOW_Ent.Entity_Property_Type'Class
			) return Property_Renderer_Ptr;
	-- uses Default_Renderers_Registry to look for the property renderer to return..
	-- if none found return the singleton instance of the Basic_Renderer_type

	package Property_Renderer_Metadata is new KOW_Ent.Generic_Property_Metadata(
						Entity_Property_Metadata_Type	=> Property_Renderer_Interface,
						Metadata_Access			=> Property_Renderer_Ptr,
						On_Null				=> Get_Default_Renderer
					);
	

	--------------------------------
	-- Property Renderer Registry --
	--------------------------------
	
	package Renderer_Maps is new Ada.Containers.Ordered_Maps(
					Key_Type	=> Unbounded_String,
					Element_Type	=> Property_Renderer_Ptr
				);

	protected Default_Renderers_Registry is

		-- the property renderers here are used in case no renderer is registered to a given property
		-- in the entity property metadata.
		--
		-- These are called by the basic property renderer
		
		procedure Set(
					Property_Tag	: in Ada.Tags.Tag; 
					Renderer	: in Property_Renderer_Ptr
				);

		procedure Set(
					Property_Tag	: in Unbounded_String;
					Renderer	: in Property_Renderer_Ptr
				);

		function Get(
					Property_Tag	: in Ada.Tags.Tag
				) return Property_Renderer_Ptr;

		function Get(
					Property_Tag	: in Unbounded_String
				) return Property_Renderer_Ptr;
	private
		Renderer_Map : Renderer_Maps.Map;
	end Default_Renderers_Registry;


	-----------------------------
	-- Basic Property Renderer --
	-----------------------------

	type Basic_Property_Renderer_Type is new Property_Renderer_Interface with null record;
	-- render as text input using dijit.Form.TextBox widget
	overriding
	procedure Render_Property(
				Renderer	: in out Basic_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			);
	-- simply render using text input

	function Get_Label(
				Renderer	: in Basic_Property_Renderer_Type;
				Request		: in AWS.Status.Data;
				Entity		: in KOW_Ent.Entity_Type'Class;
				Property	: in KOW_Ent.Entity_Property_Type'Class;
				Style		: in Rendering_Style_Type
			) return String;

	procedure Get_Input(
				Renderer	: in out Basic_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			);
	
	----------------------------
	-- Text Property Renderer --
	----------------------------
	type Text_Property_Renderer_Type is new Basic_Property_Renderer_Type with null record;

	overriding
	procedure Get_Input(
				Renderer	: in out Text_property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
		);

	---------------------------------
	-- Rich Text Property Renderer --
	---------------------------------

	type Rich_Text_Property_Renderer_Type is new Basic_Property_Renderer_Type with null record;

	overriding
	procedure Get_Input(
				Renderer	: in out Rich_Text_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			);

end KOW_View.Entities.Property_Renderers;
