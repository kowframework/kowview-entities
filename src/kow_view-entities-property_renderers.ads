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


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Generic_Property_Metadata;


package KOW_View.Entities.Property_Renderers is


	---------------------
	-- Rendering Style --
	---------------------

	type Render_Style_Type is (
				Tabular_Rendering,
				Inlined_Rendering,
				Tabular_Editing_Rendering,
				Inilied_Editing_Rendering
			);
	-- how the property can be rendered..



	-----------------------
	-- Property Renderer --
	-----------------------

	type Property_Renderer_Interface is abstract new KOW_Ent.Entity_Property_Metadata_Interface with null record;
	type Property_Renderer_Access is access all Property_Renderer_Interface'Class;

	procedure Render_Property(
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Render_Style_Type;
				Output		:    out Unbounded_String
			) is abstract;
	-- render the entity property into the output


	function Get_Default_Renderer(
				Property	: in KOW_Ent.Entity_Property_Type'Class
			) return Property_Renderer_Access;

	package Property_Renderer_Metadata is new KOW_Ent.Generic_Property_Metadata(
						Entity_Property_Metadata_Type	=> Property_Renderer_Interface,
						Metadata_Access			=> Property_Renderer_Access,
						On_NUll				=> Get_Default_Renderer
					);
	
	-----------------------------
	-- Basic Property Renderer --
	-----------------------------


--	type Basic_Property_Renderer is new Property_Renderer_Interface with null record;
--	
--	overriding
--	procedure Render_Property(
--				Entity		: in     KOW_
--
end KOW_View.Entities.Property_Renderers;
