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




--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;					use KOW_Ent;

package KOW_View.Entity_Property_Renderers is

	type Form_Mode_Type is ( Edit, Create );
	-- instructs if it's the form for creating or editing the entity



	function Disabled_Enabled(
				Property	: in KOW_Ent.Entity_Property_Type'Class;
				Form_Mode	: in Form_Mode_Type
			) return String;
	-- return the HTML property


	type Property_Renderer_Type is tagged null record;
	-- this type is used by the kow_view-entity application to render HTML views
	-- for both viewing and editing the property.

	procedure Render_Form(
				Renderer	: in out Property_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);


	procedure Render_View(
				Renderer	: in out Property_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Result		:    out Unbounded_String
			);



	
	type Factory_Ptr is access function return Property_Renderer_Type'Class;
	
	package Factory_Maps is new Ada.Containers.Ordered_Maps(
					Key_Type	=> Unbounded_String,
					Element_Type	=> Factory_Ptr
				);


	protected Registry is
		function Get_Renderer(
					U	: Unbounded_String
				) return Property_Renderer_Type'Class;

		function Get_Renderer(
					T	: Ada.Tags.Tag
				) return Property_Renderer_Type'Class;




		procedure Register(
					Property_Tag	: in Ada.Tags.Tag;
					Renderer_Factory: access function return Property_Renderer_Type'Class
				);
	private
		Factories : Factory_Maps.Map;
	end Registry;


	generic
		type Renderer_Type is new Property_Renderer_Type with private; 
	function Generic_Factory return Property_Renderer_Type'Class;



end KOW_View.Entity_Property_Renderers;
