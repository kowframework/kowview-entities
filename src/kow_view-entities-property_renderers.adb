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


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Generic_Property_Metadata;


package body KOW_View.Entities.Property_Renderers is


	function Get_Default_Renderer(
				Property	: in KOW_Ent.Entity_Property_Type'Class
			) return Property_Renderer_Access is
	begin
		return null;
	end Get_Default_Renderer;

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
