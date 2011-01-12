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


end KOW_View.Entities.Modules;
