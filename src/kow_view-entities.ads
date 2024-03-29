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
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Ent;
with KOW_Lib.Json;
with KOW_Lib.File_System;
with KOW_Lib.UString_Vectors;
with KOW_Sec.Accounting;

---------
-- AWS --
---------
with AWS.Status;

package KOW_View.Entities is



	Accountant      : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "entities", KOW_View.Accountant'Access );


	---------------------------------------
	-- Store Triggering Entity Interface --
	---------------------------------------

	type Store_Triggering_Entity_Interface is interface;
	-- implement this interface in your entity if you want all the methods in this component to call
	-- Before_Store and After_Store

	procedure Before_Store(
			Entity		: in out Store_Triggering_Entity_Interface;
			Request		: in     AWS.Status.Data
		) is abstract;

	procedure After_Store(
			Entity		: in out Store_Triggering_Entity_Interface;
			Request		: in     AWS.Status.Data
		) is abstract;


	

	procedure Before_Store(
			Entity		: in out KOW_Ent.Entity_Type'Class;
			Request		: in     AWS.Status.Data
		);
	-- call the Store_Triggering interface's before_Store if available
	
	procedure After_Store(
			Entity		: in out KOW_Ent.Entity_Type'Class;
			Request		: in     AWS.Status.Data
		);
	-- call the Store_Triggering interface's after_Store if available


	---------------------
	-- Rendering Style --
	---------------------

	type Rendering_Style_Type is (
				Big_Rendering,			-- when opening a page that displays this entity
				Small_Rendering,		-- when doing previews in a list of entities
				Big_Edit_Rendering,		-- when opening a page to edit this entity
				Small_Edit_Rendering		-- when editing inlined
			);
	-- how the entity can be rendered..



end KOW_View.Entities;
