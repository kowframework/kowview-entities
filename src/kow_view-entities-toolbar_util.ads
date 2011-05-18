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
-- Utilities for handling toolbar                                           --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_View.Modules;			use KOW_View.Modules;

package KOW_View.Entities.Toolbar_Util is

	type Toolbar_Mode is ( Navigation_Toolbar, Form_Toolbar );



	------------------------
	-- The toolbar itself --
	------------------------

	procedure Toolbar_Open(
			Output	:    out Unbounded_String;
			Module	: in out Module_Type'Class;
			Mode	: in     Toolbar_Mode
		);
	procedure Toolbar_Close(
			Output	: out Unbounded_String
		);

	
	-------------------------
	-- Buttons and buttons --
	-------------------------

	procedure Append_Button(
			Output	:   out Unbounded_String;
			Label	: in    String;
			onClick	: in    String;
			Enabled	: in    Boolean := True
		);

	procedure Append_Button(
			Output	:   out Unbounded_String;
			Label	: in    Unbounded_String;
			onClick	: in    String;
			Enabled	: in    Boolean := True
		);

	
	procedure Append_Link_Button(
			Output	:    out Unbounded_String;
			Label	: in     Unbounded_String;
			Href	: in     String;
			Enabled	: in    Boolean := True
		);

	procedure Append_Link_Button(
			Output	:    out Unbounded_String;
			Label	: in     String;
			Href	: in     String;
			Enabled	: in    Boolean := True
		);


end KOW_View.Entities.Toolbar_Util;
