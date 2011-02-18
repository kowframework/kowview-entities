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



--------------
-- Ada 2005 --
--------------
with Ada.Exceptions;
with Ada.Tags;

package body KOW_View.Entities.Validation is

	procedure Raise_Exception(
				Column	: in String;
				Message	: in String
			) is
		-- raise an exception with no entity information
	begin
		raise VALIDATION_ERROR with '|' & Column & '|' & Message;
	end Raise_Exception;

	procedure Raise_Exception(
				Entity	: in Validatable_Entity_Interface'Class;
				Column	: in String;
				Message	: in String
			) is
		-- raise an exception message 
	begin
		raise VALIDATION_ERROR with Ada.Tags.Expanded_name( Entity'Tag ) & '|' & column & '|' & Message;
	end Raise_Exception;

	procedure Raise_Exception(
				Entity	: in Validatable_Entity_Interface'Class;
				Column	: in String;
				Message	: in String;
				E	: in Ada.Exceptions.Exception_Occurrence
			) is
		-- raise an exception message 
		use Ada.Exceptions;
	begin
		Raise_Exception(
				Entity	=> Entity,
				Column	=> Column,
				Message	=> Message & '|' & Exception_Name( E ) & '|' & Exception_Message( E )
			);
	end Raise_Exception;
end KOW_View.Entities.Validation;
