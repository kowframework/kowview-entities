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
with Ada.Exceptions;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;

package KOW_View.Entities.Validation is



	VALIDATION_ERROR : Exception;


	type Validatable_Entity_Interface is interface;


	procedure Validate_Property(
				Entity	: in out Validatable_Entity_Interface;
				Property: in     KOW_Ent.Entity_Property_Ptr;
				Value	: in     String
			) is null;
	-- validate each one of the values before calling "set";

	procedure Validate(
				Entity	: in out Validatable_Entity_Interface;
				Request	: in     AWS.Status.Data
			) is null;
 	-- validate an entity after all the properties have been set
	-- not really useful when the entity has properties that store data in-disk.


	procedure Raise_Exception(
				Column	: in String;
				Message	: in String
			);
	-- raise an exception with no entity information

	procedure Raise_Exception(
				Entity	: in Validatable_Entity_Interface'Class;
				Column	: in String;
				Message	: in String
			);
	-- raise an exception message 

	procedure Raise_Exception(
				Entity	: in Validatable_Entity_Interface'Class;
				Column	: in String;
				Message	: in String;
				E	: in Ada.Exceptions.Exception_Occurrence
			);
	-- raise an exception message 

end KOW_View.Entities.Validation;
