------------------------------------------------------------------------------
--                                                                          --
--                         Ada Works :: Entity Forms                        --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------



-- Main Aw_View :: Entity_Forms package.
--
-- This package implements a generic way for dealing with entities
--
-- author Marcelo C. de Freitas <marcelo@kow.com.br>
-- createdAt 2009-04-XX
--
-- Repository information:
-- $Date: $
-- $Revision: $
-- $Author: $



---------
-- AWS --
---------
with AWS.Status;
with Templates_Parser;

---------------
-- Ada Works --
---------------
with Aw_View;

package Aw_View.Entity_Forms is



	function Read_Form_Data( Request : in AWS.Status.Data ) return Entity_Type;
	-- Read the form data, returning one entity
	--


	------------------------
	-- The FORM Component --
	------------------------

	type Component_Type is new Aw_View.Component_Interface with private;




	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		);
	-- initialize this component
	


	------------------------------------
	-- Modules for the FORM component --
	------------------------------------

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Aw_View.Module_Instance_Interface'Class;
	-- create a module instance by given name
	-- Available module:
	-- 	view_entity
	-- 	TODO :: edit_entity
	-- 	TODO :: create_entity
	


	type View_Entity_Module is new Aw_View.Module_Instance_Interface with private;
	-- a type for rendering FORMS for entities.

	overriding
	procedure Process_Request(
			Module		: in out View_Entity_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- Draws a the entity as text (usualy HTML text)


private
	type View_Entity_Module is new Aw_View.Module_Instance_Interface with record
		--Entity	: Entity_Type;
		Entity_Tag	: Ada.Tags.Tag;

	end record;




end Aw_View.Entity_Forms;
