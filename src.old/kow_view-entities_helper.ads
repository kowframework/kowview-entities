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





-- Package with helper methods for KOW_View.Entities

--------------
-- Ada 2005 --
--------------

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Lib.Locales;
with KOW_Lib.UString_Vectors;
with KOW_View.Entity_Property_Renderers;	use KOW_View.Entity_Property_Renderers;


---------
-- AWS --
---------
with AWS.Status;
with Templates_Parser;


package KOW_View.Entities_Helper is




	function Assoc_Label(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale	:= KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association;
	-- associate the label of this entity type to variabl_name



	-----------------
	-- The labels --
	-----------------
	
	function Get_Labels_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := "";
			Form_Pref	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag;

	
	function Assoc_Labels(
			Variable_Name	: in String;
			Form_Pref	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association;
	-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string



	-------------------------
	-- The resolved Labels --
	-------------------------
	
	function Get_Resolved_Labels_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag;



	function Assoc_Resolved_Labels(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association;
	-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
	-- if the property is a foreign key, get the label for the related entity instead of the property's label



	----------------
	-- The Values --
	----------------

	function Get_Values_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag;
	-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string



	function Assoc_Values(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association;
	-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string



	-------------------------
	-- The Resolved Values --
	-------------------------

	function Get_Resolved_Values_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag;
	-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
	-- if the type is a Foreign Key, get not the ID but the label for this single related entity



	function Assoc_Resolved_Values(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association;
	-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
	-- if the type is a Foreign Key, get not the ID but the label for this single related entity



	-----------------------
	-- The Form Elements --
	-----------------------
	
	
	function Get_Form_Elements_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Name_Prefix	: in String;
			Form_Mode	: in Form_Mode_Type;
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag;
	-- create a Tag inside with the corresponding Form element for each entity property.
	-- currently it supports:
	-- 	string (default)
	-- 	locale



	function Assoc_Form_Elements(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Name_Prefix	: in String; 
			Form_Mode	: in Form_Mode_Type;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association;
	-- create a Tag inside with the corresponding Form element for each entity property.
	-- currently it supports:
	-- 	string (default)
	-- 	locale


	---------------------------
	-- The current entity ID --
	---------------------------



	function Get_ID( Entity : in KOW_Ent.Entity_Type'Class ) return String;

	function Assoc_Id(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class
		) return Templates_Parser.Association;
	-- associate the ID for this entity as string.


	--------------------
	-- The Column IDs --
	--------------------
	

	function Get_column_ids_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag;
	-- create a Tag inside with all Ids (ordered by the entity's registry) as string


	function Assoc_column_ids(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association;
	-- create a Tag inside with all Ids (ordered by the entity's registry) as string




	-------------------------------
	-- Translate Set Integration --
	-------------------------------

	procedure Insert(
			Set		: in out Templates_Parser.Translate_Set;
			Variable_Prefix	: in     String;
			Entity		: in     KOW_Ent.Entity_Type'Class;
			Locale		: in     KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Include_Form	: in     Boolean := False;
			Form_Mode	: in     Form_Mode_Type;
			Ignore		: in     KOW_Lib.UString_Vectors.Vector
		);
	-- call all Assoc_* functions inserting the results in the translated set.
	-- create the associations :
	-- 	[P]_values
	-- 	[P]_labels
	-- 	[P]_column_ids
	-- 	[P]_form_element
	-- Where [P] is the value for Variable_Prefix


	procedure Insert_All(
			Set		: in out Templates_Parser.Translate_Set;
			Variable_Prefix	: in     String;
			Entity_Tags	: in     KOW_Lib.UString_Vectors.Vector;
			Locale		: in     KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Include_Form	: in     Boolean := False;
			Form_Mode	: in     Form_Mode_Type;
			Related_Entity	: in     KOW_Ent.Entity_Type'Class
		);
	-- call all Assoc_* functions inserting the results in the translated set.
	-- create the associations :
	-- 	[P]_has_form_element
	-- 	[P]_values
	-- 	[P]_labels
	-- 	[P]_column_ids
	-- 	[P]_form_element
	-- Where [P] is the value for Variable_Prefix
	--
	-- for each entity listed in the Entity_Tags vector using 2 dimentional tag
	--
	-- also, ignore the relation with the tag with expanded name IGNORE_RELATION

	

	procedure Load(
			Data		: in     AWS.Status.Data;
			Variable_Prefix	: in     String;
			Entity		: in out KOW_Ent.Entity_Type'Class;
			N		: in     Positive		-- parameter N of AWS.Parameters.Get() method
		);
	-- read the data from a FORM returning a new entity to be stored/loaded/whateveroaded
	-- 	[P_][TAG]__id		=> the ID for this entity (if available)
	-- 	[P_][TAG]__column_ids	=> the value for the given column
	-- where P == the Variable_Prefix value.
	--
	-- and
	--
	-- [P_] = "" if P = ""


	function Load(
			Data		: in AWS.Status.Data;
			Variable_Prefix	: in String;
			N		: in Positive			-- parameter N of AWS.Parameters.Get() method
		) return KOW_Ent.Entity_Type'Class;
	-- The same as the Load procedure, but create and load the entity from the database if it's set
	-- read the data from:
	-- 	[P_]entity_tag		=> the tag for this entity
	--
	-- and the others that the Load procedure read


end KOW_View.Entities_Helper;
