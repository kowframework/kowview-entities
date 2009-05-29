


-- Package with helper methods for Aw_View.Entities

--------------
-- Ada 2005 --
--------------


---------------
-- Ada Works --
---------------
with Aw_Ent;
with Aw_Lib.Locales;


---------
-- AWS --
---------
with AWS.Status;
with Templates_Parser;


package Aw_View.Entities_Helper is

	function Assoc_Label(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale	:= Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association;
	-- associate the label of this entity type to variabl_name

	
	function Assoc_Labels(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association;
	-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string


	function Assoc_Resolved_Labels(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association;
	-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
	-- if the property is a foreign key, get the label for the related entity instead of the property's label


	function Assoc_Values(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association;
	-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string


	function Assoc_Resolved_Values(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association;
	-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
	-- if the type is a Foreign Key, get not the ID but the label for this single related entity


	function Assoc_Id(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class
		) return Templates_Parser.Association;
	-- associate the ID for this entity as string.

	function Assoc_column_ids(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class
		) return Templates_Parser.Association;
	-- create a Tag inside with all Ids (ordered by the entity's registry) as string


	procedure Insert(
			Set		: in out Templates_Parser.Translate_Set;
			Variable_Prefix	: in     String;
			Entity		: in     Aw_Ent.Entity_Type'Class;
			Locale		: in     Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		);
	-- call all Assoc_* functions inserting the results in the translated set.
	-- create the associations :
	-- 	[P]_values
	-- 	[P]_labels
	-- 	[P]_column_ids
	-- Where [P] is the value for Variable_Prefix


	

	procedure Load(
			Data		: in     AWS.Status.Data;
			Variable_Prefix	: in     String;
			Entity		: in out Aw_Ent.Entity_Type'Class
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
			Variable_Prefix	: in String
		) return Aw_Ent.Entity_Type'Class;
	-- The same as the Load procedure, but create and load the entity from the database if it's set
	-- read the data from:
	-- 	[P_]entity_tag		=> the tag for this entity
	--
	-- and the others that the Load procedure read


end Aw_View.Entities_Helper;

