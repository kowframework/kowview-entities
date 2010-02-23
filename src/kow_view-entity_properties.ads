



--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Properties;

--------------------------------------------
-- package with some useful properties... --
--------------------------------------------



package KOW_View.Entity_Properties is


	----------------------------------
	-- Hidden UString Property Type --
	----------------------------------


	type Hidden_UString_Property_Type is new KOW_Ent.Properties.UString_Property_Type with null record;


	function New_Hidden_UString_Property(
				Column_Name	: in     String;
				Getter		: not null access function( Entity : in Entity_Type'Class ) return Unbounded_String;
				Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Unbounded_String );
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL



	-------------------------------
	-- File Upload Property Type --
	-------------------------------

	type File_Upload_Property_Type is new KOW_Ent.Properties.UString_Property_Type with record
		Upload_Path	: Unbounded_String;
		File_Types	: Unbounded_String;
	end record;

	overriding
	procedure Set_Property(
				Property	: in     File_Upload_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;			-- the entity
				Value		: in     String					-- the String representation of this value
			);
	-- handle the file upload 
	-- 'value' is a temporary location for the file.
	--
	-- if /="", move the file to it's definitive location ( [P.Upload_Path]/[ID]_column_name.[ext]) and set it using the setter



	function New_File_Upload_Property(
				Column_Name	: in     String;
				Getter		: not null access function( Entity : in Entity_Type'Class ) return Unbounded_String;
				Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Unbounded_String );
				Upload_Path	: in     String;
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150;
				File_Types	: in     String := ""
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL
	-- file types is a coma separated values list such as
	-- 	image/gif,image/jpeg
	--
	--
	-- it's rendered in the HTML using the "accept" parameter..

end KOW_View.Entity_Properties;
