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
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Properties;
with KOW_Lib.UString_Vectors;

--------------------------------------------
-- package with some useful properties... --
--------------------------------------------



package KOW_View.Entity_Properties is

	----------------------
	-- Helper Functions --
	----------------------

	function Thumb_Name( Orig : in String ) return String;




	----------------------------------
	-- Hidden UString Property Type --
	----------------------------------


	type Hidden_UString_Property_Type is new KOW_Ent.Properties.UString_Property_Type with null record;

	function New_Hidden_UString_Property(
				Column_Name	: in     String;
				Getter		: KOW_Ent.Properties.UString_Getter_Callback;
				Setter		: KOW_Ent.Properties.UString_Setter_Callback;
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL


	
	-----------------------------
	-- Rich Text Property Type --
	-----------------------------


	type Rich_Text_Property_Type is new KOW_Ent.Properties.UString_Property_Type with null record;

	function New_Rich_Text_Property(
				Column_Name	: in     String;
				Getter		: KOW_Ent.Properties.UString_Getter_Callback;
				Setter		: KOW_Ent.Properties.UString_Setter_Callback;
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
		File_Types	: KOW_Lib.UString_Vectors.Vector;
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
				Getter		: KOW_Ent.Properties.UString_Getter_Callback;
				Setter		: KOW_Ent.Properties.UString_Setter_Callback;
				Upload_Path	: in     String;
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150;
				File_Types	: in     String := ""
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL
	-- file types is a coma separated values list of allowed file extensions
	-- 	gif,jpg,png,jpeg
	--
	--
	-- it's rendered in the HTML using the "accept" parameter..



	--------------------------------
	-- Image Upload Property Type --
	--------------------------------


	type Image_Upload_Property_Type is new File_Upload_Property_Type with record
		thumbnail	: unbounded_string;
		-- geometry of the thumbnails


		convert		: unbounded_string;
		-- if set, contains the extension to convert the image in the server..
	end record;

	overriding
	procedure Set_Property(
				Property	: in     Image_Upload_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;			-- the entity
				Value		: in     String					-- the String representation of this value
			);


	function New_Image_Upload_Property(
				Column_Name	: in     String;
				Getter		: KOW_Ent.Properties.UString_Getter_Callback;
				Setter		: KOW_Ent.Properties.UString_Setter_Callback;
				Upload_Path	: in     String;
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150;
				Thumbnail	: in     String := "150x150";
				Convert		: in     String := ""		-- default: do not convert image after upload... should be the destination extension
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL


end KOW_View.Entity_Properties;