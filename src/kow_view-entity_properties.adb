



--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Tags;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Properties;
with KOW_Lib.File_System;

--------------------------------------------
-- package with some useful properties... --
--------------------------------------------



package body KOW_View.Entity_Properties is


	----------------------------------
	-- Hidden UString Property Type --
	----------------------------------

	function New_Hidden_UString_Property(
				Column_Name	: in     String;
				Getter		: not null access function( Entity : in Entity_Type'Class ) return Unbounded_String;
				Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Unbounded_String );
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150
			) return Entity_Property_Ptr is
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL
		UStr : Hidden_UString_Property_Type;
	begin
		UStr.Column_Name	:= To_Unbounded_String( Column_Name );
		UStr.Getter		:= Getter;
		UStr.Setter		:= Setter;
		UStr.Default_Value	:= To_Unbounded_String( Default_Value );
		UStr.Immutable		:= Immutable;
		UStr.Length		:= Length;
		return new Hidden_UString_Property_Type'( UStr );
	end New_Hidden_UString_Property;



	-------------------------------
	-- File Upload Property Type --
	-------------------------------


	package Positive_Random is new Ada.Numerics.Discrete_Random( Result_Subtype => Positive );

	overriding
	procedure Set_Property(
				Property	: in     File_Upload_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;			-- the entity
				Value		: in     String					-- the String representation of this value
			) is
		-- handle the file upload 
		-- 'value' is a temporary location for the file.
		--
		-- if /="", move the file to it's definitive location ( [P.Upload_Path]/[ID]_column_name.[ext]) and set it using the setter

		function The_Destination_Path return String is
			use Ada.Tags;
			use KOW_Lib.File_System;

			Extension : constant String := Ada.Characters.Handling.To_Lower( Ada.Directories.Extension( Value ) );
		begin
			if Entity.id.My_Tag = Entity'Tag then
				-- if we are updating the entity, we have the id... so we save it where we want it
				return To_String( Property.Upload_Path ) / 
						KOW_Ent.To_String( Entity.ID ) & '_' & To_String( Property.Column_Name ) & '.' & Extension;
			else
				declare
					Gen : Positive_Random.Generator;
				begin
					Positive_Random.Reset( Gen );

					loop
						declare
							Rnd_Str : constant String := Ada.Strings.Fixed.Trim(
												Positive'Image( Positive_Random.Random( Gen ) ),
												Ada.Strings.Both
											);
							R : constant String := To_String( Property.Upload_Path ) / 
								"tmp_" & Rnd_Str & '_' & To_String( Property.Column_Name ) & '.' & Extension;
						begin
							if not Ada.Directories.Exists( R ) then
								return R;
							end if;
						end;
					end loop;
				end;
			end if;

		end The_Destination_Path;

	begin
		if Value = "" then
			return;
		end if;


		declare
			Destination_Path : constant String := The_Destination_Path;
			Old_Path	 : constant String := To_String( Property.Getter.all( Entity ) );
		begin
			if Ada.Directories.Exists( Destination_Path ) then
				Ada.Directories.Delete_File( Destination_Path );
			end if;

			if Old_Path /= "" and then Ada.Directories.Exists( Old_Path ) then
				Ada.Directories.Delete_File( Old_Path );
			end if;

			Ada.Directories.Copy_File( Source_Name => Value, Target_Name => Destination_Path );
			Property.Setter.all( Entity, To_Unbounded_String( Destination_Path ) );
		end;
	end Set_Property;



	function New_File_Upload_Property(
				Column_Name	: in     String;
				Getter		: not null access function( Entity : in Entity_Type'Class ) return Unbounded_String;
				Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Unbounded_String );
				Upload_Path	: in     String;
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150;
				File_Types	: in     String := ""
			) return Entity_Property_Ptr is
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL
		UStr : File_Upload_Property_Type;

		use Ada.Directories;
	begin

		if not Ada.Directories.Exists( Upload_Path ) then
			Ada.Directories.Create_Path( Upload_Path );
		elsif Ada.Directories.Kind( Upload_Path ) /= Directory then
			raise Name_Error with "[" & Column_name & "] upload path must be a directory!";
		end if;


		UStr.Column_Name	:= To_Unbounded_String( Column_Name );
		UStr.Getter		:= Getter;
		UStr.Setter		:= Setter;
		UStr.Upload_Path	:= To_Unbounded_String( Upload_Path );
		UStr.Default_Value	:= To_Unbounded_String( Default_Value );
		UStr.Immutable		:= Immutable;
		UStr.Length		:= Length;
		UStr.File_Types		:= To_Unbounded_String( Ada.Characters.Handling.To_Lower( File_Types ) );
		return new File_Upload_Property_Type'( UStr );
	end New_File_Upload_Property;



	--------------------------------
	-- Image Upload Property Type --
	--------------------------------


	function New_Image_Upload_Property(
				Column_Name	: in     String;
				Getter		: not null access function( Entity : in Entity_Type'Class ) return Unbounded_String;
				Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Unbounded_String );
				Upload_Path	: in     String;
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150
			) return Entity_Property_Ptr is
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL
		UStr : Image_Upload_Property_Type;

		use Ada.Directories;
	begin

		if not Ada.Directories.Exists( Upload_Path ) then
			Ada.Directories.Create_Path( Upload_Path );
		elsif Ada.Directories.Kind( Upload_Path ) /= Directory then
			raise Name_Error with "[" & Column_name & "] upload path must be a directory!";
		end if;


		UStr.Column_Name	:= To_Unbounded_String( Column_Name );
		UStr.Getter		:= Getter;
		UStr.Setter		:= Setter;
		UStr.Upload_Path	:= To_Unbounded_String( Upload_Path );
		UStr.Default_Value	:= To_Unbounded_String( Default_Value );
		UStr.Immutable		:= Immutable;
		UStr.Length		:= Length;
		UStr.File_Types		:= To_Unbounded_String( "jpg,jpeg,gif,png" );
		return new Image_Upload_Property_Type'( UStr );
	end New_Image_Upload_Property;





end KOW_View.Entity_Properties;
