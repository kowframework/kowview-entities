



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



with GNAT.Expect;
with GNAT.OS_Lib;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Properties;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;

--------------------------------------------
-- package with some useful properties... --
--------------------------------------------



package body KOW_View.Entity_Properties is



	----------------------
	-- Helper Functions --
	----------------------

	function Thumb_Name( Orig : in String ) return String is
	begin
		return Orig & "_thumb." & Ada.Directories.Extension( Orig );
	end Thumb_Name;


	----------------------------------
	-- Hidden UString Property Type --
	----------------------------------

	function New_Hidden_UString_Property(
				Column_Name	: in     String;
				Getter		: KOW_Ent.Properties.UString_Getter_Callback;
				Setter		: KOW_Ent.Properties.UString_Setter_Callback;
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


	-----------------------------
	-- Rich Text Property Type --
	-----------------------------


	function New_Rich_Text_Property(
				Column_Name	: in     String;
				Getter		: KOW_Ent.Properties.UString_Getter_Callback;
				Setter		: KOW_Ent.Properties.UString_Setter_Callback;
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150
			) return Entity_Property_Ptr is
		-- used to assist the creation of UString properties.
		-- default_value represents the value to be set when the one retoner from database is NULL
		UStr : Rich_Text_Property_Type;
	begin
		UStr.Column_Name	:= To_Unbounded_String( Column_Name );
		UStr.Getter		:= Getter;
		UStr.Setter		:= Setter;
		UStr.Default_Value	:= To_Unbounded_String( Default_Value );
		UStr.Immutable		:= Immutable;
		UStr.Length		:= Length;
		return new Rich_Text_Property_Type'( UStr );
	end New_Rich_Text_Property;





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


		-- now we check the allowed extensions
		declare
			Ext : String := Ada.Characters.Handling.To_Lower( Ada.Directories.Extension( Value ) );

			Is_OK : Boolean := False;
			procedure Iterator( C : in KOW_Lib.UString_Vectors.Cursor ) is
			begin
				if IS_OK then
					return;
				end if;

				if Ext = KOW_Lib.UString_Vectors.Element( C ) then
					IS_OK := True;
				end if;
			end Iterator;
		begin
			KOW_Lib.UString_Vectors.Iterate(
						Property.File_Types,
						Iterator'Access
					);

			if not IS_OK then
				raise KOW_Ent.Data_Validation_Error with "invalid file type.. please submit one of the following: " & 
							KOW_Lib.String_Util.Implode( ',', Property.File_Types );
			end if;
		end;

		

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
				Getter		: KOW_Ent.Properties.UString_Getter_Callback;
				Setter		: KOW_Ent.Properties.UString_Setter_Callback;
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
		UStr.File_Types		:= KOW_Lib.String_Util.Explode( ',', Ada.Characters.Handling.To_Lower( File_Types ) );
		return new File_Upload_Property_Type'( UStr );
	end New_File_Upload_Property;



	--------------------------------
	-- Image Upload Property Type --
	--------------------------------


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
		UStr.File_Types		:= KOW_Lib.String_Util.Explode( ',',  "jpg,jpeg,gif,png" );
		UStr.Thumbnail		:= To_Unbounded_String( thumbnail );
		UStr.Convert		:= To_Unbounded_String( Convert );
		return new Image_Upload_Property_Type'( UStr );
	end New_Image_Upload_Property;


	overriding
	procedure Set_Property(
				Property	: in     Image_Upload_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;			-- the entity
				Value		: in     String					-- the String representation of this value
			) is
		Old_Name : String := Get_Property( Property, Entity );



	begin
		Set_Property(
				File_Upload_Property_Type( Property ),
				Entity,
				Value
			);
		if Old_Name /= "" then
			if Ada.Directories.Exists( Thumb_Name( Old_name ) ) then
				Ada.Directories.Delete_File( Thumb_Name( Old_Name ) );
			end if;
		end if;
		

		--
		-- convert the file, if needed
		--
		if Property.Convert /= "" then
			declare
				P_From		: aliased String := Get_Property( Property, Entity );
				Ext		:         String := Ada.Directories.Extension( P_From );
				P_To		: aliased String := P_From( P_From'First .. P_From'Last - Ext'Length ) & To_String( Property.Convert );
				Arguments	: GNAT.OS_Lib.Argument_List := (
							01	=> P_From'Unchecked_Access,
							02	=> P_To'Unchecked_Access
						);
				Out_Status	: aliased Integer;
				Output          : String := GNAT.Expect.Get_Command_Output(
							Command         => "convert",
							Arguments       => Arguments,
							Input           => "",
							Status          => Out_Status'Access,
							Err_To_Out      => True
						);
			begin
				if P_From /= P_To then
					Ada.Directories.Delete_File( P_From );
				end if;
				Property.Setter.all( Entity, To_Unbounded_String( P_To ) );
			end;

		end if;

		--
		-- generate thumbnail..
		--

		declare
			P_Thumbnail	: aliased String := "-thumbnail";
			P_Thumbpath	: aliased String := To_String( Property.Thumbnail );
			P_Property	: aliased String := Get_Property( Property, Entity );
			P_Thumbname	: aliased String := Thumb_Name( Get_Property( Property, Entity ) );

			Arguments       : GNAT.OS_Lib.Argument_List := (
							01 => P_Thumbnail'Unchecked_Access,	-- new String'( "-thumbnail" ),
							02 => P_Thumbpath'Unchecked_Access,	-- new String'( To_String( Property.Thumbnail ) ),
							03 => P_Property'Unchecked_Access,	-- new String'( Get_Property( Property, Entity ) ),
							04 => P_Thumbname'Unchecked_Access	-- new String'( Thumb_Name( Get_Property( Property, Entity ) ) )
						);
		
			Out_Status      : aliased Integer;
			Output          : String := GNAT.Expect.Get_Command_Output(
						Command         => "convert",
						Arguments       => Arguments,
						Input           => "",
						Status          => Out_Status'Access,
						Err_To_Out      => True
					);
		begin
			null;
		end;
		

		
	end Set_Property;




end KOW_View.Entity_Properties;
