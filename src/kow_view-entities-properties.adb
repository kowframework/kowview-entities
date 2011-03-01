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
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;



with GNAT.Expect;
with GNAT.OS_Lib;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Properties;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_View.Entities.Validation;

--------------------------------------------
-- package with some useful properties... --
--------------------------------------------



package body KOW_View.Entities.Properties is




	package Positive_Random is new Ada.Numerics.Discrete_Random( Result_Subtype => Positive );

	----------------------
	-- Helper Functions --
	----------------------

	function Thumb_Name( Orig : in String ) return String is
	begin
		return Orig & "_thumb." & Ada.Directories.Extension( Orig );
	end Thumb_Name;

	function Big_Name( Orig : in String ) return String is
	begin
		return Orig & "_big." & Ada.Directories.Extension( Orig );
	end Big_Name;


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

	----------------------------------
	-- Rich Text File Property Type --
	----------------------------------
	


	protected File_Lock is
		procedure Write( Destination_Path, Old_Path, Value : in String );

		procedure Read( The_path : in String; Buffer : out Unbounded_String );
	end File_Lock;


	protected body File_Lock is
		procedure Write( Destination_Path, Old_Path, Value : in String ) is
			use Ada.Text_IO;
			File		: File_Type;
		begin
			if Ada.Directories.Exists( Destination_Path ) then
				Ada.Directories.Delete_File( Destination_Path );
			end if;

			if Old_Path /= "" and then Ada.Directories.Exists( Old_Path ) then
				Ada.Directories.Delete_File( Old_Path );
			end if;

			Create( File, Out_File, Destination_Path );
			Put( File, Value );
			Close( File );
		end Write;

		procedure Read( The_path : in String; Buffer : out Unbounded_String ) is
			use Ada.Text_IO;
			File	: File_Type;
			First	: Boolean := True;
		begin

			Open( File, In_File, The_Path );

			while not End_Of_File( File ) loop
				if First then
					First := False;
				else
					Append( Buffer, Ada.Characters.Latin_1.LF );
				end if;
				Append( Buffer, Get_Line( File ) );
			end loop;

			Close( File );

		end Read;
	end ;

	overriding
	procedure Set_Property(
				Property	: in     Rich_Text_File_Property_Type;
				Entity		: in out Entity_Type'Class;
				Value		: in     String
			) is
		Destination_Path : constant String := Compute_Path( Property, Entity );
		Old_Path	 : constant String := To_String( Property.Getter.all( Entity ) );

	begin		
		File_Lock.Write( Destination_Path, Old_Path, Value );

		Property.Setter.all( Entity, To_Unbounded_String( Destination_Path ) );
	end Set_Property;



	overriding
	function Get_Property(
				Property	: in     Rich_Text_File_Property_Type;
				Entity		: in     Entity_Type'Class
			) return String is
		-- get the file contents
		The_Path : constant String := To_String( Property.Getter.all( Entity ) );

		use Ada.Directories;
		Buffer : Unbounded_String;
	begin
		if KOW_Ent.Is_New( Entity ) then
			-- new entities have no path
			return "";
		end if;

		if The_Path = "" or else not Exists( The_Path ) or else Kind( The_Path ) /= Ordinary_File then
			raise CONSTRAINT_ERROR with "inconsistency in the database detected in the rich text file property!";
		end if;

		File_Lock.Read( The_Path, Buffer );
		return To_String( Buffer );
	end Get_Property;

	function Compute_Path(
				Property	: in Rich_Text_File_Property_Type;
				Entity		: in Entity_Type'Class
			) return String is
		-- compute the path where to store the file
		use KOW_Lib.File_System;

		Extension : constant String := "html";
	begin
		if not KOW_Ent.Is_New( Entity ) then
			-- if we are updating the entity, we have the id... so we save it where we want it
			return To_String( Property.Store_Path ) / 
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
						R : constant String := To_String( Property.Store_Path ) / 
							"tmp_" & Rnd_Str & '_' & To_String( Property.Column_Name ) & '.' & Extension;
					begin
						if not Ada.Directories.Exists( R ) then
							return R;
						end if;
					end;
				end loop;
			end;
		end if;
	end Compute_Path;

	function New_Rich_Text_File_Property(
				Column_Name	: in     String;
				Getter		: KOW_Ent.Properties.UString_Getter_Callback;
				Setter		: KOW_Ent.Properties.UString_Setter_Callback;
				Store_Path	: in     String;				-- where to store files
				Default_Value	: in     String := "<p></p>";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150			-- here it refers to the filename length, which depends on the store_path
			) return Entity_Property_Ptr is
		use Ada.Directories;

		Rich : Rich_Text_File_Property_Type;
	begin
		if not Ada.Directories.Exists( Store_Path ) then
			Ada.Directories.Create_Path( Store_Path );
		elsif Ada.Directories.Kind( Store_Path ) /= Directory then
			raise Name_Error with "[" & Column_name & "] store path must be a directory!";
		end if;

		Rich.Column_Name	:= To_Unbounded_String( Column_Name );
		Rich.Getter		:= Getter;
		Rich.Setter		:= Setter;
		Rich.Store_Path		:= To_Unbounded_String( Store_Path );
		Rich.Default_Value	:= To_Unbounded_String( Default_Value );
		Rich.Immutable		:= Immutable;
		Rich.Length		:= Length;

		return new Rich_Text_File_Property_Type'( Rich );
	end New_Rich_Text_File_Property;




	-------------------------------
	-- File Upload Property Type --
	-------------------------------


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
			use KOW_Lib.File_System;

			Extension : constant String := Ada.Characters.Handling.To_Lower( Ada.Directories.Extension( Value ) );
		begin
			if not KOW_Ent.Is_New( Entity ) then
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
				KOW_View.Entities.Validation.Raise_Exception(
						Column	=> To_String( Property.Column_Name ),
						Message	=> "invalid file type.. please submit one of the following: " & KOW_Lib.String_Util.Implode( ',', Property.File_Types )
					);
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
				Big		: in     String := "720x720";
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
		UStr.Big		:= To_Unbounded_String( Big );
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
		

		--
		-- generate big..
		--

		declare
			P_Thumbnail	: aliased String := "-thumbnail";
			P_Thumbpath	: aliased String := To_String( Property.Big );
			P_Property	: aliased String := Get_Property( Property, Entity );
			P_Thumbname	: aliased String := Big_Name( Get_Property( Property, Entity ) );

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




end KOW_View.Entities.Properties;
