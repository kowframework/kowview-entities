
with ada.text_io;use ada.text_io;


-- Package with helper methods for Aw_View.Entities

--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

---------------
-- Ada Works --
---------------
with Aw_Ent;
with Aw_Ent.Properties;
with Aw_Lib.Locales;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;
with Templates_Parser;			use Templates_Parser;


package body Aw_View.Entities_Helper is


	----------------------
	-- Helper Functions --
	----------------------
	procedure Dump_Params( Data : in AWS.Status.Data ) is
		use AWS.Parameters;
		Params	: AWS.Parameters.List := AWS.Status.Parameters( Data );
	begin
		Put_Line( "Putting params.." );
		for i in 1 .. name_count( params ) loop
			declare
				name : string := get_name( params, i );
			begin
				Put_Line( name & " => " & get( params, name ) );
			end;
		end loop;
	end Dump_Params;




	function Assoc_Label(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale	:= Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- associate the label of this entity type to variabl_name
		Label : Unbounded_String := Aw_Ent.Get_Label( Entity, Locale );
	begin
		return Templates_Parser.Assoc( Variable_Name, Label );
	end Assoc_Label;



	function Assoc_Labels(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
		Labels_Tag	: Templates_Parser.Tag;
		Properties	: Aw_Ent.Property_Lists.List;

		procedure Iterator( C : Aw_Ent.Property_Lists.Cursor ) is
			P 	: Aw_Ent.Entity_Property_Ptr;
			Label	: Unbounded_String;
		begin
			P := Aw_Ent.Property_Lists.Element( C );
			Label := Aw_Ent.Get_Label( Entity, P.Column_Name, Locale );
			Labels_Tag := Labels_Tag & Label;
		end Iterator;
	begin
		Properties := Aw_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		Aw_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Labels_Tag );
	end Assoc_Labels;



	function Assoc_Resolved_Labels(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
		-- if the property is a foreign key, get the label for the related entity instead of the property's label
		Labels_Tag	: Templates_Parser.Tag;
		Properties	: Aw_Ent.Property_Lists.List;

		procedure Iterator( C : Aw_Ent.Property_Lists.Cursor ) is
			P 	: Aw_Ent.Entity_Property_Ptr;
			Label	: Unbounded_String;
		begin
			P := Aw_Ent.Property_Lists.Element( C );

			if P.all in Aw_Ent.Properties.Foreign_Key_Property_Type'Class then
				declare
					PP : Aw_Ent.Properties.Foreign_Key_Property_Type'Class :=
							Aw_Ent.Properties.Foreign_Key_Property_Type'Class( P.all );
					Related_Entity: Aw_Ent.Entity_Type'Class := Aw_Ent.New_Entity(
										PP.Related_Entity_Tag
									);
				begin
					Aw_Ent.Load(
							Related_Entity,
							Aw_Ent.To_ID(
								Natural'Value(
									Aw_Ent.Get_Property( P.all, Entity )
								)
							)
						);
					Label := Aw_Ent.Get_Label( Related_Entity, Locale );
				end;

			else
				Label := Aw_Ent.Get_Label( Entity, P.Column_Name, Locale );
			end if;
			Labels_Tag := Labels_Tag & Label;
		end Iterator;
	begin
		Properties := Aw_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		Aw_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Labels_Tag );
	end Assoc_Resolved_Labels;





	function Assoc_Values(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
		Values_Tag	: Templates_Parser.Tag;
		Properties	: Aw_Ent.Property_Lists.List;

		procedure Iterator( C : Aw_Ent.Property_Lists.Cursor ) is
			P : Aw_Ent.Entity_Property_Ptr;
		begin
			P := Aw_Ent.Property_Lists.Element( C );
			Values_Tag := Values_Tag & Aw_Ent.Get_Property( P.all, Entity );
		end Iterator;
	begin
		Properties := Aw_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		Aw_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Values_Tag );
	end Assoc_Values;


	function Assoc_Resolved_Values(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class;
			Locale		: in Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
		Values_Tag	: Templates_Parser.Tag;
		Properties	: Aw_Ent.Property_Lists.List;

		procedure Iterator( C : Aw_Ent.Property_Lists.Cursor ) is
			P : Aw_Ent.Entity_Property_Ptr;
		begin
			P := Aw_Ent.Property_Lists.Element( C );

			if P.all in Aw_Ent.Properties.Foreign_Key_Property_Type'Class then
				declare
					PP : Aw_Ent.Properties.Foreign_Key_Property_Type'Class :=
							Aw_Ent.Properties.Foreign_Key_Property_Type'Class( P.all );
					Related_Entity: Aw_Ent.Entity_Type'Class := Aw_Ent.New_Entity(
										PP.Related_Entity_Tag
									);
				begin
					Aw_Ent.Load(
							Related_Entity,
							Aw_Ent.To_ID(
								Natural'Value(
									Aw_Ent.Get_Property( P.all, Entity )
								)
							)
						);
					Values_Tag := Values_Tag & Aw_Ent.To_String( Related_Entity );
				end;
			else
				Values_Tag := Values_Tag & Aw_Ent.Get_Property( P.all, Entity );
			end if;
		end Iterator;
	begin
		Properties := Aw_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		Aw_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Values_Tag );
	end Assoc_Resolved_Values;


	function Assoc_Id(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class
		) return Templates_Parser.Association is
		-- associate the ID for this entity as string.

		function Entity_ID return String is
			use Ada.Tags;
			The_Tag : String := Ada.Tags.Expanded_Name( Entity'Tag );
		begin
			The_Tag := Ada.Characters.Handling.To_Lower( The_Tag );
			if Entity.Id.My_Tag = No_Tag then
				return "";
			else
				return Aw_Ent.To_String( Entity.Id );
			end if;
		end Entity_ID;
	begin
		return Templates_Parser.Assoc( Variable_Name, Entity_ID );
	end Assoc_Id;

	function Assoc_column_ids(
			Variable_Name	: in String;
			Entity		: in Aw_Ent.Entity_Type'Class
		) return Templates_Parser.Association is
		-- create a Tag inside with all Ids (ordered by the entity's registry) as string
		Ids_Tag	: Templates_Parser.Tag;
		Properties	: Aw_Ent.Property_Lists.List;

		procedure Iterator( C : Aw_Ent.Property_Lists.Cursor ) is
			P 	: Aw_Ent.Entity_Property_Ptr;
			T	: constant Unbounded_String := To_Unbounded_String(
						Ada.Characters.Handling.To_Lower(
							Ada.Tags.Expanded_Name( Entity'Tag )
						)
					);
			Key	: Unbounded_String;
		begin
			P := Aw_Ent.Property_Lists.Element( C );
			Key := T & "__" & P.Column_Name;
			Ids_Tag := Ids_Tag & Key;
		end Iterator;
	begin
		Properties := Aw_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		Aw_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Ids_Tag );
	end Assoc_column_ids;



	procedure Insert(
			Set		: in out Templates_Parser.Translate_Set;
			Variable_Prefix	: in     String;
			Entity		: in     Aw_Ent.Entity_Type'Class;
			Locale		: in     Aw_Lib.Locales.Locale := Aw_Lib.Locales.Default_Locale
		) is
		-- call all Assoc_* functions inserting the results in the translated set.
		-- create the associations :
		-- 	[P]_values
		-- 	[P]_labels
		-- 	[P]_column_ids
		-- Where [P] is the value for Variable_Prefix


		P : String renames Variable_Prefix;
		The_Tag : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity'Tag )
					);
		use Templates_Parser;
	begin
		Insert( Set, Templates_Parser.Assoc( P & "_tag", The_Tag ) );
		Insert( Set, Assoc_Id( P & "_id", Entity ) );
		Insert( Set, Assoc_column_ids( P & "_column_ids", Entity ) );
		Insert( Set, Assoc_Label( P & "_label", Entity, Locale ) );
		Insert( Set, Assoc_Labels( P & "_labels", Entity, Locale ) );
		Insert( Set, Assoc_Resolved_Labels( P & "_resolved_labels", Entity, Locale ) );
		Insert( Set, Assoc_Values( P & "_values", Entity, Locale ) );
		Insert( Set, Assoc_Resolved_Values( P & "_resolved_values", Entity, Locale ) );
	end Insert;


	procedure Load(
			Data		: in     AWS.Status.Data;
			Variable_Prefix	: in     String;
			Entity		: in out Aw_Ent.Entity_Type'Class
		) is
	-- read the data from a FORM returning a new entity to be stored/loaded/whateveroaded
	-- 	[P_][TAG]__id		=> the ID for this entity (if available)
	-- 	[P_][TAG]__column_ids	=> the value for the given column
	-- where P == the Variable_Prefix value.
	--
	-- and
	--
	-- [P_] = "" if P = ""
		function Set_Prefix return String is
			The_Tag : String := Ada.Characters.Handling.To_Lower(
							Ada.Tags.Expanded_Name( Entity'Tag )
						);
		begin
			if Variable_Prefix = "" then
				return The_Tag & "__";
			else
				return Variable_Prefix & '_' & The_Tag & "__";
			end if;
		end Set_Prefix;

		P	: String := Set_Prefix;
		Params	: AWS.Parameters.List := AWS.Status.Parameters( Data );

		Properties	: Aw_Ent.Property_Lists.List;

		procedure Iterator( C : Aw_Ent.Property_Lists.Cursor ) is
			Prop : Aw_Ent.Entity_Property_Ptr;


			function Param_ID return String is
			begin
				return P & To_String( Prop.Column_Name );
			end Param_ID;

		begin
			Prop := Aw_Ent.Property_Lists.Element( C );
			Aw_Ent.Set_Property( Prop.all, Entity, AWS.Parameters.Get( Params, Param_ID ) );
		end Iterator;
	begin
		Properties := Aw_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		Aw_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
	end Load;



	function Do_Load(
			Data		: in AWS.Status.Data;
			Variable_Prefix	: in String
		) return Aw_Ent.Entity_Type'Class is
		-- The same as the Load procedure, but create and load the entity from the database if it's set
		-- read the data from:
		-- 	[P_]entity_tag		=> the tag for this entity
		--
		-- and the others that the Load procedure read

		function Set_Prefix return String is
		begin
			if Variable_Prefix = "" then
				return "";
			else
				return Variable_Prefix & '_';
			end if;
		end Set_Prefix;

		P	: String := Set_Prefix;
		Params	: AWS.Parameters.List := AWS.Status.Parameters( Data );
		
		The_Tag : String := AWS.Parameters.Get( Params, P & "tag" );
		Entity	: Aw_Ent.Entity_Type'Class := Aw_Ent.New_Entity( To_Unbounded_String( The_Tag ) );
		Id	: String := AWS.Parameters.Get( Params, P & The_Tag & "__id" );
	begin
		if Id /= "" then
		 	Aw_Ent.Load( Entity, Aw_Ent.To_Id( Natural'Value( Id ), Entity'Tag  ) );
		end if;

		Load( Data, Variable_Prefix, Entity );
		return Entity;
	end Do_Load;

	function Load(
			Data		: in AWS.Status.Data;
			Variable_Prefix	: in String
		) return Aw_Ent.Entity_Type'Class is
	begin
		--Dump_Params( Data );
		return Do_Load( Data, Variable_Prefix );
	end Load;



end Aw_View.Entities_Helper;

