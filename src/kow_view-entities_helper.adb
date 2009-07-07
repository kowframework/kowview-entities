
with ada.text_io;use ada.text_io;


-- Package with helper methods for KOW_View.Entities

--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

---------------
-- Ada Works --
---------------
with KOW_Ent;
with KOW_Ent.Properties;
with KOW_Lib.Locales;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;
with Templates_Parser;			use Templates_Parser;


package body KOW_View.Entities_Helper is


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
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale	:= KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- associate the label of this entity type to variabl_name
		Label : Unbounded_String := KOW_Ent.Get_Label( Entity, Locale );
	begin
		return Templates_Parser.Assoc( Variable_Name, Label );
	end Assoc_Label;



	function Assoc_Labels(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
		Labels_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P 	: KOW_Ent.Entity_Property_Ptr;
			Label	: Unbounded_String;
		begin
			P := KOW_Ent.Property_Lists.Element( C );
			Label := KOW_Ent.Get_Label( Entity, P.Column_Name, Locale );
			Labels_Tag := Labels_Tag & Label;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Labels_Tag );
	end Assoc_Labels;



	function Assoc_Resolved_Labels(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
		-- if the property is a foreign key, get the label for the related entity instead of the property's label
		Labels_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P 	: KOW_Ent.Entity_Property_Ptr;
			Label	: Unbounded_String;
		begin
			P := KOW_Ent.Property_Lists.Element( C );

			if P.all in KOW_Ent.Properties.Foreign_Key_Property_Type'Class then
				declare
					PP : KOW_Ent.Properties.Foreign_Key_Property_Type'Class :=
							KOW_Ent.Properties.Foreign_Key_Property_Type'Class( P.all );
					Related_Entity: KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity(
										PP.Related_Entity_Tag
									);
				begin
					KOW_Ent.Load(
							Related_Entity,
							KOW_Ent.To_ID(
								Natural'Value(
									KOW_Ent.Get_Property( P.all, Entity )
								)
							)
						);
					Label := KOW_Ent.Get_Label( Related_Entity, Locale );
				end;

			else
				Label := KOW_Ent.Get_Label( Entity, P.Column_Name, Locale );
			end if;
			Labels_Tag := Labels_Tag & Label;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Labels_Tag );
	end Assoc_Resolved_Labels;





	function Assoc_Values(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
		Values_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P : KOW_Ent.Entity_Property_Ptr;
		begin
			P := KOW_Ent.Property_Lists.Element( C );
			Values_Tag := Values_Tag & KOW_Ent.Get_Property( P.all, Entity );
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Values_Tag );
	end Assoc_Values;


	function Assoc_Resolved_Values(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
		Values_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P : KOW_Ent.Entity_Property_Ptr;
		begin
			P := KOW_Ent.Property_Lists.Element( C );

			if P.all in KOW_Ent.Properties.Boolean_Property_Type'Class then
				declare
					PP : KOW_Ent.Properties.Boolean_Property_Type'Class :=
							KOW_Ent.Properties.Boolean_Property_Type'Class( P.all );
				begin
					if PP.Getter( Entity ) then
						Values_Tag := Values_Tag & "<img src=""/themes/true.png"" alt=""true""/>";
					else
						Values_Tag := Values_Tag & "<img src=""/themes/false.png"" alt=""false""/>";
					end if;
				end;
			elsif P.all in KOW_Ent.Properties.Foreign_Key_Property_Type'Class then
				declare
					PP : KOW_Ent.Properties.Foreign_Key_Property_Type'Class :=
							KOW_Ent.Properties.Foreign_Key_Property_Type'Class( P.all );
					Related_Entity: KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity(
										PP.Related_Entity_Tag
									);
				begin
					KOW_Ent.Load(
							Related_Entity,
							KOW_Ent.To_ID(
								Natural'Value(
									KOW_Ent.Get_Property( P.all, Entity )
								)
							)
						);
					Values_Tag := Values_Tag & KOW_Ent.To_String( Related_Entity );
				end;
			elsif P.all in KOW_Ent.Properties.Locale_Property_Type'Class then
				declare
					PP : KOW_Ent.Properties.Locale_property_Type'Class :=
							KOW_Ent.Properties.Locale_Property_Type'Class( P.all );
					Locale : KOW_Lib.Locales.Locale := PP.Getter.all( Entity );
				begin
					Values_Tag := Values_Tag & Locale.Name;
				end;
			else
				Values_Tag := Values_Tag & KOW_Ent.Get_Property( P.all, Entity );
			end if;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Values_Tag );
	end Assoc_Resolved_Values;





	function Assoc_Form_Elements(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Name_Prefix	: in String := "entity"
		) return Templates_Parser.Association is
		-- create a Tag inside with the corresponding Form element for each entity property.
		-- currently it supports:
		-- 	string (default)
		-- 	locale
	
		Elements_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;
		Pref		: constant Unbounded_String := To_Unbounded_String( Name_Prefix & '_' );




		function Form_Element(
					Name	: in Unbounded_String;
					P	: KOW_Ent.Entity_Property_Type'Class
				) return Unbounded_String is


			String_Value : String := KOW_Ent.Get_Property( P, Entity );
	
			Ret : Unbounded_String;

			function T( Str : in String ) return Unbounded_String renames To_Unbounded_String;



			procedure Foreign_Key_Iterator( Entity : in KOW_Ent.Entity_Type'Class ) is
				use KOW_Ent;
				
				ID : Id_Type := KOW_Ent.To_ID( Natural'Value( String_Value ) );
				String_Id : String := KOW_Ent.To_String( Entity.Id );

			begin
				Ret := Ret & T( "<option value=""" );
				Ret := Ret & T( String_ID );
				Ret := Ret & T( """" );
				ID.My_Tag := Entity'Tag;
				if ID = Entity.Id then
					Ret := Ret & T( " selected=""1""" );
				end if;
				Ret := Ret & T( ">" );
				Ret := Ret & T( KOW_Ent.To_String( Entity ) );
				Ret := Ret & T( "</option>" );
			end Foreign_Key_Iterator;



			procedure Locale_Iterator( C: in KOW_Lib.Locales.Locale_Tables.Cursor ) is
				use KOW_Lib.Locales.Locale_Tables;
			begin

				if Element( C ).Auto_Generalized then
					-- we want to avoid authomatically generalized elements
					-- so there are no dupplicated elements in the option list
					return;
				end if;

				Ret := Ret & T( "<option value=""" );
				Ret := Ret & Key( C );
				Ret := Ret & T( """" );
				if String_Value = Key ( C ) then
					Ret := Ret & "selected=""1""";
				end if;
				Ret := Ret & T( ">" );
				Ret := Ret & Element( C ).Name;
				Ret := Ret & T( "</option>" );
			end Locale_Iterator;



		begin
			if P in KOW_Ent.Properties.Boolean_Property_Type'Class then
				Ret := T( "<div onClick=""trueFalseMe(this)"">" );
				Ret := Ret & T( "<input type=""hidden"" name=""") & Name & T( """ " );

				if KOW_Ent.Properties.Boolean_Property_Type'Class( P ).Getter.all( Entity ) then
					Ret := Ret & T( "value=""true""/>" );
					Ret := Ret & T( "<img src=""/themes/true.png""/>" );
				else
					Ret := Ret & T( "value=""false""/>" );
					Ret := Ret & T( "<img src=""/themes/false.png""/>" );
				end if;
				Ret := Ret & T( "</div>" );
			elsif P in KOW_Ent.Properties.Foreign_Key_Property_Type'Class then
				Ret := T( "<select name=""" );
				Ret := Ret & Name;
				Ret := Ret & T( """>" );

				declare
					PP : KOW_Ent.Properties.Foreign_Key_Property_Type'Class :=
						KOW_Ent.Properties.Foreign_Key_Property_Type'Class( P );
					Entity  : KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity( PP.Related_Entity_Tag );
					All_Ids : KOW_Ent.Id_Array_Type := KOW_Ent.Get_All_Ids( PP.Related_Entity_Tag );
				begin
					for i in All_Ids'First .. All_Ids'Last loop
						KOW_Ent.Load( Entity, All_Ids( i ) );
						Foreign_Key_Iterator( Entity );
					end loop;
				end;

				Ret := Ret & T("</select>" );
			elsif P in KOW_Ent.Properties.Locale_property_type'Class then
				Ret := T( "<select name=""" );
				Ret := Ret & Name;
				Ret := Ret & T( """>" );

				KOW_Lib.Locales.Locale_Tables.Iterate(
						KOW_Lib.Locales.Supported_Locales,
						Locale_Iterator'Access
					);

				Ret := Ret & T("</select>" );


			else
				Ret := To_Unbounded_String( "<input type=""text"" name=""" );
				Ret := Ret & Name;
				Ret := Ret & To_Unbounded_String(
							""" value=""" & String_Value & """ />"
						);
			end if;

			return Ret;
		end Form_Element;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P 	: KOW_Ent.Entity_Property_Ptr;
			T	: constant Unbounded_String := To_Unbounded_String(
						Ada.Characters.Handling.To_Lower(
							Ada.Tags.Expanded_Name( Entity'Tag )
						)
					);
			Name	: Unbounded_String;
		begin
			P	:= KOW_Ent.Property_Lists.Element( C );
			Name	:= Pref & T & "__" & P.Column_Name;

			


			Elements_Tag := Elements_Tag & Form_Element( Name, P.all );
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Elements_Tag );

	end Assoc_Form_Elements;







	function Assoc_Id(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class
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
				return KOW_Ent.To_String( Entity.Id );
			end if;
		end Entity_ID;
	begin
		return Templates_Parser.Assoc( Variable_Name, Entity_ID );
	end Assoc_Id;

	function Assoc_column_ids(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class
		) return Templates_Parser.Association is
		-- create a Tag inside with all Ids (ordered by the entity's registry) as string
		Ids_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P 	: KOW_Ent.Entity_Property_Ptr;
			T	: constant Unbounded_String := To_Unbounded_String(
						Ada.Characters.Handling.To_Lower(
							Ada.Tags.Expanded_Name( Entity'Tag )
						)
					);
			Key	: Unbounded_String;
		begin
			P := KOW_Ent.Property_Lists.Element( C );
			Key := T & "__" & P.Column_Name;
			Ids_Tag := Ids_Tag & Key;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Templates_Parser.Assoc( Variable_Name, Ids_Tag );
	end Assoc_column_ids;



	procedure Insert(
			Set		: in out Templates_Parser.Translate_Set;
			Variable_Prefix	: in     String;
			Entity		: in     KOW_Ent.Entity_Type'Class;
			Locale		: in     KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Include_Form	: in     Boolean := False
		) is
		-- call all Assoc_* functions inserting the results in the translated set.
		-- create the associations :
		-- 	[P]_values
		-- 	[P]_labels
		-- 	[P]_column_ids
		-- 	[P]_form_element
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
		
		if Include_Form then
			Insert( Set, Assoc_Form_Elements( P & "_form_elements", Entity, Locale ) );
		end if;
	end Insert;


	procedure Load(
			Data		: in     AWS.Status.Data;
			Variable_Prefix	: in     String;
			Entity		: in out KOW_Ent.Entity_Type'Class
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

		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			Prop : KOW_Ent.Entity_Property_Ptr;


			function Param_ID return String is
			begin
				return P & To_String( Prop.Column_Name );
			end Param_ID;

		begin
			Prop := KOW_Ent.Property_Lists.Element( C );
			KOW_Ent.Set_Property( Prop.all, Entity, AWS.Parameters.Get( Params, Param_ID ) );
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
	end Load;



	function Do_Load(
			Data		: in AWS.Status.Data;
			Variable_Prefix	: in String
		) return KOW_Ent.Entity_Type'Class is
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
		Entity	: KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity( To_Unbounded_String( The_Tag ) );
		Id	: String := AWS.Parameters.Get( Params, P & The_Tag & "__id" );
	begin
		if Id /= "" then
		 	KOW_Ent.Load( Entity, KOW_Ent.To_Id( Natural'Value( Id ), Entity'Tag  ) );
		end if;

		Load( Data, Variable_Prefix, Entity );
		return Entity;
	end Do_Load;

	function Load(
			Data		: in AWS.Status.Data;
			Variable_Prefix	: in String
		) return KOW_Ent.Entity_Type'Class is
	begin
		--Dump_Params( Data );
		return Do_Load( Data, Variable_Prefix );
	end Load;



end KOW_View.Entities_Helper;

