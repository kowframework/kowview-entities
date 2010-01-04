
with ada.text_io;use ada.text_io;


-- Package with helper methods for KOW_View.Entities

--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

--------------------
-- KOW Frameworks --
--------------------
with APQ;
with KOW_Ent;
with KOW_Ent.Properties;
with KOW_Lib.Locales;
with KOW_Lib.Log;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;
with Templates_Parser;			use Templates_Parser;


package body KOW_View.Entities_Helper is


	Logger : KOW_Lib.Log.Logger_Type := KOW_Lib.Log.Get_Logger( "KOW_View.Entities_Helper" );

	procedure Log( Message : in String; Level : in KOW_Lib.Log.Log_Level := KOW_Lib.Log.Level_Debug ) is
	begin
		KOW_Lib.Log.Log(
				Logger	=> Logger,
				Level	=> Level,
				Message	=> Message
			);
	end Log;




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




	function Should_Ignore( P : in KOW_Ent.Entity_Property_Ptr; Related_Entity_Tag : in String ) return Boolean is
		use KOW_Ent;
		use KOW_Ent.Properties;
	begin
		if P = null then
			return false;
		elsif P.all not in Foreign_Key_Property_Type'Class then
			return false;
		else
			Log( "Comparing " & Ada.Tags.Expanded_Name( Foreign_Key_Property_Type( P.all ).Related_Entity_Tag ) & " with " & Related_Entity_Tag);
			return Ada.Tags.Expanded_Name( Foreign_Key_Property_Type( P.all ).Related_Entity_Tag ) = Related_Entity_Tag;
		end if;
	end SHould_Ignore;




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



	----------------
	-- The Labels --
	----------------


	function Get_Labels_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := ""
			) return Templates_Parser.Tag is
		Labels_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P 	: KOW_Ent.Entity_Property_Ptr;
			Label	: Unbounded_String;
		begin
			P := KOW_Ent.Property_Lists.Element( C );
			if not Should_Ignore( P, Ignore_Relation ) then
				Label := KOW_Ent.Get_Label( Entity, P.Column_Name, Locale );
				Labels_Tag := Labels_Tag & Label;
			end if;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );

		return Labels_Tag;
	end Get_Labels_Tag;



	function Assoc_Labels(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Labels_Tag( Entity, Locale ) );
	end Assoc_Labels;




	-------------------------
	-- The resolved Labels --
	-------------------------
	
	function Get_Resolved_Labels_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := ""
		) return Templates_Parser.Tag is
		Labels_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P 	: KOW_Ent.Entity_Property_Ptr;
			Label	: Unbounded_String;
		begin
			P := KOW_Ent.Property_Lists.Element( C );

			if not Should_Ignore( P, Ignore_Relation ) then 
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
			end if;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Labels_Tag;
	exception
		when APQ.NO_TUPLE => return Labels_Tag;
	end Get_Resolved_Labels_Tag;



	function Assoc_Resolved_Labels(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
		-- if the property is a foreign key, get the label for the related entity instead of the property's label
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Resolved_Labels_Tag( Entity, Locale ) );
	end Assoc_Resolved_Labels;



	----------------
	-- The Values --
	----------------

	function Get_Values_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := ""
		) return Templates_Parser.Tag is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
		Values_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P : KOW_Ent.Entity_Property_Ptr;
		begin
			if not Should_Ignore( P, Ignore_Relation ) then
				P := KOW_Ent.Property_Lists.Element( C );
				Values_Tag := Values_Tag & KOW_Ent.Get_Property( P.all, Entity );
			end if;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Values_Tag;
	end Get_Values_Tag;




	function Assoc_Values(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Values_Tag( Entity, Locale ) );
	end Assoc_Values;




	-------------------------
	-- The Resolved Values --
	-------------------------

	function Get_Resolved_Values_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := ""
		) return Templates_Parser.Tag is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
		-- if the type is a Foreign Key, get not the ID but the label for this single related entity
		Values_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P : KOW_Ent.Entity_Property_Ptr;
		begin
			P := KOW_Ent.Property_Lists.Element( C );

			if Should_Ignore( P, Ignore_Relation ) then
				return;
			end if;

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
		return Values_Tag;
	exception
		when APQ.NO_TUPLE => return Values_Tag;
	end Get_Resolved_Values_Tag;




	function Assoc_Resolved_Values(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
		) return Templates_Parser.Association is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Resolved_Values_Tag( Entity, Locale ) );
	end Assoc_Resolved_Values;


	-----------------------
	-- The Form Elements --
	-----------------------
	
	
	function Get_Form_Elements_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Name_Prefix	: in String;
			Form_Mode	: in Form_Mode_Type;
			Ignore_Relation	: in String := ""
		) return Templates_Parser.Tag is
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


			function Disabled_Enabled( Entity : in KOW_Ent.Entity_Property_Type'CLass ) return String is
			begin
				if Form_Mode = Edit AND THEN P.Immutable then
					Log( "Field as disabled.." );
					return " disabled ";
				else
					Log( "Field as enabled.." );
					return "";
				end if;
			end Disabled_Enabled;


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
				if Form_Mode = Edit and then P.Immutable then
					Ret := T( "<div>" );
				else
					Ret := T( "<div onClick=""trueFalseMe(this)"">" );
				end if;

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
				Ret := T( "<select dojoType=""dijit.form.FilteringSelect"" name=""" );
				Ret := Ret & Name & """";
				Ret := Ret & T( Disabled_Enabled( P ) );
				Ret := Ret & T( ">" );

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
				Ret := T( "<select dojoType=""dijit.form.FilteringSelect"" name=""" );
				Ret := Ret & Name & """";
				Ret := Ret & T( Disabled_Enabled( P ) );
				Ret := Ret & T( ">" );

				KOW_Lib.Locales.Locale_Tables.Iterate(
						KOW_Lib.Locales.Supported_Locales,
						Locale_Iterator'Access
					);

				Ret := Ret & T("</select>" );

			elsif P in KOW_Ent.Properties.Password_Property_Type'Class then
				Ret := To_Unbounded_String( "<input type=""password"" name=""" );
				Ret := Ret & Name;
				Ret := Ret & To_Unbounded_String(
							""" value=""" & String_Value & """"
						);

				Ret := Ret & T( Disabled_Enabled( P ) & "/>");

			else
				Ret := To_Unbounded_String( "<input type=""text"" name=""" );
				Ret := Ret & Name;
				Ret := Ret & To_Unbounded_String(
							""" value=""" & String_Value & """"
						);
				if P in KOW_Ent.Properties.UString_Property_Type'Class then
					Ret := Ret & To_Unbounded_String( " maxlength=""" & Integer'Image(
								KOW_Ent.Properties.UString_Property_Type(P).Length
							) & """" );
				end if;
				Ret := Ret & T( Disabled_Enabled( P ) & "/>");
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

			if Should_Ignore( P, Ignore_Relation ) then
				return;
			end if;

			Name	:= Pref & T & "__" & P.Column_Name;

			


			Elements_Tag := Elements_Tag & Form_Element( Name, P.all );
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );

		return Elements_Tag;
	end Get_Form_Elements_Tag;




	function Assoc_Form_Elements(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Name_Prefix	: in String;
			Form_Mode	: in Form_Mode_Type
		) return Templates_Parser.Association is
		-- create a Tag inside with the corresponding Form element for each entity property.
		-- currently it supports:
		-- 	string (default)
		-- 	locale
	begin	
		return Templates_Parser.Assoc(
					Variable_Name,
					Get_Form_Elements_Tag(
						Entity		=> Entity,
						Locale		=> Locale,
						Name_Prefix	=> Name_Prefix,
						Form_Mode	=> Form_Mode
					)
				);
	end Assoc_Form_Elements;



	---------------------------
	-- The current entity ID --
	---------------------------


	function Get_ID( Entity : in KOW_Ent.Entity_Type'Class ) return String is
		use Ada.Tags;
		The_Tag : String := Ada.Tags.Expanded_Name( Entity'Tag );
	begin
		The_Tag := Ada.Characters.Handling.To_Lower( The_Tag );
		if Entity.Id.My_Tag = No_Tag then
			return "";
		else
			return KOW_Ent.To_String( Entity.Id );
		end if;
	end Get_ID;



	function Assoc_Id(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class
		) return Templates_Parser.Association is
		-- associate the ID for this entity as string.

	begin
		return Templates_Parser.Assoc( Variable_Name, Get_ID( Entity ) );
	end Assoc_Id;


	--------------------
	-- The Column IDs --
	--------------------
	

	function Get_column_ids_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Ignore_Relation	: in String := ""
		) return Templates_Parser.Tag is
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
			if Should_Ignore( P, Ignore_Relation ) then
				P := KOW_Ent.Property_Lists.Element( C );
				Key := T & "__" & P.Column_Name;
				Ids_Tag := Ids_Tag & Key;
			end if;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );

		return IDs_Tag;
	end Get_column_ids_Tag;



	function Assoc_column_ids(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class
		) return Templates_Parser.Association is
		-- create a Tag inside with all Ids (ordered by the entity's registry) as string
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Column_Ids_Tag( Entity ) );
	end Assoc_column_ids;



	procedure Insert(
			Set		: in out Templates_Parser.Translate_Set;
			Variable_Prefix	: in     String;
			Entity		: in     KOW_Ent.Entity_Type'Class;
			Locale		: in     KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Include_Form	: in     Boolean := False;
			Form_Mode	: in     Form_Mode_Type 
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
			Insert( Set, Assoc_Form_Elements(
						Variable_Name	=> P & "_form_elements",
						Entity		=> Entity,
						Locale		=> Locale,
						Name_Prefix	=> P,
						Form_Mode	=> Form_Mode
					)
				);
		end if;
	end Insert;









	procedure Insert_All(
			Set		: in out Templates_Parser.Translate_Set;
			Variable_Prefix	: in     String;
			Entity_Tags	: in     KOW_Lib.UString_Vectors.Vector;
			Locale		: in     KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Include_Form	: in     Boolean := False;
			Form_Mode	: in     Form_Mode_Type;
			Ignore_Relation	: in     String
		) is
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

		P : String renames Variable_Prefix;
		use Templates_Parser;


		Form_Ids		: Templates_Parser.Tag;

		Tags_Tag		: Templates_Parser.Tag;
		Ids_Tag			: Templates_Parser.Tag;
		Column_Ids_Tag		: Templates_Parser.Tag;
		Label_Tag		: Templates_Parser.Tag;
		Labels_Tag		: Templates_Parser.Tag;
		Resolved_Labels_Tag	: Templates_Parser.Tag;
		Values_Tag		: Templates_Parser.Tag;
		Resolved_Values_Tag	: Templates_Parser.Tag;
		Form_Elements_Tag	: Templates_Parser.Tag;

		
		procedure Create_Tags_Iterator( C : in KOW_Lib.UString_Vectors.Cursor ) is
			Entity : KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity(
							KOW_Lib.UString_Vectors.Element( C )
						);

			The_Tag : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity'Tag )
					);
			Form_ID	: String := P & '_' & The_Tag;
			The_Label : String := KOW_Ent.Get_Label( Entity, Locale );
		begin
			Form_IDs		:= Form_IDs		& Form_ID;
			Tags_Tag		:= Tags_Tag		& The_Tag;
			Ids_Tag			:= Ids_Tag		& Get_ID( Entity );
			Column_Ids_Tag		:= Column_Ids_Tag	& Get_Column_Ids_Tag( Entity, Ignore_Relation );
			Label_Tag		:= Label_Tag		& The_Label;
			Labels_Tag		:= Labels_Tag		& Get_Labels_Tag( Entity, Locale, Ignore_Relation );
			Resolved_Labels_Tag	:= Resolved_Labels_Tag	& Get_Resolved_Labels_Tag( Entity, Locale, Ignore_Relation );
			Values_Tag		:= Values_Tag		& Get_Values_Tag( Entity, Locale, Ignore_Relation );
			Resolved_Values_Tag	:= Resolved_Values_Tag	& Get_Resolved_Values_Tag( Entity, Locale, Ignore_Relation );

			if Include_Form then
				Form_Elements_Tag := Form_Elements_Tag & Get_Form_Elements_Tag(
									Entity		=> Entity,
									Locale		=> Locale,
									Name_Prefix	=> Variable_Prefix,
									Form_Mode	=> Form_Mode,
									Ignore_Relation => Ignore_Relation
								);
			end if;
		end Create_Tags_Iterator;


	
		procedure Edit_Tags_Iterator( C : in KOW_Lib.UString_Vectors.Cursor ) is
			Entity : KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity(
							KOW_Lib.UString_Vectors.Element( C )
						);

			The_Tag : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity'Tag )
					);
			Form_ID	: String := P & '_' & The_Tag;
			The_Label : String := KOW_Ent.Get_Label( Entity, Locale );

		begin
			Form_IDs		:= Form_IDs		& Form_ID;
			Tags_Tag		:= Tags_Tag		& The_Tag;
			Ids_Tag			:= Ids_Tag		& Get_ID( Entity );
			Column_Ids_Tag		:= Column_Ids_Tag	& Get_Column_Ids_Tag( Entity, Ignore_Relation );
			Label_Tag		:= Label_Tag		& The_Label;
			Labels_Tag		:= Labels_Tag		& Get_Labels_Tag( Entity, Locale, Ignore_Relation );
			Resolved_Labels_Tag	:= Resolved_Labels_Tag	& Get_Resolved_Labels_Tag( Entity, Locale, Ignore_Relation );
			Values_Tag		:= Values_Tag		& Get_Values_Tag( Entity, Locale, Ignore_Relation );
			Resolved_Values_Tag	:= Resolved_Values_Tag	& Get_Resolved_Values_Tag( Entity, Locale, Ignore_Relation );

			if Include_Form then
				Form_Elements_Tag := Form_Elements_Tag & Get_Form_Elements_Tag(
									Entity		=> Entity,
									Locale		=> Locale,
									Name_Prefix	=> Variable_Prefix,
									Form_Mode	=> Form_Mode,
									Ignore_Relation => Ignore_Relation
								);
			end if;
		end Edit_Tags_Iterator;

	begin
		case Form_Mode is
			when Create =>
				KOW_Lib.UString_Vectors.Iterate(
						Entity_Tags,
						Create_Tags_iterator'Access
					);
			when Edit =>
				KOW_Lib.UString_Vectors.Iterate(
						Entity_Tags,
						Edit_Tags_iterator'Access
					);
		end case;

		Insert( Set, Assoc( P & "_form_ids",		Form_IDs ) );
		Insert( Set, Assoc( P & "_tag",			Tags_Tag ) );
		Insert( Set, Assoc( P & "_id",			Ids_Tag ) );
		Insert( Set, Assoc( P & "_column_ids",		Column_Ids_Tag ) );
		Insert( Set, Assoc( P & "_label",		Label_Tag ) );
		Insert( Set, Assoc( P & "_labels",		Labels_Tag ) );
		Insert( Set, Assoc( P & "_resolved_labels",	Resolved_Labels_Tag ) );
		Insert( Set, Assoc( P & "_values",		Values_Tag ) );
		Insert( Set, Assoc( P & "_resolved_values",	Resolved_Values_Tag ) );
		
		if Include_Form then
			Insert( Set, Assoc( P & "_form_elements", Form_Elements_Tag ) );
		end if;
	end Insert_All;




	procedure Load(
			Data		: in     AWS.Status.Data;
			Variable_Prefix	: in     String;
			Entity		: in out KOW_Ent.Entity_Type'Class;
			N		: in     Positive
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
			Prop : KOW_Ent.Entity_Property_Ptr := KOW_Ent.Property_Lists.Element( C );


			function Param_ID return String is
			begin
				return P & To_String( Prop.Column_Name );
			end Param_ID;

			use KOW_Ent;
		begin
			Log( "Getting property.." );
			if Prop /= null then
				Log( "Iterating over " & Param_ID );
				KOW_Ent.Set_Property( Prop.all, Entity, AWS.Parameters.Get( Params, Param_ID, N ) );
			end if;
		exception
			when constraint_error => null;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
	end Load;



	function Do_Load(
			Data		: in AWS.Status.Data;
			Variable_Prefix	: in String;
			N		: in Positive
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
		
		The_Tag : String := AWS.Parameters.Get( Params, P & "tag", N );
		Entity	: KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity( To_Unbounded_String( The_Tag ) );
		Id	: String := AWS.Parameters.Get( Params, P & The_Tag & "__id", N );
	begin
		if Id /= "" then
		 	KOW_Ent.Load( Entity, KOW_Ent.To_Id( Natural'Value( Id ), Entity'Tag  ) );
		end if;

		Load( Data, Variable_Prefix, Entity, N );
		return Entity;
	end Do_Load;

	function Load(
			Data		: in AWS.Status.Data;
			Variable_Prefix	: in String;
			N		: in Positive			-- parameter N of AWS.Parameters.Get() method
		) return KOW_Ent.Entity_Type'Class is
	begin
		--Dump_Params( Data );
		return Do_Load( Data, Variable_Prefix, N );
	end Load;



end KOW_View.Entities_Helper;

