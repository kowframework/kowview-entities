
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
with KOW_View.Entity_Property_Renderers;	use KOW_View.Entity_Property_Renderers;


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




	function Should_Ignore( P : in KOW_Ent.Entity_Property_Ptr; Related_Entity_Tag : in String; Ignore : KOW_Lib.UString_Vectors.Vector ) return Boolean is
		use KOW_Ent;
		use KOW_Ent.Properties;
	begin
		if P = null then
			return false;
		elsif P.all not in Foreign_Key_Property_Type'Class then
			return false;
		else
			Log( "Comparing " & Ada.Tags.Expanded_Name( Foreign_Key_Property_Type( P.all ).Related_Entity_Tag ) & " with " & Related_Entity_Tag);
			if Ada.Tags.Expanded_Name( Foreign_Key_Property_Type( P.all ).Related_Entity_Tag ) = Related_Entity_Tag then
				return true;
			else
				return KOW_Lib.UString_Vectors.Contains( Ignore, P.Column_Name );
			end if;
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
			Ignore_Relation	: in String := "";
			Form_Pref	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
			) return Templates_Parser.Tag is
		Labels_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P 	: KOW_Ent.Entity_Property_Ptr;
			Label	: Unbounded_String;
		begin
			P := KOW_Ent.Property_Lists.Element( C );
			if not Should_Ignore( P, Ignore_Relation, Ignore ) then
				declare
					T	: constant String := Ada.Characters.Handling.To_Lower(
								Ada.Tags.Expanded_Name( Entity'Tag )
							);

					Name : String := Form_Pref & "_" & T & "__" & To_String( P.Column_Name );
				begin
					if Form_Pref /= "" then
						Label := To_Unbounded_String( "<label for=""" & Name & """>" );
						Label := Label & To_Unbounded_String( KOW_Ent.Get_Label( Entity, P.Column_Name, Locale ) );
						Label := Label & To_Unbounded_String( "</label>" );
					else
						Label := KOW_Ent.Get_Label( Entity, P.Column_Name, Locale );
					end if;
					Labels_Tag := Labels_Tag & Label;
				end;
			end if;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );

		return Labels_Tag;
	end Get_Labels_Tag;



	function Assoc_Labels(
			Variable_Name	: in String;
			Form_Pref	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association is
		-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Labels_Tag( Entity, Locale, Form_Pref => Form_Pref, Ignore => Ignore ) );
	end Assoc_Labels;




	-------------------------
	-- The resolved Labels --
	-------------------------
	
	function Get_Resolved_Labels_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag is
		Labels_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P 	: KOW_Ent.Entity_Property_Ptr;
			Label	: Unbounded_String;
		begin
			P := KOW_Ent.Property_Lists.Element( C );

			if not Should_Ignore( P, Ignore_Relation, Ignore ) then 
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
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Labels_Tag;
	exception
		when APQ.NO_TUPLE => return Labels_Tag;
	end Get_Resolved_Labels_Tag;



	function Assoc_Resolved_Labels(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association is
		-- create a Tag inside with all labels (ordered by the entity's registry) in formatted as string
		-- if the property is a foreign key, get the label for the related entity instead of the property's label
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Resolved_Labels_Tag( Entity, Locale, Ignore => Ignore ) );
	end Assoc_Resolved_Labels;



	----------------
	-- The Values --
	----------------

	function Get_Values_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
		Values_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P : KOW_Ent.Entity_Property_Ptr;
		begin

			if not Should_Ignore( P, Ignore_Relation, Ignore ) then
				P := KOW_Ent.Property_Lists.Element( C );
				declare
					R : KOW_View.Entity_Property_Renderers.Property_Renderer_Type'Class
							:= KOW_View.Entity_Property_Renderers.Registry.Get_Renderer(
									P.all'Tag
								);
					Result	: Unbounded_String;
				begin
					KOW_View.Entity_Property_Renderers.Render_View(
							Renderer	=> R,
							Entity		=> Entity,
							Property	=> P.all,
							Result		=> Result
						);
					Values_Tag := Values_Tag & Result;
				end;
			end if;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Values_Tag;
	end Get_Values_Tag;




	function Assoc_Values(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Values_Tag( Entity, Locale, Ignore => Ignore ) );
	end Assoc_Values;




	-------------------------
	-- The Resolved Values --
	-------------------------

	function Get_Resolved_Values_Tag(
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Tag is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
		-- if the type is a Foreign Key, get not the ID but the label for this single related entity
		Values_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;

		procedure Iterator( C : KOW_Ent.Property_Lists.Cursor ) is
			P : KOW_Ent.Entity_Property_Ptr;
		begin
			P := KOW_Ent.Property_Lists.Element( C );



			if not Should_Ignore( P, Ignore_Relation, Ignore ) then
				declare
					R : KOW_View.Entity_Property_Renderers.Property_Renderer_Type'Class
							:= KOW_View.Entity_Property_Renderers.Registry.Get_Renderer(
									P.all'Tag
								);
					Result	: Unbounded_String;
				begin
					KOW_View.Entity_Property_Renderers.Render_View(
							Renderer	=> R,
							Entity		=> Entity,
							Property	=> P.all,
							Result		=> Result
						);
					Values_Tag := Values_Tag & Result;
				end;
			end if;

		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		return Values_Tag;
	exception
		when APQ.NO_TUPLE => return Values_Tag;
	end Get_Resolved_Values_Tag;




	function Assoc_Resolved_Values(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association is
		-- create a Tag inside with all values (ordered by the entity's registry) in formatted as string
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Resolved_Values_Tag( Entity, Locale, Ignore => Ignore ) );
	end Assoc_Resolved_Values;


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
		) return Templates_Parser.Tag is
		-- create a Tag inside with the corresponding Form element for each entity property.
		-- currently it supports:
		-- 	string (default)
		-- 	locale

		Elements_Tag	: Templates_Parser.Tag;
		Properties	: KOW_Ent.Property_Lists.List;
		Pref		: constant Unbounded_String := To_Unbounded_String( Name_Prefix & '_' );





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

			if Should_Ignore( P, Ignore_Relation, Ignore ) then
				return;
			end if;

			Name	:= Pref & T & "__" & P.Column_Name;

			declare
				R : KOW_View.Entity_Property_Renderers.Property_Renderer_Type'Class
						:= KOW_View.Entity_Property_Renderers.Registry.Get_Renderer(
								P.all'Tag
							);
				Result	: Unbounded_String;
			begin
				KOW_View.Entity_Property_Renderers.Render_Form(
						Renderer	=> R,
						Entity		=> Entity,
						Property	=> P.all,
						Name		=> Name,
						Form_Mode	=> Form_Mode,
						Result		=> Result
					);
				Elements_Tag := Elements_Tag & Result;
				-- Form_Element( Name, P.all );
			end;


		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );

		return Elements_Tag;
	end Get_Form_Elements_Tag;




	function Assoc_Form_Elements(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Name_Prefix	: in String;
			Form_Mode	: in Form_Mode_Type;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
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
						Form_Mode	=> Form_Mode,
						Ignore		=> Ignore
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
			Ignore_Relation	: in String := "";
			Ignore		: in KOW_Lib.UString_Vectors.Vector
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
			if Should_Ignore( P, Ignore_Relation, Ignore ) then
				P := KOW_Ent.Property_Lists.Element( C );
				Key := T & "__" & P.Column_Name;
				Ids_Tag := Ids_Tag & Key;
			end if;
		end Iterator;
	begin
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );

		return IDs_Tag;
	end Get_column_ids_Tag;



	function Assoc_column_ids(
			Variable_Name	: in String;
			Entity		: in KOW_Ent.Entity_Type'Class;
			Ignore		: in KOW_Lib.UString_Vectors.Vector
		) return Templates_Parser.Association is
		-- create a Tag inside with all Ids (ordered by the entity's registry) as string
	begin
		return Templates_Parser.Assoc( Variable_Name, Get_Column_Ids_Tag( Entity, Ignore => Ignore ) );
	end Assoc_column_ids;



	procedure Insert(
			Set		: in out Templates_Parser.Translate_Set;
			Variable_Prefix	: in     String;
			Entity		: in     KOW_Ent.Entity_Type'Class;
			Locale		: in     KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale;
			Include_Form	: in     Boolean := False;
			Form_Mode	: in     Form_Mode_Type;
			Ignore		: in     KOW_Lib.UString_Vectors.Vector
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
		Insert( Set, Assoc_column_ids( P & "_column_ids", Entity, Ignore ) );
		Insert( Set, Assoc_Label( P & "_label", Entity, Locale) );
		Insert( Set, Assoc_Labels( P & "_labels", P, Entity, Locale, Ignore ) );
		Insert( Set, Assoc_Resolved_Labels( P & "_resolved_labels", Entity, Locale, Ignore ) );
		Insert( Set, Assoc_Values( P & "_values", Entity, Locale, Ignore ) );
		Insert( Set, Assoc_Resolved_Values( P & "_resolved_values", Entity, Locale, Ignore ) );
		Insert( Set, Assoc( "filter_tags", Entity.Filter_Tags ) );
		
		if Include_Form then
			Insert( Set, Assoc_Form_Elements(
						Variable_Name	=> P & "_form_elements",
						Entity		=> Entity,
						Locale		=> Locale,
						Name_Prefix	=> P,
						Form_Mode	=> Form_Mode,
						Ignore		=> Ignore
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
			Related_Entity	: in     KOW_Ent.Entity_Type'Class
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
		Ignore_Relation	: constant String := Ada.Tags.Expanded_Name( Related_Entity'Tag );
		use Templates_Parser;


		Form_Ids		: Templates_Parser.Tag;

		Tags_Tag		: Templates_Parser.Tag;
		Template_Tags_Tag	: Templates_Parser.Tag;
		Ids_Tag			: Templates_Parser.Tag;
		Column_Ids_Tag		: Templates_Parser.Tag;
		Label_Tag		: Templates_Parser.Tag;
		Labels_Tag		: Templates_Parser.Tag;
		Resolved_Labels_Tag	: Templates_Parser.Tag;
		Values_Tag		: Templates_Parser.Tag;
		Resolved_Values_Tag	: Templates_Parser.Tag;
		Form_Elements_Tag	: Templates_Parser.Tag;

		Template_Form_Elements_Tag	: Templates_Parser.Tag;

		
		procedure Create_Tags_Iterator( C : in KOW_Lib.UString_Vectors.Cursor ) is
			Entity : KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity(
							KOW_Lib.UString_Vectors.Element( C )
						);

			The_Tag : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity'Tag )
					);
			Form_ID	: String := P & '_' & The_Tag;
			The_Label : String := KOW_Ent.Get_Label( Entity, Locale );


			Ignore			: KOW_Lib.UString_Vectors.Vector;
		
		begin
			Form_IDs		:= Form_IDs		& Form_ID;
			Tags_Tag		:= Tags_Tag		& The_Tag;
			Template_Tags_Tag	:= Template_Tags_Tag	& The_Tag;
			Ids_Tag			:= Ids_Tag		& Get_ID( Entity );
			Column_Ids_Tag		:= Column_Ids_Tag	& Get_Column_Ids_Tag( Entity, Ignore_Relation, Ignore => Ignore );
			Label_Tag		:= Label_Tag		& The_Label;
			Labels_Tag		:= Labels_Tag		& Get_Labels_Tag( Entity, Locale, Ignore_Relation, Ignore => Ignore );
			Resolved_Labels_Tag	:= Resolved_Labels_Tag	& Get_Resolved_Labels_Tag( Entity, Locale, Ignore_Relation, Ignore => Ignore );
			Values_Tag		:= Values_Tag		& Get_Values_Tag( Entity, Locale, Ignore_Relation, Ignore => Ignore );
			Resolved_Values_Tag	:= Resolved_Values_Tag	& Get_Resolved_Values_Tag( Entity, Locale, Ignore_Relation, Ignore => Ignore );

			if Include_Form then
				Form_Elements_Tag := Form_Elements_Tag & Get_Form_Elements_Tag(
									Entity		=> Entity,
									Locale		=> Locale,
									Name_Prefix	=> Variable_Prefix,
									Form_Mode	=> Form_Mode,
									Ignore_Relation => Ignore_Relation,
									Ignore		=> Ignore
								);
			end if;

			Template_Form_Elements_Tag := Form_elements_Tag;
		end Create_Tags_Iterator;


	
		procedure Edit_Tags_Iterator( C : in KOW_Lib.UString_Vectors.Cursor ) is
			Entity : KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity(
							KOW_Lib.UString_Vectors.Element( C )
						);

			Empty_Entity : KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity(
							KOW_Lib.UString_Vectors.Element( C )
						);

			The_Tag : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity'Tag )
					);
			Form_ID	: String := P & '_' & The_Tag;
			The_Label : String := KOW_Ent.Get_Label( Entity, Locale );


			All_Ids	: KOW_Ent.Id_Array_Type := KOW_Ent.Get_Related_IDs(
								Related_To	=> Related_Entity,
								Entity_Tag	=> Ada.Tags.Expanded_Name( Entity'Tag )
							);

			L_IDs_Tag		: Templates_Parser.Tag;
			L_Form_Elements_Tag	: Templates_Parser.Tag;

			L_Resolved_Values_Tag	: Templates_Parser.Tag;
			L_Values_Tag		: Templates_Parser.Tag;

			L_Tags_Tag		: Templates_Parser.Tag;


			Ignore			: KOW_Lib.UString_Vectors.Vector;
		

		begin
			Form_IDs		:= Form_IDs		& Form_ID;
			Template_Tags_Tag	:= Template_Tags_Tag	& The_Tag;
			Column_Ids_Tag		:= Column_Ids_Tag	& Get_Column_Ids_Tag( Entity, Ignore_Relation, Ignore => Ignore );
			Label_Tag		:= Label_Tag		& The_Label;
			Labels_Tag		:= Labels_Tag		& Get_Labels_Tag( Entity, Locale, Ignore_Relation, Ignore => Ignore );
			Resolved_Labels_Tag	:= Resolved_Labels_Tag	& Get_Resolved_Labels_Tag( Entity, Locale, Ignore_Relation, Ignore => Ignore );



			for i in All_Ids'Range loop
				KOW_Ent.Load( Entity, All_Ids( i ) );
				L_IDs_Tag := L_IDs_Tag & Get_ID( Entity );
				L_Tags_Tag := L_Tags_Tag & The_Tag;

				L_Values_Tag		:= L_Values_Tag			& Get_Values_Tag( Entity, Locale, Ignore_Relation, Ignore => Ignore );
				L_Resolved_Values_Tag	:= L_Resolved_Values_Tag	& Get_Resolved_Values_Tag( Entity, Locale, Ignore_Relation, Ignore => Ignore );
				if Include_Form then
					L_Form_Elements_Tag := L_Form_Elements_Tag & Get_Form_Elements_Tag(
										Entity		=> Entity,
										Locale		=> Locale,
										Name_Prefix	=> Variable_Prefix,
										Form_Mode	=> Form_Mode,
										Ignore_Relation => Ignore_Relation,
										Ignore		=> Ignore
									);
				end if;
			end loop;
			Ids_Tag			:= Ids_Tag		& L_IDs_Tag;
			Values_Tag		:= Values_Tag		& L_Values_Tag;
			Resolved_Values_Tag	:= Resolved_Values_Tag	& L_Resolved_Values_Tag;
			Tags_Tag		:= Tags_Tag		& L_Tags_Tag;
			if Include_Form then
				Form_Elements_Tag := Form_Elements_Tag & L_Form_Elements_Tag;
			end if;


			Template_Form_Elements_Tag := Template_Form_Elements_Tag & Get_Form_Elements_Tag(
									Entity		=> Empty_Entity,
									Locale		=> Locale,
									Name_Prefix	=> Variable_Prefix,
									Form_Mode	=> Create,
									Ignore_Relation => Ignore_Relation,
									Ignore		=> Ignore
								);

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
		Insert( Set, Assoc( P & "_template_tag",	Template_Tags_Tag ) );
		Insert( Set, Assoc( P & "_id",			Ids_Tag ) );
		Insert( Set, Assoc( P & "_column_ids",		Column_Ids_Tag ) );
		Insert( Set, Assoc( P & "_label",		Label_Tag ) );
		Insert( Set, Assoc( P & "_labels",		Labels_Tag ) );
		Insert( Set, Assoc( P & "_resolved_labels",	Resolved_Labels_Tag ) );
		Insert( Set, Assoc( P & "_values",		Values_Tag ) );
		Insert( Set, Assoc( P & "_resolved_values",	Resolved_Values_Tag ) );
		
		if Include_Form then
			Insert( Set, Assoc( P & "_form_elements", Form_Elements_Tag ) );
			Insert( Set, Assoc( P & "_template_form_elements", Template_Form_Elements_Tag ) );
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
				Log( "Iterating over " & Param_ID & " :: " & Positive'Image( N ) );
				Log( "Value for this property :: " & AWS.Parameters.Get( Params, Param_ID, N ) );
				KOW_Ent.Set_Property( Prop.all, Entity, AWS.Parameters.Get( Params, Param_ID, N ) );
			end if;
		exception
			when constraint_error => null;
		end Iterator;
		Id	: String := AWS.Parameters.Get( Params, P & "id", N );
	begin
		if Id /= "" AND Id /= "x" then
			Log("loading previously saved entity with id " & ID );
		 	KOW_Ent.Load( Entity, KOW_Ent.To_Id( Natural'Value( Id ), Entity'Tag  ) );
		end if;

		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity'Tag, True );
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
		Entity.Filter_Tags := To_Unbounded_String( AWS.Parameters.Get( Params, "filter_tags" ) );
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
	begin	
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

