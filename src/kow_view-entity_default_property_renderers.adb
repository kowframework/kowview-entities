


--------------
-- Ada 2005 --
--------------
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent.Properties;
with KOW_Lib.Locales;
with KOW_View.Entity_Properties;
with KOW_View.Entity_Property_Renderers;		 use KOW_View.Entity_Property_Renderers;


package body KOW_View.Entity_Default_Property_Renderers is


	function T( Str : in String ) return unbounded_string renames To_Unbounded_String;







	function Generic_Factory return Property_Renderer_Type'Class is
		R : Renderer_Type;
	begin
		return R;
	end Generic_Factory;




	-------------------------------
	-- Unbounded String Property --
	-------------------------------
	overriding
	procedure Render_Form(
				Renderer	: in out UString_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		P : KOW_Ent.Properties.UString_Property_Type'Class := KOW_Ent.Properties.UString_Property_Type'Class( Property );
		String_Value : String := KOW_Ent.Get_Property( Property, Entity );
		Ret : Unbounded_String;
	begin
		if P.Length > 255 then
			Ret := To_Unbounded_String( "<textarea name=""" );
			Ret := Ret & Name;
			Ret := Ret & To_Unbounded_String( """ maxlength=""" & 
					Integer'Image(
						P.Length
				) & """" & Disabled_Enabled( Property,Form_Mode ) & ">" );
			Ret := Ret & To_Unbounded_String( String_Value );
			Ret := Ret & To_Unbounded_String( "</textarea>" );
		else
			Ret := To_Unbounded_String( "<input type=""text"" name=""" );
			Ret := Ret & Name;
			Ret := Ret & To_Unbounded_String(
						""" value=""" & String_Value & """"
					);
			Ret := Ret & To_Unbounded_String( " maxlength=""" & Integer'Image(
						P.Length
					) & """" );
			Ret := Ret & T( Disabled_Enabled( Property, Form_Mode ) & "/>");
		end if;

		Result := Ret;
	end Render_Form;


	----------------------
	-- Boolean Property --
	----------------------
	overriding
	procedure Render_Form(
				Renderer	: in out Boolean_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		Ret : Unbounded_String;
	begin
		if Form_Mode = Edit and then Property.Immutable then
			Ret := T( "<div>" );
		else
			Ret := T( "<div onClick=""trueFalseMe(this)"">" );
		end if;

		Ret := Ret & T( "<input type=""hidden"" name=""") & Name & T( """ " );

		if KOW_Ent.Properties.Boolean_Property_Type'Class( Property ).Getter.all( Entity ) then
			Ret := Ret & T( "value=""true""/>" );
			Ret := Ret & T( "<img src=""/themes/true.png""/>" );
		else
			Ret := Ret & T( "value=""false""/>" );
			Ret := Ret & T( "<img src=""/themes/false.png""/>" );
		end if;
		Ret := Ret & T( "</div>" );

		Result := Ret;
	end Render_Form;


	procedure Render_View(
				Renderer	: in out Boolean_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Result		:    out Unbounded_String
			) is
	begin
		if KOW_Ent.Properties.Boolean_Property_Type'Class( Property ).Getter.all( Entity ) then
			Result := T( "<img src=""/themes/true.png""/>" );
		else
			Result := T( "<img src=""/themes/false.png""/>" );
		end if;
	end Render_View;



	--------------------------
	-- Foreign Key Property --
	--------------------------


	overriding
	procedure Render_Form(
				Renderer	: in out Foreign_Key_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		PP	: KOW_Ent.Properties.Foreign_Key_Property_Type'Class :=
				KOW_Ent.Properties.Foreign_Key_Property_Type'Class( Property );
		Ent  : KOW_Ent.Entity_Type'Class := KOW_Ent.New_Entity( PP.Related_Entity_Tag );
		All_Ids : KOW_Ent.Id_Array_Type := KOW_Ent.Get_All_Ids( PP.Related_Entity_Tag );

		Ret	: Unbounded_String;
		String_Value : String := KOW_Ent.Get_Property( Property, Entity );


		procedure Foreign_Key_Iterator( E : in KOW_Ent.Entity_Type'Class ) is
			use KOW_Ent;
			
			ID : Id_Type := KOW_Ent.To_ID( Natural'Value( String_Value ) );
			String_Id : String := KOW_Ent.To_String( E.Id );

		begin
			Ret := Ret & T( "<option value=""" );
			Ret := Ret & T( String_ID );
			Ret := Ret & T( """" );
			ID.My_Tag := E'Tag;
			if ID = E.Id then
				Ret := Ret & T( " selected=""1""" );
			end if;
			Ret := Ret & T( ">" );
			Ret := Ret & T( KOW_Ent.To_String( E ) );
			Ret := Ret & T( "</option>" );
		end Foreign_Key_Iterator;




	begin
		Ret := T( "<select dojoType=""dijit.form.FilteringSelect"" name=""" );
		Ret := Ret & Name & """";
		Ret := Ret & T( Disabled_Enabled( Property, Form_Mode ) );
		Ret := Ret & T( ">" );

		for i in All_Ids'First .. All_Ids'Last loop
			KOW_Ent.Load( Ent, All_Ids( i ) );
			Foreign_Key_Iterator( Ent );
		end loop;
		Ret := Ret & T("</select>" );

		Result := Ret;

	end Render_Form;

	---------------------
	-- Locale Property --
	---------------------
	

	overriding
	procedure Render_Form(
				Renderer	: in out Locale_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is


		Ret		: Unbounded_String;
		String_Value	: String := KOW_Ent.Get_Property( Property, Entity );

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
		Ret := T( "<select dojoType=""dijit.form.FilteringSelect"" name=""" );
		Ret := Ret & Name & """";
		Ret := Ret & T( Disabled_Enabled( Property, Form_mode ) );
		Ret := Ret & T( ">" );

		KOW_Lib.Locales.Locale_Tables.Iterate(
				KOW_Lib.Locales.Supported_Locales,
				Locale_Iterator'Access
			);

		Ret := Ret & T("</select>" );
		Result := Ret;
	end Render_Form;


	-----------------------
	-- Password Property --
	-----------------------
	overriding
	procedure Render_Form(
				Renderer	: in out Password_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		Ret : Unbounded_String;
		String_Value : String := KOW_Ent.Get_Property( Property, Entity );
	begin
		Ret := To_Unbounded_String( "<input type=""password"" name=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String(
					""" value=""" & String_Value & """"
				);

		Ret := Ret & T( Disabled_Enabled( Property, Form_Mode ) & "/>");

		Result := Ret;

	end Render_Form;










	---------------------------------------------
	-- Some types declared within this library --
	---------------------------------------------

	-------------------------------
	-- Unbounded String Property --
	-------------------------------
	procedure Render_Form(
				Renderer	: in out Hidden_UString_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		Ret : Unbounded_String;
		String_Value : String := KOW_Ent.Get_Property( Property, Entity );
	begin
		Ret := To_Unbounded_String( "<input type=""hidden"" name=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String( """ value=""" & String_Value & """" );
		Ret := Ret & To_Unbounded_String( Disabled_Enabled( Property, Form_Mode ) & "/>");
		
		Result := Ret;
	end Render_Form;


	-- The stuff we need for bootstraping the packages..


	function UString_Factory is new Generic_Factory( Renderer_Type => UString_Renderer_Type );
	function Boolean_Factory is new Generic_Factory( Renderer_Type => Boolean_Renderer_Type );
	function Foreign_Key_Factory is new Generic_Factory( Renderer_Type => Foreign_Key_Renderer_Type );
	function Locale_Factory is new Generic_Factory( Renderer_Type => Locale_Renderer_Type );
	function Password_Factory is new Generic_Factory( Renderer_Type => Password_Renderer_Type );
	function Hidden_UString_Factory is new Generic_Factory( Renderer_Type => Hidden_UString_Renderer_Type );



	procedure R( T: in Ada.Tags.Tag; F : access function return Property_Renderer_Type'Class )
			renames KOW_View.Entity_Property_Renderers.Registry.Register;

	package P renames KOW_Ent.Properties;


	package KVP renames KOW_View.Entity_Properties;



begin
	R( P.UString_Property_Type'Tag,  UString_Factory'Access );
	R( P.Boolean_Property_Type'Tag, Boolean_Factory'Access );
	R( P.Foreign_Key_Property_Type'Tag, Foreign_Key_Factory'Access );
	R( P.Locale_Property_Type'Tag, Locale_Factory'Access );
	R( P.Password_Property_Type'Tag, Password_Factory'Access );
	R( KVP.Hidden_UString_Property_Type'Tag, Hidden_UString_Factory'Access );
end KOW_View.Entity_Default_Property_Renderers;
