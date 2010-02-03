


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;					use KOW_Ent;
with KOW_Ent.Extra_Properties;
with KOW_View.Entity_Default_Property_Renderers;
with KOW_View.Entity_Properties;
with KOW_View.Entity_Property_Renderers;	use KOW_View.Entity_Property_Renderers;


package body KOW_View.Entity_Extra_Property_Renderers is




	--------------------------
	-- Date String Property --
	--------------------------

	overriding
	procedure Render_Form(
				Renderer	: in out Date_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		Ret : Unbounded_String;
		String_Value : String := KOW_Ent.Get_Property( Property, Entity );
	begin
		Ret := To_Unbounded_String( "<input type=""text"" dojoType=""dijit.form.DateTextBox"" name=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String( """ value=""" & String_Value & """" );
		Ret := Ret & To_Unbounded_String( Disabled_Enabled( Property, Form_Mode ) & "/>");
		
		Result := Ret;
	end Render_Form;











	-------------------------------
	-- Timestamp String Property --
	-------------------------------

	overriding
	procedure Render_Form(
				Renderer	: in out Timestamp_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		Ret : Unbounded_String;
		String_Value : String := KOW_Ent.Get_Property( Property, Entity );
	begin
		Ret := To_Unbounded_String( "<input type=""text"" dojoType=""dijit.form.DateTextBox"" name=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String( """ value=""" & String_Value & """" );
		Ret := Ret & To_Unbounded_String( Disabled_Enabled( Property, Form_Mode ) & "/>");
		
		Result := Ret;
	end Render_Form;










	function Date_Factory is new KOW_View.Entity_Default_Property_Renderers.Generic_Factory( Renderer_Type => Date_Renderer_Type );
	function Timestamp_Factory is new KOW_View.Entity_Default_Property_Renderers.Generic_Factory( Renderer_Type => Timestamp_Renderer_Type );



	procedure R( T: in Ada.Tags.Tag; F : access function return Property_Renderer_Type'Class )
			renames KOW_View.Entity_Property_Renderers.Registry.Register;

	package P renames KOW_Ent.Extra_Properties;


	package KVP renames KOW_View.Entity_Properties;



begin
	R( P.Date_Properties.Property_Type'Tag,  Date_Factory'Access );
	R( P.Timestamp_Properties.Property_Type'Tag,  Timestamp_Factory'Access );

end KOW_View.Entity_Extra_Property_Renderers;
