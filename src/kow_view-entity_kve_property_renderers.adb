


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


package body KOW_View.Entity_KVE_Property_Renderers is


	function T( Str : in String ) return unbounded_string renames To_Unbounded_String;











	--------------------------------------
	-- Hidden Unbounded String Property --
	--------------------------------------
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
		
		Result := Ret & String_Value;
	end Render_Form;







	--------------------------
	-- File Upload Property --
	--------------------------
	overriding
	procedure Render_Form(
				Renderer	: in out File_Upload_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		Ret : Unbounded_String;
		FTypes : Unbounded_String := KOW_View.Entity_Properties.File_Upload_Property_Type( Property ).File_Types;
	begin
		Ret := To_Unbounded_String( "<input type=""file"" name=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String( """ " );
		if FTYpes /= "" then
			Ret := Ret & To_Unbounded_String( "accept=""" ) & FTypes & To_Unbounded_String( """ " );
		end if;

		Ret := Ret & To_Unbounded_String( Disabled_Enabled( Property, Form_Mode ) & "/>");
		-- pra filtrar por tipo de arquivo use accept="image/gif,image/jpeg"

		Result := Ret;
	end Render_Form;




















	-- The stuff we need for bootstraping the packages..


	function Hidden_UString_Factory is new Generic_Factory( Renderer_Type => Hidden_UString_Renderer_Type );
	function File_Upload_Factory is new Generic_Factory( Renderer_Type => File_Upload_Renderer_Type );



	procedure R( T: in Ada.Tags.Tag; F : access function return Property_Renderer_Type'Class )
			renames KOW_View.Entity_Property_Renderers.Registry.Register;

	package P renames KOW_Ent.Properties;


	package KVP renames KOW_View.Entity_Properties;



begin
	R( KVP.Hidden_UString_Property_Type'Tag, Hidden_UString_Factory'Access );
	R( KVP.File_Upload_Property_Type'Tag, File_Upload_Factory'Access );
end KOW_View.Entity_KVE_Property_Renderers;
