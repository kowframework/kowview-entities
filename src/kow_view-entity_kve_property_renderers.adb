


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


	-------------------------------
	-- Rich Text String Property --
	-------------------------------

	overriding
	procedure Render_Form(
				Renderer	: in out Rich_Text_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		P : KOW_View.Entity_Properties.Rich_Text_Property_Type'Class := KOW_View.Entity_Properties.Rich_Text_Property_Type'Class( Property );
		String_Value : String := KOW_Ent.Get_Property( Property, Entity );
		Ret : Unbounded_String;
	begin
		Ret := To_Unbounded_String( "<textarea name=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String( """ id=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String( """ maxlength=""" & 
				Integer'Image(
					P.Length
			) & """" & Disabled_Enabled( Property,Form_Mode ) & ">" );
		Ret := Ret & To_Unbounded_String( String_Value );
		Ret := Ret & To_Unbounded_String( "</textarea>" );
		Ret := Ret & To_Unbounded_String( "<script type=""text/javascript"">tinyMCE.execCommand(""mceAddControl"", true, """ );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String(""")</script>" );
		Result := Ret;
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
	begin
		Ret := To_Unbounded_String( "<input type=""file"" name=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String( """ " );

		Ret := Ret & To_Unbounded_String( Disabled_Enabled( Property, Form_Mode ) & "/>");

		Result := Ret;
	end Render_Form;





	---------------------------
	-- Image Upload Property --
	---------------------------

	overriding
	procedure Render_Form(
				Renderer	: in out Image_Upload_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		Ret1, Ret2 : Unbounded_String;


		use Ada.Tags;
	begin

		if Entity.ID.My_Tag = Entity'Tag then
			Ret1 := To_Unbounded_String( "<a href=""/image/" & Expanded_Name( Entity'Tag ) & "/" & KOW_Ent.To_String( Entity.ID ) & "/" & To_String( Property.Column_Name ) & """>" );
			Ret1 := Ret1 & To_Unbounded_String( "<img src=""" & KOW_Ent.Image_URL( Entity ) & """/>" );
			Ret1 := Ret1 & To_Unbounded_String( "</a>" );
		end if;

		Render_Form(
				Renderer	=> File_Upload_Renderer_Type( Renderer ),
				Entity		=> Entity,
				Property	=> property,
				Name		=> Name,
				Form_Mode	=> Form_Mode,
				Result		=> Ret2
			);
		

		Result := Ret1 & Ret2;
	end Render_Form;



	overriding
	procedure Render_View(
				Renderer	: in out Image_Upload_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Result		:    out Unbounded_String
			) is
		use Ada.Tags;
		Ret1 : Unbounded_String;
	begin
		Ret1 := To_Unbounded_String( "<a href=""/image/" & Expanded_Name( Entity'Tag ) & "/" & KOW_Ent.To_String( Entity.ID ) & "/" & To_String( Property.Column_Name ) & """>" );
		Ret1 := Ret1 & To_Unbounded_String( "<img src=""" & KOW_Ent.Image_URL( Entity ) & """/>" );
		Ret1 := Ret1 & To_Unbounded_String( "</a>" );
		Result := Ret1;
	end Render_View;







	-- The stuff we need for bootstraping the packages..


	function Hidden_UString_Factory is new Generic_Factory( Renderer_Type => Hidden_UString_Renderer_Type );
	function Rich_Text_Factory is new Generic_Factory( Renderer_Type => Rich_Text_Renderer_Type );
	function File_Upload_Factory is new Generic_Factory( Renderer_Type => File_Upload_Renderer_Type );
	function Image_Upload_Factory is new Generic_Factory( Renderer_Type => Image_Upload_Renderer_Type );



	procedure R( T: in Ada.Tags.Tag; F : access function return Property_Renderer_Type'Class )
			renames KOW_View.Entity_Property_Renderers.Registry.Register;

	package P renames KOW_Ent.Properties;


	package KVP renames KOW_View.Entity_Properties;



begin
	R( KVP.Hidden_UString_Property_Type'Tag, Hidden_UString_Factory'Access );
	R( KVP.Rich_Text_Property_Type'Tag, Rich_Text_Factory'Access );
	R( KVP.File_Upload_Property_Type'Tag, File_Upload_Factory'Access );
	R( KVP.Image_Upload_Property_Type'Tag, Image_Upload_Factory'Access );
end KOW_View.Entity_KVE_Property_Renderers;
