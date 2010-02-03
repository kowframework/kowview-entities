


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;					use KOW_Ent;
with KOW_View.Entity_Property_Renderers;	use KOW_View.Entity_Property_Renderers;


package KOW_View.Entity_Extra_Property_Renderers is




	--------------------------
	-- Date String Property --
	--------------------------
	type Date_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out Date_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);


	-------------------------------
	-- Timestamp String Property --
	-------------------------------
	type Timestamp_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out Timestamp_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);



end KOW_View.Entity_Extra_Property_Renderers;
