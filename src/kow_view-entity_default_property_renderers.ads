


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;					use KOW_Ent;
with KOW_View.Entity_Property_Renderers;	use KOW_View.Entity_Property_Renderers;

package KOW_View.Entity_Default_Property_Renderers is




	-------------------------------
	-- Unbounded String Property --
	-------------------------------
	type UString_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out UString_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);

	
	----------------------
	-- Boolean Property --
	----------------------
	type Boolean_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out Boolean_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);

	procedure Render_View(
				Renderer	: in out Boolean_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Result		:    out Unbounded_String
			);





	--------------------------
	-- Foreign Key Property --
	--------------------------

	type Foreign_Key_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out Foreign_Key_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);


	---------------------
	-- Locale Property --
	---------------------
	
	type Locale_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out Locale_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);



	-----------------------
	-- Password Property --
	-----------------------
	
	type Password_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out Password_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);





end KOW_View.Entity_Default_Property_Renderers;
