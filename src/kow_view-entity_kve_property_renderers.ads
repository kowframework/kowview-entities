


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;					use KOW_Ent;
with KOW_View.Entity_Property_Renderers;	use KOW_View.Entity_Property_Renderers;

package KOW_View.Entity_KVE_Property_Renderers is

	--------------------------------------
	-- Hidden Unbounded String Property --
	--------------------------------------
	type Hidden_UString_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out Hidden_UString_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);



	--------------------------
	-- File Upload Property --
	--------------------------
	type File_Upload_Renderer_Type is new Property_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out File_Upload_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);


	
	---------------------------
	-- Image Upload Property --
	---------------------------
	type Image_Upload_Renderer_Type is new File_Upload_Renderer_Type with null record;

	overriding
	procedure Render_Form(
				Renderer	: in out Image_Upload_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			);
	overriding
	procedure Render_View(
				Renderer	: in out Image_Upload_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Result		:    out Unbounded_String
			);



end KOW_View.Entity_KVE_Property_Renderers;
