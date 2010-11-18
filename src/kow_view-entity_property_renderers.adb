

--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;					use KOW_Ent;

package body KOW_View.Entity_Property_Renderers is


	type Property_Render_Type is abstract tagged null record;
	-- this type is used by the kow_view-entity application to render HTML views
	-- for both viewing and editing the property.




	function Disabled_Enabled(
				Property	: in KOW_Ent.Entity_Property_Type'Class;
				Form_Mode	: in Form_Mode_Type
			) return String is
	begin
		if Form_Mode = Edit AND THEN Property.Immutable then
			return " disabled ";
		else
			return "";
		end if;
	end Disabled_Enabled;






	procedure Render_Form(
				Renderer	: in out Property_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Name		: in     Unbounded_String;
				Form_Mode	: in     Form_Mode_Type;
				Result		:    out Unbounded_String
			) is
		Ret : Unbounded_String;
		String_Value : String := KOW_Ent.Get_Property( Property, Entity );
	begin
		Ret := To_Unbounded_String( "<input type=""text"" name=""" );
		Ret := Ret & Name;
		Ret := Ret & To_Unbounded_String( """ value=""" & String_Value & """" );
		Ret := Ret & To_Unbounded_String( Disabled_Enabled( Property, Form_Mode ) & "/>");
		
		Result := Ret;
	end Render_Form;


	procedure Render_View(
				Renderer	: in out Property_Renderer_Type;
				Entity		: in     Entity_Type'Class;
				Property	: in     Entity_Property_Type'Class;
				Result		:    out Unbounded_String
			) is
	begin
		Result := To_Unbounded_String(
					KOW_Ent.Get_Property( Property, Entity )
				);
	end Render_View;



	
	

	function TU( T : in Ada.Tags.Tag ) return Unbounded_String is
	begin
		return To_Unbounded_String(
				Ada.Tags.Expanded_Name( T )
			);
	end TU;

	protected body Registry is
		function Get_Renderer(
					U	: Unbounded_String
				) return Property_Renderer_Type'Class is
			use Factory_Maps;
		begin
			if Contains( Factories, U ) then
				return Element( Factories, U ).all;
			else
				declare
					R : Property_Renderer_Type;
				begin
					return R;
				end;
			end if;

		end Get_Renderer;



		function Get_Renderer(
					T	: Ada.Tags.Tag
				) return Property_Renderer_Type'Class is
		begin
			return Get_Renderer( TU( T ) );
		end Get_Renderer;


		procedure Register(
					Property_Tag	: in Ada.Tags.Tag;
					Renderer_Factory: access function return Property_Renderer_Type'Class
				) is
		begin
			Factory_Maps.Include( Factories, TU( Property_Tag), Renderer_Factory );
		end Register;
	end Registry;




	function Generic_Factory return Property_Renderer_Type'Class is
		R : Renderer_Type;
	begin
		return Property_Renderer_Type'Class(R);
	end Generic_Factory;




end KOW_View.Entity_Property_Renderers;
