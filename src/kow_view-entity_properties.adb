



--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Properties;

--------------------------------------------
-- package with some useful properties... --
--------------------------------------------



package body KOW_View.Entity_Properties is



	function New_Hidden_UString_Property(
				Column_Name	: in     String;
				Getter		: not null access function( Entity : in Entity_Type'Class ) return Unbounded_String;
				Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Unbounded_String );
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150
			) return Entity_Property_Ptr is
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL
		UStr : Hidden_UString_Property_Type;
	begin
		UStr.Column_Name	:= To_Unbounded_String( Column_Name );
		UStr.Getter		:= Getter;
		UStr.Setter		:= Setter;
		UStr.Default_Value	:= To_Unbounded_String( Default_Value );
		UStr.Immutable		:= Immutable;
		UStr.Length		:= Length;
		return new Hidden_UString_Property_Type'( UStr );
	end New_Hidden_UString_Property;



end KOW_View.Entity_Properties;
