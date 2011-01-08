------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWView is free software; you can redistribute it  and/or modify it under--
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWView is distributed in the hope that it will be useful,but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWView; see file COPYING.  If not, write--
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
------------------------------------------------------------------------------
pragma License( GPL );





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
with KOW_Lib.String_Util;
with KOW_View.Entity_Default_Property_Renderers;
with KOW_View.Entity_Properties;
with KOW_View.Entity_Property_Renderers;	use KOW_View.Entity_Property_Renderers;


package body KOW_View.Entity_Extra_Property_Renderers is

	function Dojo_Require( Dojo_Package : in String ) return Unbounded_String is
		JS_Start : constant String := "<script type=""text/javascript"">dojo.require('";
		JS_End : constant String := "');</script>";
	begin
		return To_Unbounded_String( JS_Start & Dojo_Package & JS_End );
	end Dojo_Require;



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
		Ret := Dojo_Require( "dijit.form.DateTextBox" );
		Ret := Ret & To_Unbounded_String( "<input type=""text"" dojoType=""dijit.form.DateTextBox"" name=""" );
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

		String_Value 		: constant String := KOW_Ent.Get_Property( Property, Entity );
		DFirst			: constant Integer := String_Value'First;
		DLast			: constant Integer := DFirst + 9;
		TFirst			: constant Integer := DLast + 2;
		TLast			: constant Integer := String_Value'Last;
		Date_String_Value	: constant String := String_Value( DFirst .. DLast );
		Time_String_Value	: constant String := 'T' & String_Value( TFirst .. TLast );



		SName		: constant String := To_String( Name );

		element_thedate	: constant String := SName & "_thedate";
		element_thetime	: constant String := SName & "_thetime";
		element_input	: constant String := SName;
		function_name	: constant String := KOW_Lib.String_Util.Str_Replace( From => '.', To => '_', Str => SName ) & "_change";

		function T( Str : in String ) return Unbounded_String renames To_Unbounded_String;
	begin
		Ret := Dojo_Require( "dijit.form.DateTextBox" ) & Dojo_Require( "dijit.form.TimeTextBox" );
		Ret := Ret & T( "<script language=""javascript"">" &
				"function "&function_name&"(){" &
				"theDate = dijit.byId(""" & element_thedate & """).attr('value');" &
				"theTime = dijit.byId(""" & element_thetime & """).attr('value');" &
				"theInput = dojo.byId( """ & element_input & """ );" &
				"theInput.theDate = dojo.date.stamp.toISOString(theDate, {selector: 'date'});" &
				"theInput.theTime = dojo.date.stamp.toISOString(theTime,{selector: 'time'}).substring(1,9);" &
				"theInput.value = theInput.theDate + ' ' + theInput.theTime;}"  &
			"</script>" );

		Ret := Ret & "<input type=""text"" dojoType=""dijit.form.DateTextBox"" name=""";
		Ret := Ret & element_thedate;
		Ret := Ret & T( """ value=""" & Date_String_Value & """ id=""" );
		Ret := Ret & element_thedate & """ ";
		Ret := Ret & "onchange=""" & function_name & "()"" ";
		Ret := Ret & T( Disabled_Enabled( Property, Form_Mode ) & "/>");

		Ret := Ret & "<input type=""text"" dojoType=""dijit.form.TimeTextBox"" name=""";
		Ret := Ret & element_thetime;
		Ret := Ret & T( """ value=""" & Time_String_Value & """ id=""" );
		Ret := Ret & element_thetime & """ ";
		Ret := Ret & "onchange=""" & function_name & "()"" ";
		Ret := Ret & T( Disabled_Enabled( Property, Form_Mode ) & "/>");


		Ret := Ret & "<input type=""hidden"" name=""";
		Ret := Ret & element_input;
		Ret := Ret & T( """ value=""" & String_Value & """ id=""" );
		Ret := Ret & element_input & """/> ";


		
		Result := Ret;
	end Render_Form;










	function Date_Factory is new KOW_View.Entity_Property_Renderers.Generic_Factory( Renderer_Type => Date_Renderer_Type );
	function Timestamp_Factory is new KOW_View.Entity_Property_Renderers.Generic_Factory( Renderer_Type => Timestamp_Renderer_Type );



	procedure R( T: in Ada.Tags.Tag; F : access function return Property_Renderer_Type'Class )
			renames KOW_View.Entity_Property_Renderers.Registry.Register;

	package P renames KOW_Ent.Extra_Properties;


	package KVP renames KOW_View.Entity_Properties;



begin
	R( P.Date_Properties.Property_Type'Tag,  Date_Factory'Access );
	R( P.Timestamp_Properties.Property_Type'Tag,  Timestamp_Factory'Access );

end KOW_View.Entity_Extra_Property_Renderers;
