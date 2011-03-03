------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               CopyRich (C) 2007-2011, KOW Framework Project             --
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



------------------------------------------------------------------------------
-- Renderer for properties                                                  --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Tags;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Generic_Property_Metadata;
with KOW_Ent.Properties;
with KOW_View.Entities.Properties;
with KOW_View.Locales;

---------
-- AWS --
---------
with AWS.Status;

package body KOW_View.Entities.Property_Renderers is


	Default_Renderer : Property_Renderer_Ptr := new Basic_Property_Renderer_Type;
	-- only one instance as it has no internal variables...

	function Get_Default_Renderer(
				Property	: in KOW_Ent.Entity_Property_Type'Class
			) return Property_Renderer_Ptr is
		Renderer : Property_Renderer_Ptr;
	begin
		Renderer := Default_Renderers_Registry.Get( Property'Tag );
		if Renderer = null then
			return Default_Renderer;
		else
			return Renderer;
		end if;
	exception
		when others =>
			return Default_Renderer;
	end Get_Default_Renderer;



	--------------------------------
	-- Property Renderer Registry --
	--------------------------------
	
	protected body Default_Renderers_Registry is

		-- the property renderers here are used in case no renderer is registered to a given property
		-- in the entity property metadata.
		--
		-- These are called by the basic property renderer
		
		procedure Set(
					Property_Tag	: in Ada.Tags.Tag; 
					Renderer	: in Property_Renderer_Ptr
				) is
		begin
			Set( To_Unbounded_String( Ada.Tags.Expanded_Name( Property_Tag ) ), Renderer );
		end Set;

		procedure Set(
					Property_Tag	: in Unbounded_String;
					Renderer	: in Property_Renderer_Ptr
				) is
		begin
			Renderer_Maps.Include( Renderer_Map, Property_Tag, Renderer );
		end Set;

		function Get(
					Property_Tag	: in Ada.Tags.Tag
				) return Property_Renderer_Ptr is
		begin
			return Get( To_Unbounded_String( Ada.Tags.Expanded_Name( Property_Tag ) ) );
		end Get;

		function Get(
					Property_Tag	: in Unbounded_String
				) return Property_Renderer_Ptr is
		begin
			return Renderer_Maps.Element( Renderer_Map, Property_Tag );
		exception
			when others =>
				return null;
		end Get;
	end Default_Renderers_Registry;


	-----------------------------
	-- Basic Property Renderer --
	-----------------------------

	overriding
	procedure Render_Property(
				Renderer	: in out Basic_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
		-- render the property using the default property renderers registered by the property tag

		Label	: constant String := Get_Label(
							Renderer	=> Basic_Property_Renderer_Type'Class( Renderer ),
							Request		=> Request,
							Entity		=> Entity,
							Property	=> Property,
							Style		=> Style
						);

		Input	: Unbounded_String;		
	begin
		Get_Input(
				Renderer	=> Basic_Property_Renderer_Type'Class( Renderer ),
				Module		=> Module,
				Request		=> Request,
				Entity		=> Entity,
				Property	=> Property,
				Style		=> Style,
				Output		=> Input
			);
	
		Output := To_Unbounded_String( "<span class=""label"">" & Label ) & "</span><span class=""value"">" & Input & "</span>";
	end Render_Property;

	function Get_Label(
				Renderer	: in Basic_Property_Renderer_Type;
				Request		: in AWS.Status.Data;
				Entity		: in KOW_Ent.Entity_Type'Class;
				Property	: in KOW_Ent.Entity_Property_Type'Class;
				Style		: in Rendering_Style_Type
			) return String is
	begin
		return KOW_Ent.Get_Label( Entity, Property.Column_Name, KOW_View.Locales.Get_Locale( Request ) );
	end Get_Label;

	procedure Get_Input(
				Renderer	: in out Basic_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
		Value	: constant String := KOW_Ent.Get_Property( Property, Entity );
	begin
		case Style is
			when Big_Rendering | Small_Rendering =>
				Output := To_Unbounded_String( Value );
			when Big_Edit_Rendering | Small_Edit_Rendering =>
				declare
					Buffer	: Unbounded_String;
				begin
					KOW_View.Modules.Include_Dojo_Package( Module, "dijit.form.ValidationTextBox" );
					Append( Buffer, "<input type=""text"" dojoType=""dijit.form.ValidationTextBox"" name=""" );
					Append( Buffer, Property.Column_Name );
					Append( Buffer, """ " );
					if Property.Immutable and then not KOW_Ent.Is_New( Entity ) then
						Append( Buffer, "disabled " );
					end if;
					Append( Buffer, "value=""" & Value & """/>" );
					Output := Buffer;
				end;
		end case;
	end Get_Input;

	----------------------------
	-- Text Property Renderer --
	----------------------------

	overriding
	procedure Get_Input(
				Renderer	: in out Text_property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
		Value	: constant String := KOW_Ent.Get_Property( Property, Entity );
	begin
		if Property not in KOW_Ent.Properties.UString_Property_Type'Class then
			raise CONSTRAINT_ERROR with "Can't render text property that's not implemented by ustring_property_type";
		end if;



		case Style is
			when Big_Rendering | Small_Rendering =>
				Output := To_Unbounded_String( Value );
			when Big_Edit_Rendering | Small_Edit_Rendering =>
				declare
					Buffer	: Unbounded_String;
					Length	: constant Positive := KOW_Ent.Properties.UString_Property_Type'Class( Property ).Length;
					SLength : constant String := Ada.Strings.Fixed.Trim( Positive'Image( Length ), Ada.Strings.Both );
				begin
					if Length <= 255 then
						KOW_View.Modules.Include_Dojo_Package( Module, "dijit.form.ValidationTextBox" );
						Append( Buffer, "<input type=""text"" dojoType=""dijit.form.ValidationTextBox"" name=""" );
						Append( Buffer, Property.Column_Name );
						Append( Buffer, """ " );
						if Property.Immutable and then not KOW_Ent.Is_New( Entity ) then
							Append( Buffer, "disabled " );
						end if;
						Append( Buffer, "value=""" & Value & """ maxLength=""" & SLength & """/>" );
						Output := Buffer;
					else
						KOW_View.Modules.Include_Dojo_Package( Module, "dijit.form.Textarea" );
						Append( Buffer, "<textarea dojoType=""dijit.form.Textarea"" name=""" );
						Append( Buffer, Property.Column_Name );
						Append( Buffer, """ " );
						if Property.Immutable and then not KOW_Ent.Is_New( Entity ) then
							Append( Buffer, "disabled " );
						end if;
						Append( Buffer, "maxLength=""" & SLength & """>" );
						Append( Buffer, Value );
						Append( Buffer, "</textarea>" );

						Output := Buffer;
					end if;
				end;
		end case;
	end Get_Input;


	---------------------------------
	-- Rich Text Property Renderer --
	---------------------------------


	function Strip_HTML(
				Input		: in String;
				Max_Length	: in Positive
			) return String is
		Striped		: string( 1 .. Max_Length ) := ( others => ' ' );
		Real_Length	: Positive := 1;

		In_Tag		: Boolean := False;
	begin
		for i in Input'range loop
			if In_tag then
				In_Tag := Input( i ) /= '>';
			else
				In_Tag := Input( i ) = '<';
				
				if not In_Tag then
					Striped( Real_Length ) := Input( i );
	
					exit when Real_Length = Max_Length;
					Real_Length := Real_Length + 1;
				end if;
			end if;
		end loop;

		if Real_Length = Max_Length then
			return Striped & "...";
		else
			return Striped( 1 .. Real_Length );
		end if;
	end Strip_HTML;

	overriding
	procedure Get_Input(
				Renderer	: in out Rich_Text_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
		Value : constant String := KOW_Ent.Get_Property( Property, Entity );

		procedure Editor( Class : in String ) is
			-- TODO :: implement in the property support for plugins in dojo editor through parameters
			Editor_ID	: Unbounded_String;
			Input_ID	: Unbounded_String;
		begin
			KOW_View.Modules.Generate_HTML_Id( Module, Editor_ID );
			KOW_View.Modules.Generate_HTML_Id( Module, Input_ID );

			KOW_View.Modules.Include_Dojo_Package( Module, "dijit.Editor" );
			Append( Output, "<div class=""" & Class & """ dojoType=""dijit.Editor"" id=""" );
			Append( Output, Editor_ID );
			Append( Output, """>" & Value & "</div>" );

			Append( Output, "<input type=""hidden"" name=""" );
			Append( Output, Property.Column_Name );
			Append( Output, """ id=""" );
			Append( Output, Input_ID );
			Append( Output, """/>" );

			Append( Output, "<script type=""text/javascript"">dojo.addOnLoad(function () { kowview.entities.connectEditor( """ );
			Append( Output, Editor_ID );
			Append( Output, """,""" );
			Append( Output, Input_ID );
			Append( Output, """);});</script>" );
		end Editor;
	begin
		case Style is
			when Big_Rendering =>
				Output := To_Unbounded_String( "<div class=""richTextContent"">" & Value & "</div>" );
			when Small_Rendering =>
				Output := To_Unbounded_String( "<div class=""richTextContentPreview"">"& Strip_HTML( Value, 150 ) & "</div>" ); -- TODO here

			when Big_Edit_Rendering =>
				Editor( "richTextEditor" );

			when Small_Edit_Rendering =>
				Editor( "smallRichTextEditor" );
		end case;
	end Get_Input;

begin
	Default_Renderers_Registry.Set(
					KOW_Ent.Properties.UString_Property_Type'Tag,
					new Text_Property_Renderer_Type
				);

	Default_Renderers_Registry.Set(
					KOW_View.Entities.Properties.Rich_Text_Property_Type'Tag,
					new Rich_Text_Property_Renderer_Type
				);

	Default_Renderers_Registry.Set(
					KOW_View.Entities.Properties.Rich_Text_File_Property_Type'Tag,
					new Rich_Text_Property_Renderer_Type
				);

end KOW_View.Entities.Property_Renderers;
