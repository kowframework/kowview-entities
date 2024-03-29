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
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Tags;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Extra_Properties;
with KOW_Ent.Generic_Property_Metadata;
with KOW_Ent.Id_Query_Builders;
with KOW_Ent.Properties;
with KOW_Lib.Calendar;
with KOW_Lib.Locales;
with KOW_View.Entities.Components;
with KOW_View.Entities.Properties;
with KOW_View.Locales;
with KOW_View.Time_Zones;

with KOW_View.Entities.Modules;

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
			use Ada.Tags;
			The_Tag		: Unbounded_String	:= To_Unbounded_String( Expanded_Name( Property_Tag ) );
			Renderer	: Property_Renderer_Ptr	:= Get( The_Tag );
		begin
			if Renderer = null then
				declare
					Parent : Tag := Parent_Tag( Property_Tag );
				begin
					if Parent = No_Tag then
						return null;
					else
						return Get( Parent );
					end if;
				end;
			else
				return Renderer;
			end if;
		end Get;

		function Get(
					Property_Tag	: in Unbounded_String
				) return Property_Renderer_Ptr is
		begin
			if Renderer_Maps.Contains( Renderer_Map, Property_Tag ) then
				return Renderer_Maps.Element( Renderer_Map, Property_Tag );
			else
				return null;
			end if;
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

	begin
		KOW_View.Modules.Generate_HTML_Id( Module, Renderer.HTML_Id );

		declare
			Label	: constant String := Get_Label(
								Renderer	=> Basic_Property_Renderer_Type'Class( Renderer ),
								Request		=> Request,
								Entity		=> Entity,
								Property	=> Property,
								Style		=> Style
							);

			Input	: Unbounded_String;


			Label_Id : constant String := To_String( Renderer.HTML_id ) & "_label";
			Value_Id : constant String := To_String( Renderer.HTML_id ) & "_value";
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
		
			Output := To_Unbounded_String( "<span id="""&label_id&""" class=""label"">" & Label ) & "</span><span id="""&value_id&""" class=""value"">" & Input & "</span>";
		end;
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


		procedure Append_If_Set( Key : in String; Value : in Unbounded_String ) is
		begin
			if Value /= "" then
				Append( Output, " " );
				Append( Output, Key );
				Append( Output, "=""" );
				Append( Output, Value );
				Append( Output, """" );
			end if;
		end Append_if_Set;

		procedure Editor( Class : in String ) is
			Editor_ID	: Unbounded_String;
			Input_ID	: Unbounded_String;
		begin
			KOW_View.Modules.Generate_HTML_Id( Module, Editor_ID );
			KOW_View.Modules.Generate_HTML_Id( Module, Input_ID );

			KOW_View.Modules.Include_Dojo_Package( Module, "dijit.Editor" );
			Append( Output, "<div class=""" & Class & """ styleSheets=""/pages/css/component:entities/kowview-rich_text.css"" dojoType=""dijit.Editor"" id=""" );
			Append( Output, Editor_ID );
			Append( Output, """" );

			Append_If_Set( "plugins", Renderer.Plugins );
			Append_If_Set( "extraPlugins", Renderer.Extra_Plugins );
			
			Append( Output, ">" & Value & "</div>" );

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
		KOW_View.Modules.Include_Component_CSS(
					Module		=> Module,
					Component	=> KOW_View.Entities.Components.Component,
					CSS		=> "kowview-rich_text.css"		-- css with some data formating
				);

		case Style is
			when Big_Rendering =>
				Output := To_Unbounded_String( "<div class=""richTextContent"">" & Value & "</div>" );
			when Small_Rendering =>
				Output := To_Unbounded_String( "<div class=""richTextContentPreview"">"& Strip_HTML( Value, 150 ) & "</div>" ); 

			when Big_Edit_Rendering =>
				Editor( "richTextEditor" );

			when Small_Edit_Rendering =>
				Editor( "smallRichTextEditor" );
		end case;
	end Get_Input;

	function New_Rich_Text_Property_Renderer(
			Plugins		: in String := "";
			Extra_Plugins	: in String := ""
		) return Property_Renderer_Ptr is
	begin
		return new Rich_Text_Property_Renderer_Type'(
					Basic_Property_Renderer_Type with
						Plugins		=> To_Unbounded_String( Plugins ),
						Extra_Plugins	=> To_Unbounded_String( Extra_Plugins )
					);
	end New_Rich_Text_Property_Renderer;


	-----------------------------------
	-- Foreing Key Property Renderer --
	-----------------------------------


	overriding
	procedure Get_Input(
				Renderer	: in out Foreign_Key_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
	begin
		if Property not in KOW_Ent.Properties.Foreign_Key_Property_Type'Class then
			raise PROGRAM_ERROR with "expected foreign key property type";
		end if;

		case Style is
			when Big_Rendering | Small_Rendering =>
				declare
					Entity_Tag	: Ada.Tags.Tag	 	:= KOW_Ent.Properties.Foreign_Key_Property_Type'Class( Property ).Related_Entity_Tag;
					Id		: KOW_Ent.Id_Type	:= KOW_Ent.Properties.Foreign_Key_Property_Type'Class( Property ).Getter.all( Entity );
				begin
					Append( Output, KOW_Ent.To_String( KOW_Ent.Load_Entity( Entity_Tag, ID ) ) );
				exception
					when others => Output := To_Unbounded_String( "--" );
				end;
			when Big_Edit_Rendering | Small_Edit_Rendering =>
				declare
					Entity_Tag	: Ada.Tags.Tag		:= KOW_Ent.Properties.Foreign_Key_Property_Type'Class( Property ).Related_Entity_Tag;
					Ids		: KOW_Ent.Id_Array_Type := Query_Entities(
										Renderer	=> Foreign_Key_Property_Renderer_Type'Class( Renderer ),
										Request		=> Request,
										Entity		=> Entity,
										Property	=> Property
									);
					Id		: KOW_Ent.Id_Type	:= KOW_Ent.Properties.Foreign_Key_Property_Type'Class( Property ).Getter.all( Entity );
				begin
					KOW_View.Modules.Include_Dojo_Package( Module, "dijit.form.FilteringSelect" );
					Append( Output, "<select dojoType=""dijit.form.FilteringSelect"" name=""" );
					Append( Output, Property.Column_Name );
					Append( Output, """" );
					if Property.Immutable and then not KOW_Ent.Is_New( Entity ) then
						Append( Output, " disabled" );
					end if;
					Append( Output, ">" );

					for i in Ids'range loop
						declare
							use KOW_Ent;
							Ent : KOW_Ent.Entity_Type'Class := KOW_Ent.Load_Entity( Entity_Tag, Ids( i ) );
						begin
							Append( Output, "<option value=""" & KOW_Ent.To_String( Ent.ID ) & """" );
								if Ent.id = Id then
									Append( Output, " selected" );
								end if;
							Append( Output, ">" );
							Append( Output, KOW_Ent.To_String( Ent ) );
							Append( Output, "</option>" );
						end;
					end loop;

					Append( Output, "</select>" );
				end;
		end case;
	end Get_Input;
	

	function Query_Entities(
				Renderer	: in Foreign_Key_Property_Renderer_Type;
				Request		: in AWS.Status.Data;
				Entity		: in KOW_Ent.Entity_Type'Class;
				Property	: in KOW_Ent.Entity_Property_Type'Class
			) return KOW_Ent.Id_Array_Type is
		use KOW_Ent.ID_Query_Builders;

		Q : Query_Type;
	begin

		Prepare(
				Q		=> Q,
				Entity_Tag	=> KOW_Ent.Properties.Foreign_Key_Property_Type'Class( Property ).Related_Entity_Tag
			);

		return Get_All( Q );
	end Query_Entities;


	-----------------------------------
	-- File Upload Property Renderer --
	-----------------------------------

	overriding
	procedure Get_Input(
				Renderer	: in out File_Upload_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
		function File_Input return String is
		begin
			if not KOW_Ent.Is_New( Entity ) then
				-- we don't let the user replace existing image 
				return "";
			end if;

			case KOW_View.Entities.Modules.Entity_Module_Type'Class( Module ).Style is
				when Small_Rendering | Big_Rendering =>
					return "";
				when Small_Edit_Rendering | Big_Edit_Rendering =>
					return "<span><input type=""file"" name=""" & To_String( Property.Column_Name ) & """/></span>";
			end case;
		end File_Input;
	begin
		Output := To_Unbounded_String( File_Input );
	end Get_Input;



	----------------------------
	-- Date Property Renderer --
	----------------------------

	overriding
	procedure Get_Input(
				Renderer	: in out Timestamp_Property_Renderer_Type;
				Module		: in out KOW_View.Modules.Module_Type'Class;
				Request		: in     AWS.Status.Data;
				Entity		: in     KOW_Ent.Entity_Type'Class;
				Property	: in     KOW_Ent.Entity_Property_Type'Class;
				Style		: in     Rendering_Style_Type;
				Output		:    out Unbounded_String
			) is
		use Ada.Calendar.Time_Zones;

		Time_Zone	: Time_Offset := KOW_View.Time_Zones.Get_Time_Zone( Request );


		procedure Ap( Str : in String ) is
		begin
			Append( Output, Str );
		end Ap;

		procedure Formatted_Date is
			use Ada.Calendar;
			use Ada.Calendar.Formatting;


			function Date return Time is
			begin
				if Property not in KOW_Ent.Extra_Properties.Timestamp_Properties.Property_Type'Class then
					raise CONSTRAINT_ERROR with "using timestamp renderer in a not-allowed place...";
				end if;
				return KOW_Ent.Extra_Properties.Timestamp_Properties.Property_Type'Class( Property ).Getter.all( Entity );
			end Date;

			Locale		: KOW_Lib.Locales.Locale := KOW_View.Locales.Get_Locale( Request );
		begin
			Ap(
				KOW_Lib.Calendar.Format( 
						L		=> Locale,
						F		=> KOW_Lib.Calendar.Get_Formatter( KOW_Lib.Locales.Get_Long_Date_Pattern( Locale ) ),
						Date		=> Date,
						Time_Zone	=> Time_Zone
					)
				);
		end Formatted_Date;



		procedure Date_Time_Text_Box is
			The_ID : Unbounded_String;


			function Date_Id return String is
			begin
				return To_String( The_ID ) & "__date";
			end Date_Id;

			function Time_Id return String is
			begin
				return To_String( The_Id ) & "__time";
			end Time_Id;
		begin
			KOW_View.Modules.Generate_HTML_Id( Module, The_ID );

			KOW_View.Modules.Include_Component_Script(
							Module		=> Module,
							Component	=> KOW_View.Entities.Components.Component,
							Script		=> "kowview-entities-timestamp.js"
						);
			KOW_View.Modules.Include_Dojo_Package( Module, "dijit.form.DateTextBox" );
			KOW_View.Modules.Include_Dojo_Package( Module, "dijit.form.TimeTextBox" );


			Ap( "<input type=""hidden"" name=""" & To_String( Property.Column_Name ) & """ value=""" & KOW_Ent.Get_Property( Property, Entity ) & '"' );
				Ap( " id=""" & To_String( The_Id ) & """ dateId=""" & Date_id & """ timeId=""" & Time_Id & """/>" );

			Ap( "<input type=""text"" dojoType=""dijit.form.DateTextBox"" id=""" & Date_Id & """/>" );
			Ap( "<input type=""text"" dojoType=""dijit.form.TimeTextBox"" id=""" & Time_Id & """/>" );

			Ap( "<script type=""text/javascript"">dojo.addOnLoad(function() {" );
				Ap( "kowview.entities.timestamp.init('" & To_String( The_Id ) & "', " & Time_Offset'Image( Time_Zone ) & ");" );
			Ap( "</script>" );

		end Date_Time_Text_Box;
	begin
		case Style is
			when Small_Rendering | Big_Rendering =>
				-- render the good stuff
				Formatted_Date;
			when Small_Edit_Rendering | Big_Edit_Rendering =>
				 Date_Time_Text_Box;
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
					New_Rich_Text_Property_Renderer
				);


	Default_Renderers_Registry.Set(
					KOW_Ent.Properties.Foreign_Key_Property_Type'Tag,
					new Foreign_Key_Property_Renderer_Type
				);
	

	Default_Renderers_Registry.Set(
					KOW_View.Entities.Properties.File_Upload_Property_Type'Tag,
					new File_Upload_Property_Renderer_Type
				);

	Default_Renderers_Registry.Set(
					KOW_Ent.Extra_Properties.Timestamp_Properties.Property_Type'Tag,
					new Timestamp_Property_Renderer_Type
				);

end KOW_View.Entities.Property_Renderers;
