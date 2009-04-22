





package body Aw_Enty.Entity_Forms is



	function Get_Label(
			Module		: in Entity_Form_Module;
			Property	: in Aw_Ent.Entity_Property_Type'Class
		) return String is
	begin
		-- TODO: implement a proper way for handling labels:
		return To_String( Property.Column_Name );
	end Get_Label;

	function Get_Input(
			Module		: in Entity_Form_Module;
			Property	: in Aw_Ent.Entity_Property_Type'Class
		) return String is
	begin
	end Get_Input;

	overriding
	procedure Process_Request(
			Module		: in out Entity_Form_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is


		Properties : Aw_Ent.Property_Lists.List;


		Entity_Table : Templates_Parser.Table;



		procedure Append_Property( P: in Aw_Ent.Entity_Property_Type'Class ) is
			Table		: Templates_Parser.Table_Set;
			Label_Set	: Templates_Parser.Translate_Set;
			Input_Set	: Templates_Parser.Translate_Set;
		begin
			Label_Set := Label_Set & Get_Label( Module, P );
			Input_Set := Input_Set & Get_Input( Module, P );
		end Append_Property;

		procedure Iterator( C : Aw_Ent.Property_Lists.Cursor ) is
		begin
			Append_Property(
					Aw_Ent.Property_Lists.Element( C )
				);
		end Iterator;

	begin

		if not Module.Tag in Aw_Ent.Entity_Type'Class then
			raise Program_Error "lol";
		end if;

		Properties := Aw_Ent.Entity_Registry.Get_Properties( Module.Tag );

		Aw_Ent.Property_Lists.Iterate( Properties, Iterator'Access );

		Response := Response &
			To_Unbounded_String(
				Templates_Parser.Parse(
						To_String( Module.Form_Template_File_name ),
						Properties
					)
				);
	end Process_Request;

end Aw_Ent.Entity_Forms;
